#lang racket

(require math/bigfloat "rival.rkt")
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-specs compile-spec compile-progs compile-prog)

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
(define (make-progs-interpreter vars ivec roots)
  (define vreg-count (+ (length vars) (vector-length ivec)))
  (define vregs (make-vector vreg-count))
  (λ args
    (for ([arg (in-list args)] [n (in-naturals)])
      (vector-set! vregs n arg))
    (for ([instr (in-vector ivec)] [n (in-naturals (length vars))])
      
      ; tail
      (define srcs
        (for/list ([idx (in-list (cdr instr))])
          (vector-ref vregs idx)))
      
      ; current op
      (if (list? (car instr))
          ; it is some operation - not a constant
          (if (equal? 2 (length (car instr)))
              ; it is not a trig function
              (if (box? (cdar instr))
                  ; this operation has a precision to be calculated under
                  (parameterize ([bf-precision
                                  (min (*max-mpfr-prec*)
                                       (max
                                        (+ 10 (bf-precision))
                                        (+ (unbox (cdar instr)) (bf-precision) 10)))])
                    (vector-set! vregs n (apply (caar instr) srcs)))
                  ; this operation doesn't have a specific precision
                  (vector-set! vregs n (apply (caar instr) srcs)))
              
              ; It is a trig function because only trig function has 3 arguments
              ; (sin is-inside-trig-flag exponent-value-of-this-operation)
              (if (box? (second (car instr)))
                  ; this trig function has a specific precision it should be computed under
                  ; this trig function is inside another trig function
                  (parameterize ([bf-precision
                                  (min (*max-mpfr-prec*)
                                       (max
                                        (+ 10 (bf-precision))
                                        (+ (unbox (second (car instr))) (bf-precision) 10)))])
                    (let ([result (apply (first (car instr)) srcs)]) ; calculate the result of trig function
                      (set-box! (third (car instr)) ; set the exponent of the input to cos/sin/tan instruction
                                (max (+ (bigfloat-exponent (ival-lo (car srcs))) (bigfloat-precision (ival-lo (car srcs))))
                                     (+ (bigfloat-exponent (ival-hi (car srcs))) (bigfloat-precision (ival-hi (car srcs))))))
                      (vector-set! vregs n result)))

                  ; this trig function doesn't have a specific precision
                  (let ([result (apply (first (car instr)) srcs)]) ; calculate the result of trig function
                    (set-box! (third (car instr)) ; set the exponent of the input to cos/sin/tan instruction
                              (max (+ (bigfloat-exponent (ival-lo (car srcs))) (bigfloat-precision (ival-lo (car srcs))))
                                   (+ (bigfloat-exponent (ival-hi (car srcs))) (bigfloat-precision (ival-hi (car srcs))))))
                    (vector-set! vregs n result))))
            
          ; if it is just a single procedure const
          (vector-set! vregs n (apply (car instr) srcs))))
    
    (for/list ([root (in-list roots)])
      (vector-ref vregs root))))

;; Translates a Herbie IR into an interpretable IR.
;; Requires some hooks to complete the translation.
(define (make-compiler name
                       #:input->value input->value
                       #:op->procedure op->proc
                       #:op->itypes op->itypes
                       #:if-procedure if-proc
                       #:cond-type cond-type)
  (lambda (exprs vars type)
    ;; Instruction cache
    (define icache '())
    (define exprhash
      (make-hash
       (for/list ([var vars] [i (in-naturals)])
         (cons var i))))

    ; Counts
    (define size 0)
    (define exprc 0)
    (define varc (length vars))

    ; Translates programs into an instruction sequence
    (define (munge prog type [prec #f])
      (set! size (+ 1 size))
      ;(printf "prog=~a\n" prog)
      (define expr
        (match prog
          [(? number?) (list (const (input->value prog type)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list (list if-proc prec)
                 (munge c cond-type)
                 (munge t type)
                 (munge f type))]
          [(list op args ...)
           (if (set-member? '(sin cos tan) op)
               (let ([exponent (box #f)])
                 (cons (list (op->proc op) prec exponent) ; we drag the exponent value of input to this op
                       (map munge
                            args
                            (op->itypes op)
                            (make-list (length args) exponent)))) ; and here, so that we can change it quickly using box everywhere
               (cons (list (op->proc op) prec)
                     (map munge
                          args
                          (op->itypes op)
                          (make-list (length args) prec))))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      ;(printf "expr=~a\n\n" expr)
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                     (set! exprc (+ 1 exprc))
                     (set! icache (cons expr icache))))))

    (define names (map (curryr munge type) exprs))
    (timeline-push! 'compiler (+ varc size) (+ exprc varc))
    (define ivec (list->vector (reverse icache)))
    (define interpret (make-progs-interpreter vars ivec names))
    (procedure-rename interpret (sym-append 'eval-prog '- name))))

;; Compiles a program of operators into a procedure
;; that evaluates the program on a single input of intervals
;; returning intervals.
(define (compile-specs specs vars)
  (define compile
    (make-compiler 'ival
                   #:input->value (lambda (prog _) (ival (bf prog)))
                   #:op->procedure (lambda (op) (operator-info op 'ival))
                   #:op->itypes (lambda (op) (operator-info op 'itype))
                   #:if-procedure ival-if
                   #:cond-type 'bool))
  (compile specs vars 'real))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
(define (compile-progs exprs ctx)
  (define compile
    (make-compiler 'fl
                   #:input->value real->repr
                   #:op->procedure (lambda (op) (impl-info op 'fl))
                   #:op->itypes (lambda (op) (impl-info op 'itype))
                   #:if-procedure (λ (c ift iff) (if c ift iff))
                   #:cond-type (get-representation 'bool)))
  (compile exprs (context-vars ctx) (context-repr ctx)))

;; Like `compile-specs`, but for a single spec.
(define (compile-spec spec vars)
  (compose first (compile-specs (list spec) vars)))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (compose first (compile-progs (list expr) ctx)))
