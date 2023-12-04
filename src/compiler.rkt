#lang racket

(require math/bigfloat "rival.rkt")
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-specs compile-spec compile-progs compile-prog)

;; Function calculates a precision for the operation
;;    with respect to the given extra-precision (exponents from a previous run)
(define (define-precision extra-precision)
  (min (*max-mpfr-prec*)
       (+ extra-precision
          (*extra-bits*)
          (bf-precision))))

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; <op-procedure> ::= | #(<operation> <extra-precision> <exponents-checkpoint>) - tuned op
;;                    | #(<operation> <extra-precision>) - regular op
;;                    | <operation> - const
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
;; name ::= 'fl or 'ival
(define (make-progs-interpreter name vars ivec roots)
  (define vreg-count (+ (length vars) (vector-length ivec)))
  (define vregs (make-vector vreg-count))

  (define (unbox-prec x)
    (if (box? x)
        (unbox x)
        0))

  (define list-of-trig (list ival-sin ival-cos ival-tan))
  (define (tuning-filter instr)
    (if (vector? (car instr))
        (if (member
             (vector-ref (car instr) 0)
             list-of-trig)
            #t
            #f)
        #f))
  
  (define tuning-ivec
    (if (equal? name 'ival)
        (vector-filter tuning-filter ivec)
        '()))
  
  (if (equal? name 'ival)
    (λ args
      ;; remove all the exponent values we assigned previously when a new point comes
      (when (equal? (bf-precision) (*starting-prec*))
        (for ([instr (in-vector tuning-ivec)])
          (set-box! (vector-ref (car instr) 2) 0)))
    
      (for ([arg (in-list args)] [n (in-naturals)])
        (vector-set! vregs n arg))
      (for ([instr (in-vector ivec)] [n (in-naturals (length vars))])
        ; Tail
        (define srcs
          (for/list ([idx (in-list (cdr instr))])
            (vector-ref vregs idx)))

        ; Current op
        (if (vector? (car instr))
            ; Current op is an operation - not a constant
            (let ([op (vector-ref (car instr) 0)])
              (cond
                [(member op list-of-trig)
                 (let ([extra-precision (unbox-prec (vector-ref (car instr) 1))]
                       [exponents-checkpoint (vector-ref (car instr) 2)])
                 
                   (if (zero? extra-precision)
                       ; Current trig function should be computed under (bf-precision)
                       (vector-set! vregs n (apply op srcs))
                 
                       ; The current op possibly has an extra-precision it should be computed under
                       (parameterize ([bf-precision (define-precision extra-precision)])
                         (vector-set! vregs n (apply op srcs))))
                   
                   (set-box! exponents-checkpoint ; Save exponents with the passed precision for the next run
                             (max 0
                                  (+ extra-precision
                                     (max (+ (bigfloat-exponent (ival-lo (car srcs))) (bigfloat-precision (ival-lo (car srcs))))
                                          (+ (bigfloat-exponent (ival-hi (car srcs))) (bigfloat-precision (ival-hi (car srcs)))))))))]
                [else
                 (let ([extra-precision (unbox-prec (vector-ref (car instr) 1))])
                   (if (zero? extra-precision)
                       ; The current op should be computed under (bf-precision)
                       (vector-set! vregs n (apply op srcs))
                     
                       ; The current op possibly has an extra-precision it should be computed under
                       (parameterize ([bf-precision (define-precision extra-precision)])
                         (vector-set! vregs n (apply op srcs)))))]))
            ; This is a constant operation
            (vector-set! vregs n (apply (car instr) srcs))))
    
      (for/list ([root (in-list roots)])
        (vector-ref vregs root)))
    
    ; name == 'fl
    (λ args
      (for ([arg (in-list args)] [n (in-naturals)])
        (vector-set! vregs n arg))
      (for ([instr (in-vector ivec)] [n (in-naturals (length vars))])
        ; Tail
        (define srcs
          (for/list ([idx (in-list (cdr instr))])
            (vector-ref vregs idx)))
        (if (vector? (car instr))
            (vector-set! vregs n (apply (vector-ref (car instr) 0) srcs))
            (vector-set! vregs n (apply (car instr) srcs))))
    
      (for/list ([root (in-list roots)])
        (vector-ref vregs root))))
  )

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
    (define (munge prog type [prec 0])
      (set! size (+ 1 size))
      
      (define expr
        (match prog
          [(? number?) (list (const (input->value prog type)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list (vector if-proc prec)
                 (munge c cond-type prec)
                 (munge t type prec)
                 (munge f type prec))]
          [(list op args ...)
           #:when (and (set-member? '(sin cos tan) op)
                       (equal? name 'ival))
           (let ([exponent (box 0)])
             (cons (vector (op->proc op) prec exponent)
                   (map (curryr munge exponent)
                        args
                        (op->itypes op))))]
          [(list op args ...)
           (cons (vector (op->proc op) prec)
                    (map (curryr munge prec)
                         args
                         (op->itypes op)))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                     (set! exprc (+ 1 exprc))
                     (set! icache (cons expr icache))))))

    (define names (map (curryr munge type) exprs))
    (timeline-push! 'compiler (+ varc size) (+ exprc varc))
    (define ivec (list->vector (reverse icache)))
    (define interpret (make-progs-interpreter name vars ivec names))
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
