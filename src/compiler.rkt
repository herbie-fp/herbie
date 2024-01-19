#lang racket

(require math/bigfloat rival)
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-specs compile-spec compile-progs compile-prog)

;; Function calculates a precision for the operation
;;    with respect to the given extra-precision (exponents from a previous run)
(define (operator-precision extra-precision)
  (min (*max-mpfr-prec*)
       (+ extra-precision
          (*ground-truth-extra-bits*)
          (bf-precision))))

(define (true-exponent x)
  (+ (bigfloat-exponent x) (bigfloat-precision x)))

(define (unbox-prec x)
    (if (box? x)
        (unbox x)
        0))

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; <op-procedure> ::= #(<operation> <extra-precision> <exponents-checkpoint>)
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
;; name ::= 'fl or 'ival
(define (make-progs-interpreter name vars ivec roots)
  (define vreg-count (+ (length vars) (vector-length ivec)))
  (define vregs (make-vector vreg-count))

  (define (tuning-filter instr)
    (if (member
         (vector-ref (car instr) 0)
         (list ival-sin ival-cos ival-tan))
        #t
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
        (define srcs
          (for/list ([idx (in-list (cdr instr))])
            (vector-ref vregs idx)))

        (match-define (vector op extra-precision exponents-checkpoint) (car instr))
        (let ([extra-prec (unbox-prec extra-precision)])
          
          (define precision (operator-precision extra-prec))
          (define timeline-stop! (timeline-start! 'mixsample (~a (object-name op)) precision))
          (define output
            (parameterize ([bf-precision precision])
              (apply op srcs)))
          (vector-set! vregs n output)
          
          (when
               (member op (list ival-sin ival-cos ival-tan))
            (set-box! exponents-checkpoint ; Save exponents with the passed precision for the next run
                      (+ extra-prec
                         (max 0
                              (true-exponent (ival-lo (car srcs)))
                              (true-exponent (ival-hi (car srcs)))))))
          (timeline-stop!)))
    
      (for/list ([root (in-list roots)])
        (vector-ref vregs root)))
    
    ; name == 'fl
    (λ args
      (for ([arg (in-list args)] [n (in-naturals)])
        (vector-set! vregs n arg))
      (for ([instr (in-vector ivec)] [n (in-naturals (length vars))])
        (define srcs
          (for/list ([idx (in-list (cdr instr))])
            (vector-ref vregs idx)))
        (vector-set! vregs n (apply (car instr) srcs)))
      (for/list ([root (in-list roots)])
        (vector-ref vregs root)))))

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
    
    ; Translates programs into an instruction sequence of ival operations
    (define (munge-ival prog type [prec 0])
      (set! size (+ 1 size))

      (define expr
        (match prog
          [(? number?) (list (vector (const (input->value prog type)) (box 0) (box 0)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list (vector if-proc prec prec)
                 (munge-ival c cond-type prec)
                 (munge-ival t type prec)
                 (munge-ival f type prec))]
          [(list (and (or 'sin 'cos 'tan) op) args ...)
           (let ([exponent (box 0)])
             (cons (vector (op->proc op) prec exponent)
                   (map (curryr munge-ival exponent)
                        args
                        (op->itypes op))))]
          [(list op args ...)
           (cons (vector (op->proc op) prec prec)
                    (map (curryr munge-ival prec)
                         args
                         (op->itypes op)))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc)
                     (set! exprc (+ 1 exprc))
                     (set! icache (cons expr icache))))))

    ; Translates programs into an instruction sequence of flonum operations
    (define (munge-fl prog type)
      (set! size (+ 1 size))
      (define expr
        (match prog
          [(? number?) (list (const (input->value prog type)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list if-proc (munge-fl c cond-type) (munge-fl t type) (munge-fl f type))]
          [(list op args ...)
           (cons (op->proc op) (map munge-fl args (op->itypes op)))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                     (set! exprc (+ 1 exprc))
                     (set! icache (cons expr icache))))))

    (define names (map (curryr (if (equal? name 'fl) munge-fl munge-ival) type) exprs))
    (timeline-push! 'compiler (+ varc size) (+ exprc varc))
    (define ivec (list->vector (reverse icache)))
    (define interpret (make-progs-interpreter name vars ivec names))
    (procedure-rename interpret (sym-append 'eval-prog '- name))))

;; Compiles a program of operators into a procedure
;; that evaluates the program on a single input of intervals
;; returning intervals.
(define (compile-specs specs vars)
  ; strangeness with specs: need to check for `repr-conv?` operators
  ; normally we'd call `repr-conv?` from `src/syntax/syntax.rkt`
  ; but it will check the entire table of operators every call,
  ; so greedily compute the set ahead of time
  (define repr-convs (operator-all-impls 'cast))
  (define (real-op op)
    (if (member op repr-convs)
        (impl->operator op)
        op))

  (define compile
    (make-compiler 'ival
                   #:input->value (lambda (prog _) (ival (bf prog)))
                   #:op->procedure (lambda (op) (operator-info (real-op op) 'ival))
                   #:op->itypes (lambda (op) (operator-info (real-op op) 'itype))
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
