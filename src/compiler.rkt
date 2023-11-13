#lang racket

(require math/bigfloat rival)
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt")

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
      (define srcs
        (for/list ([idx (in-list (cdr instr))])
          (vector-ref vregs idx)))
      (vector-set! vregs n (apply (car instr) srcs)))
    (for/list ([root (in-list roots)])
      (vector-ref vregs root))))

(define (make-compiler arg->precision operator-proc operator-itypes
                       if-proc cond-type interpreter-name)
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
    (define (munge prog type)
      (set! size (+ 1 size))
      (define expr
        (match prog
          [(? number?) (list (const (arg->precision prog type)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list if-proc (munge c cond-type) (munge t type) (munge f type))]
          [(list op args ...)
           (cons (operator-proc op) (map munge args (operator-itypes op)))]
          ;; (cons (operator-info op 'ival) (map munge args))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                     (set! exprc (+ 1 exprc))
                     (set! icache (cons expr icache))))))

    (define names (map (curryr munge type) exprs))
    (timeline-push! 'compiler (+ varc size) (+ exprc varc))
    (define ivec (list->vector (reverse icache)))
    (define interpret (make-progs-interpreter vars ivec names))
    (procedure-rename interpret (sym-append 'eval-prog '- interpreter-name))))

(define (compile-spec spec vars)
  (compose first (compile-specs (list spec) vars)))

(define (compile-prog expr ctx)
  (compose first (compile-progs (list expr) ctx)))

;; Compiles a program of operators into a procedure
;; that evaluates the program on a single input of intervals
;; returning intervals.
(define (compile-specs specs vars)
  ((make-compiler
    (lambda (prog _) (ival (bf prog)))
    (curryr operator-info 'ival) (curryr operator-info 'itype)
    ival-if 'bool
    'ival)
   specs vars 'real))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
(define (compile-progs exprs ctx)
  ((make-compiler
    real->repr
    (curryr impl-info 'fl) (curryr impl-info 'itype)
    (λ (c ift iff) (if c ift iff)) (get-representation 'bool)
    'fl)
   exprs (context-vars ctx) (context-repr ctx)))
