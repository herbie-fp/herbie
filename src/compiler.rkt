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
    (define (munge prog type)
      (set! size (+ 1 size))
      (define expr
        (match prog
          [(? number?) (list (const (input->value prog type)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list if-proc (munge c cond-type) (munge t type) (munge f type))]
          [(list op args ...)
           (cons (op->proc op) (map munge args (op->itypes op)))]
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
    (procedure-rename interpret (sym-append 'eval-prog '- name))))

;; Compiles a program of operators into a procedure
;; that evaluates the program on a single input of intervals
;; returning intervals.
(define (compile-specs specs vars)
  (define compile
    (make-compiler 'ival
                   #:input->value (lambda (prog _) (ival (bf prog)))
                   #:op->procedure (lambda (op)
                                    (match op
                                      [(? repr-conv? impl) ; strangeness with specs
                                       (operator-info (impl->operator impl) 'ival)]
                                      [_
                                       (operator-info op 'ival)]))
                   #:op->itypes (lambda (op)
                                  (match op
                                    [(? repr-conv? impl) ; strangeness with specs
                                     (operator-info (impl->operator impl) 'itype)]
                                    [_
                                     (operator-info op 'itype)]))
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
