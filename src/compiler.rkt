#lang racket

(require math/bigfloat math/flonum rival)
;; Faster than bigfloat-exponent and avoids an expensive offset & contract check.
(require "syntax/syntax.rkt" "syntax/types.rkt" "correct-round.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-progs compile-prog compile-specs compile-spec)

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
(define (make-progs-interpreter vars ivec rootvec)
  (define rootlen (vector-length rootvec))
  (define iveclen (vector-length ivec))
  (define varc (length vars))
  (define vregs (make-vector (+ varc iveclen)))
  (define (compiled-prog . args)
    (for ([arg (in-list args)] [n (in-naturals)])
      (vector-set! vregs n arg))
    (for ([instr (in-vector ivec)] [n (in-naturals varc)])
      (vector-set! vregs n (apply-instruction instr vregs)))
    (for/vector #:length rootlen ([root (in-vector rootvec)])
      (vector-ref vregs root)))

  compiled-prog)

(define (apply-instruction instr regs)
  ;; By special-casing the 0-3 instruction case,
  ;; we avoid any allocation in the common case.
  ;; We could add more cases if we want wider instructions.
  ;; At some extreme, vector->values plus call-with-values
  ;; becomes the fastest option.
  (match instr
    [(list op) (op)]
    [(list op a)
     (op (vector-ref regs a))]
    [(list op a b)
     (op (vector-ref regs a)
         (vector-ref regs b))]
    [(list op a b c)
     (op (vector-ref regs a)
         (vector-ref regs b)
         (vector-ref regs c))]
    [(list op args ...)
     (apply op (map (lambda (arg) (vector-ref regs arg)) args))]))

(define (if-proc c a b)
  (if c a b))

(define (progs->batch exprs vars)
  (define icache (reverse vars))
  (define exprhash
    (make-hash
     (for/list ([var vars] [i (in-naturals)])
       (cons var i))))
  ; Counts
  (define size 0)
  (define exprc 0)
  (define varc (length vars))

  ; Translates programs into an instruction sequence of operations
  (define (munge prog)
    (set! size (+ 1 size))
    (define node ; This compiles to the register machine
      (match prog
        [(list op args ...)
         (cons op (map munge args))]
        [_
         prog]))
    (hash-ref! exprhash node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  (define roots (list->vector (map munge exprs)))
  (define nodes (list->vector (reverse icache)))

  (timeline-push! 'compiler (+ varc size) (+ exprc varc))
  (values nodes roots))

;; Translates a Herbie IR into an interpretable IR.
;; Requires some hooks to complete the translation.
(define (make-compiler exprs vars)
  (define num-vars (length vars))
  (define-values (nodes roots)
    (progs->batch exprs vars))

  (define instructions
    (for/vector #:length (- (vector-length nodes) num-vars)
                ([node (in-vector nodes num-vars)])
      (match node
        [(literal value (app get-representation repr))
         (list (const (real->repr value repr)))]
        [(list 'if c t f)
         (list if-proc c t f)]
        [(list op args ...)
         (cons (impl-info op 'fl) args)])))

  (make-progs-interpreter vars instructions roots))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
(define (compile-progs exprs ctx)
  (make-compiler exprs (context-vars ctx)))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (compiled-prog . xs) (vector-ref (apply core xs) 0))
  compiled-prog)
