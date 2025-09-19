#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/platform.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "batch.rkt"
         "dvector.rkt"
         "programs.rkt")

(provide compile-progs
         compile-batch
         compile-prog)

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
(define (make-progs-interpreter vars ivec rootvec args)
  (define rootlen (vector-length rootvec))
  (define vregs (make-vector (vector-length ivec)))
  (define (compiled-prog args*)
    (vector-copy! args 0 args*)
    (for ([instr (in-vector ivec)]
          [n (in-naturals)])
      (vector-set! vregs n (apply-instruction instr vregs)))
    (for/vector #:length rootlen
                ([root (in-vector rootvec)])
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
    [(list op a) (op (vector-ref regs a))]
    [(list op a b) (op (vector-ref regs a) (vector-ref regs b))]
    [(list op a b c) (op (vector-ref regs a) (vector-ref regs b) (vector-ref regs c))]
    [(list op args ...) (apply op (map (curry vector-ref regs) args))]))

; This functions needs to preserve vars
(define (batch-for-compiler batch brfs vars args)
  (batch-apply
   batch
   brfs
   (λ (node)
     (match node
       [(? symbol?)
        (define idx (index-of vars node))
        (list (λ () (vector-ref args idx)))]
       [(approx _ impl) impl]
       [(literal value (app get-representation repr)) (list (const (real->repr value repr)))]
       [(list op args ...) (cons (impl-info op 'fl) args)]))))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
;; Translates a Herbie IR into an interpretable IR.
;; Requires some hooks to complete the translation.
(define (compile-progs exprs ctx)
  (define vars (context-vars ctx))
  (define-values (batch brfs) (progs->batch exprs #:vars vars))
  (compile-batch batch brfs ctx))

(define (compile-batch batch brfs ctx)
  (define vars (context-vars ctx))
  (define num-vars (length vars))

  ; Here we need to keep vars even though no roots refer to the vars
  (define args (make-vector num-vars))
  (define-values (batch* brfs*) (batch-for-compiler batch brfs vars args))
  (timeline-push! 'compiler (batch-tree-size batch* brfs*) (batch-length batch*))

  (define instructions (dvector->vector (batch-nodes batch*)))
  (define rootvec (list->vector (map batchref-idx brfs*)))

  (make-progs-interpreter vars instructions rootvec args))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (compiled-prog . xs)
    (vector-ref (apply core xs) 0))
  compiled-prog)
