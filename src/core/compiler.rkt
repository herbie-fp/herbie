#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/platform.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/batch.rkt")

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
(define (make-progs-interpreter ivec rootvec args)
  (define rootlen (vector-length rootvec))
  (define vregs (make-vector (vector-length ivec)))
  (define (compiled-prog args*)
    (define args**
      (if (vector? args*)
          args*
          (vector args*)))
    (vector-copy! args 0 args**)
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

;; This function:
;;   1) copies only nodes associated with provided brfs - so, gets rid of useless nodes
;;   2) rewrites these nodes as fl-instructions
(define (batch-for-compiler batch brfs vars args)
  (define out (batch-empty))
  (define f
    (batch-recurse
     batch
     (λ (brf recurse)
       (match (deref brf)
         [(approx _ impl) (recurse impl)] ;; do not push, it is already a batchref
         [(? symbol? n)
          (define idx (index-of vars n))
          (batch-push! out (list (λ () (vector-ref args idx))))]
         [(literal value (app get-representation repr))
          (define repr*
            (if (not (vector? value))
                (match (representation-type repr)
                  ['array (array-representation-base repr)]
                  [`(array ,_ ,_) (array-representation-base repr)]
                  [_ repr])
                repr))
          (batch-push! out (list (const (real->repr value repr*))))]
         [(list op args ...)
          (batch-push! out (cons (impl-info op 'fl) (map (compose batchref-idx recurse) args)))]))))
  (values out (map f brfs)))

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
  (define args (make-vector (length vars)))
  (define-values (batch* brfs*) (batch-for-compiler batch brfs vars args))
  (define instructions (batch-get-nodes batch*))
  (define rootvec (list->vector (map batchref-idx brfs*)))

  (timeline-push! 'compiler (batch-tree-size batch* brfs*) (batch-length batch*))

  (make-progs-interpreter instructions rootvec args))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (compiled-prog . xs)
    (vector-ref (apply core xs) 0))
  compiled-prog)
