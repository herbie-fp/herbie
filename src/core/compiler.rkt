#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/platform.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "batch.rkt")

(provide compile-progs
         compile-batch
         compile-prog)

;; Drops spec in-place
(define (drop-spec! batch)
  (batch-apply! batch
                (lambda (node)
                  (match node
                    [(approx spec impl) impl]
                    [node node]))))

;; Allocates a new batch with executable instructions
(define (rewrite batch brfs)
  (batch-apply
   batch
   brfs
   (lambda (node)
     (match node
       [(literal value (app get-representation repr)) (list (const (real->repr value repr)))]
       [(list op args ...) (cons (impl-info op 'fl) args)]
       [_ node]))))

(define (evaluate batch vars)
  (batch-map-iterative
   batch
   (lambda (evaluate-child node pt)
     (match node
       [(list op) (op)]
       [(list op a) (op (evaluate-child a))]
       [(list op a b) (op (evaluate-child a) (evaluate-child b))]
       [(list op a b c) (op (evaluate-child a) (evaluate-child b) (evaluate-child c))]
       [(list op args ...) (apply op (map evaluate-child args))]
       [(? symbol?) (vector-ref pt (index-of vars node))]))))

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
  ;; Modifying batch
  (define-values (batch* brfs*) (rewrite batch (map (drop-spec! batch) brfs)))
  (define evaluator (evaluate batch* vars))

  (define (fn pt)
    (list->vector (map (curryr evaluator pt) brfs*)))
  fn)

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (compiled-prog . xs)
    (vector-ref (apply core xs) 0))
  compiled-prog)
