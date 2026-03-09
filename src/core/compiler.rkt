#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/platform.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/batch.rkt")

(provide compile-progs
         compile-batch
         compile-prog)

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<thunk> ..+)
;; ```
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
(define (make-progs-interpreter tvec rootvec args vregs)
  (define rootlen (vector-length rootvec))
  (define vregs-len (vector-length tvec))
  (define (compiled-prog args*)
    (vector-copy! args 0 args*)
    (for ([thunk (in-vector tvec)]
          [n (in-range vregs-len)])
      (vector-set! vregs n (thunk)))
    (for/vector #:length rootlen
                ([root (in-vector rootvec)])
      (vector-ref vregs root)))
  compiled-prog)

(define (make-thunk op argidxs regs)
  (match argidxs
    ['() op]
    [(list a) (λ () (op (vector-ref regs a)))]
    [(list a b) (λ () (op (vector-ref regs a) (vector-ref regs b)))]
    [(list a b c) (λ () (op (vector-ref regs a) (vector-ref regs b) (vector-ref regs c)))]
    [(list args ...)
     (define argc (length args))
     (define argv (list->vector args))
     (λ ()
       (apply op
              (for/list ([arg (in-vector argv)])
                (vector-ref regs arg))))]))

;; This function:
;;   1) copies only nodes associated with provided brfs - so, gets rid of useless nodes
;;   2) rewrites these nodes as fl-instructions
(define (batch-for-compiler batch brfs vars args vregs)
  (define out (batch-empty))
  (define f
    (batch-recurse batch
                   (λ (brf recurse)
                     (match (deref brf)
                       [(approx _ impl) (recurse impl)] ;; do not push, it is already a batchref
                       [(? symbol? n)
                        (define idx (index-of vars n))
                        (batch-push! out (make-thunk (λ () (vector-ref args idx)) '() vregs))]
                       [(literal value (app get-representation repr))
                        (batch-push! out (make-thunk (const (real->repr value repr)) '() vregs))]
                       [(list op args ...)
                        (batch-push! out
                                     (make-thunk (impl-info op 'fl)
                                                 (map (compose batchref-idx recurse) args)
                                                 vregs))]))))
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
  (define vregs (make-vector (batch-length batch)))
  (define-values (batch* brfs*) (batch-for-compiler batch brfs vars args vregs))
  (define thunks (batch-get-nodes batch*))
  (define rootvec (list->vector (map batchref-idx brfs*)))

  (timeline-push! 'compiler (batch-tree-size batch* brfs*) (batch-length batch*))

  (make-progs-interpreter thunks rootvec args vregs))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (compiled-prog . xs)
    (vector-ref (apply core xs) 0))
  compiled-prog)
