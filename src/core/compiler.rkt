#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/platform.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../utils/common.rkt"
         "../utils/dvector.rkt"
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
     (define argv (list->vector args))
     (λ ()
       (apply op
              (for/list ([arg (in-vector argv)])
                (vector-ref regs arg))))]))

;; This function:
;;   1) copies only nodes associated with provided brfs - so, gets rid of useless nodes
;;   2) rewrites these nodes as fl-instructions
(define (batch-for-compiler batch brfs vars args vregs)
  (define out (make-dvector (batch-length batch)))
  (define (compile-node brf recurse)
    (match (deref brf)
      [(approx _ impl) (recurse impl)] ;; do not push, it is already compiled
      [(? symbol? n)
       (define idx (index-of vars n))
       (dvector-add! out (make-thunk (λ () (vector-ref args idx)) '() vregs))]
      [(literal value (app get-representation repr))
       (dvector-add! out (make-thunk (const (real->repr value repr)) '() vregs))]
      [(list op args ...)
       (dvector-add! out (make-thunk (impl-info op 'fl) (map recurse args) vregs))]))
  (define roots (map (batch-recurse batch compile-node) brfs))
  (values (dvector->vector out) roots))

(define (batch-tree-size batch brfs)
  (define (tree-size brf recurse)
    (define args (reap [sow] (expr-recurse-impl (deref brf) sow)))
    (apply + 1 (map recurse args)))
  (apply + (map (batch-recurse batch tree-size) brfs)))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
;; Translates a Herbie IR into a vector of thunks.
(define (compile-progs exprs ctx)
  (define-values (batch brfs) (progs->batch exprs #:ctx ctx))
  (compile-batch batch brfs ctx))

(define (compile-batch batch brfs ctx)
  (define vars (batch-vars batch))
  (define args (make-vector (length vars)))
  (define vregs (make-vector (batch-length batch)))
  (define-values (thunks roots) (batch-for-compiler batch brfs vars args vregs))
  (define rootvec (list->vector roots))
  (timeline-push! 'compiler (batch-tree-size batch brfs) (vector-length thunks))
  (make-progs-interpreter thunks rootvec args vregs))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (compiled-prog . xs)
    (vector-ref (apply core xs) 0))
  compiled-prog)
