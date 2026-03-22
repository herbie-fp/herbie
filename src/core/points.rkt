#lang racket

(require math/flonum
         "../syntax/float.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt"
         "compiler.rkt")

(provide in-pcontext
         mk-pcontext
         for/pcontext
         pcontext?
         pcontext-points
         split-pcontext
         pcontext-length
         errors
         batchref-errors
         batch-errors
         exprs-errors
         errors-score)

;; pcontexts are Herbie's standard data structure for storing
;; ground-truth information. They contain 1) a set of sampled input
;; points; and 2) a ground-truth output for each input.

(struct pcontext (points exacts) #:prefab)

(define (in-pcontext pcontext)
  (in-parallel (in-vector (pcontext-points pcontext)) (in-vector (pcontext-exacts pcontext))))

(define (pcontext-length pcontext)
  (vector-length (pcontext-points pcontext)))

(define/contract (mk-pcontext points exacts)
  (-> (non-empty-listof vector?) (non-empty-listof any/c) pcontext?)
  (pcontext (list->vector points) (list->vector exacts)))

(define-syntax-rule (for/pcontext ([(pt ex) pcontext] other ...) body ...)
  (let-values ([(pts* exs*)
                (for/lists (pts* exs*) ([(pt ex) (in-pcontext pcontext)] other ...) body ...)])
    (mk-pcontext pts* exs*)))

(define (split-pcontext pctx num-a num-b)
  (match-define (pcontext pts exs) pctx)
  (unless (= (+ num-a num-b) (vector-length pts))
    (error 'split-pcontext
           "Cannot split pcontext of size ~a into ~a and ~a"
           (vector-length pts)
           num-a
           num-b))
  (define-values (pts-a pts-b) (vector-split-at pts num-a))
  (define-values (exs-a exs-b) (vector-split-at exs num-a))
  (values (pcontext pts-a exs-a) (pcontext pts-b exs-b)))

;; Herbie's standard error measure is the average bits of error across
;; all points in a pcontext.

(define (average . s)
  (/ (apply + s) (length s)))

(define (errors-score e)
  (/ (flvector-sum e) (flvector-length e)))

(define (errors expr pcontext ctx)
  (first (exprs-errors (list expr) pcontext ctx)))

(define (batchref-errors brf pcontext ctx)
  (first (batch-errors (batchref-batch brf) (list brf) pcontext ctx)))

(define (exprs-errors exprs pcontext ctx)
  (define fn (compile-progs exprs ctx))
  (define num-exprs (length exprs))
  (generate-errors fn pcontext ctx num-exprs))

(define (batch-errors batch brfs pcontext ctx)
  (define fn (compile-batch batch brfs ctx))
  (define num-exprs (length brfs))
  (generate-errors fn pcontext ctx num-exprs))

(define (generate-errors fn pcontext ctx num-exprs)
  (define repr (context-repr ctx))
  (define ulps (repr-ulps repr))
  (define max-ulps (+ 1 (expt 2 (representation-total-bits repr))))
  (define invalid-bits (real->double-flonum (representation-total-bits repr)))
  (define num-points (pcontext-length pcontext))
  (define results (build-vector num-exprs (lambda (_) (make-flvector num-points invalid-bits))))
  (for ([point (in-vector (pcontext-points pcontext))]
        [exact (in-vector (pcontext-exacts pcontext))]
        [pidx (in-naturals)])
    (define outs (fn point))
    (for ([out (in-vector outs)]
          [result (in-vector results)])
      (define err-ulps (ulps out exact))
      (flvector-set! result
                     pidx
                     (if (= err-ulps max-ulps)
                         invalid-bits
                         (ulps->bits err-ulps)))))
  (vector->list results))
