#lang racket

(require "../utils/common.rkt"
         "../utils/timeline.rkt"
         "egg-herbie.rkt")

(provide rewrite-expressions)

;;
;;  Egg recursive rewriter
;;

(define (rewrite-expressions exprs reprs schedule ctx)
  (timeline-push! 'method "batch-egg-rewrite")
  (timeline-push! 'inputs (map ~a exprs))

  (define extractor
    (typed-egg-extractor (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc)))

  (define runner (make-egg-runner exprs reprs schedule #:context ctx))
  (define variantss (run-egg runner `(multi . ,extractor)))

  (define out
    (for/list ([variants variantss])
      (for/list ([variant (remove-duplicates variants)])
        (list variant runner))))

  (timeline-push! 'outputs (map ~a (apply append variantss)))
  out)
