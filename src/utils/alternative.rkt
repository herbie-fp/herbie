#lang racket

(require "../syntax/platform.rkt")
(provide (struct-out alt)
         make-alt
         *start-prog*
         alt-cost
         alt-map)

;; Alts are an expression plus a derivation for it.

(struct alt (expr event prevs preprocessing) #:prefab)

(define (make-alt expr)
  (alt expr 'start '() '()))

(define (alt-cost altn repr)
  (define expr-cost (platform-cost-proc (*active-platform*)))
  (expr-cost (alt-expr altn) repr))

(define (alt-map f altn)
  (f (struct-copy alt altn [prevs (map (curry alt-map f) (alt-prevs altn))])))

;; A useful parameter for regimes/bsearch
;; TODO: should be the spec, not initial program, and ideally not a global

(define *start-prog* (make-parameter #f))
