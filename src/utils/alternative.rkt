#lang racket

(require "../syntax/platform.rkt")
(provide (struct-out alt)
         make-alt
         alt-cost
         alt-map)

;; Alts are an expression plus a derivation for it.

(struct alt (expr event prevs) #:prefab)

(define (make-alt expr)
  (alt expr 'start '()))

(define (alt-cost altn repr)
  (define expr-cost (platform-cost-proc (*active-platform*)))
  (expr-cost (alt-expr altn) repr))

(define (alt-map f altn)
  (f (struct-copy alt altn [prevs (map (curry alt-map f) (alt-prevs altn))])))
