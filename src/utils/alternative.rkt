#lang racket

(require "../syntax/platform.rkt")
(provide (struct-out alt)
         make-alt
         alt?
         alt-expr
         alt-add-event
         *all-alts*
         alt-cost
         alt-equal?
         alt-map
         alt-add-preprocessing
         make-alt-preprocessing)

;; Alts are a lightweight audit trail.
;; An alt records a low-level view of how Herbie got
;; from one program to another.
;; They are a labeled linked list of changes.

(struct alt (expr event prevs preprocessing) #:prefab)

(define (make-alt expr)
  (alt expr 'start '() '()))

(define (make-alt-preprocessing expr preprocessing)
  (alt expr 'start '() preprocessing))

(define (alt-equal? x y)
  (equal? (alt-expr x) (alt-expr y)))

(define (alt-add-event altn event)
  (alt (alt-expr altn) event (list altn) (alt-preprocessing altn)))

(define (alt-add-preprocessing altn preprocessing)
  (alt (alt-expr altn) 'add-preprocessing (list altn) preprocessing))

(define (alt-cost altn repr)
  (define expr-cost (platform-cost-proc (*active-platform*)))
  (expr-cost (alt-expr altn) repr))

(define (alt-map f altn)
  (f (struct-copy alt altn [prevs (map (curry alt-map f) (alt-prevs altn))])))

;; Keeps track of all alts so far.
(define *all-alts* (make-parameter '()))
