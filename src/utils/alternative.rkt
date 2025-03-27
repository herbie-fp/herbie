#lang typed/racket

(require "../syntax/platform.rkt")

(define-type Program Any)
(define-type Event Any)
(define-type Preprocessing (Listof Symbol))

(define-type Platform Any)
(define-type representation Any)

(require/typed "../syntax/platform.rkt"
  [*active-platform* (-> Platform)]
  [platform-cost-proc (-> Platform (-> Program representation Real))])

(provide (struct-out alt)
         make-alt
         alt?
         alt-expr
         alt-add-event
         *start-prog*
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

(struct alt
  ([expr : Program]
   [event : Event]
   [prevs : (Listof alt)]
   [preprocessing : (Listof Preprocessing)])
  #:prefab)

(: make-alt (-> Program alt))
(define (make-alt expr)
  (alt expr 'start '() '()))

(: make-alt-preprocessing (-> Program (Listof Preprocessing) alt))
(define (make-alt-preprocessing expr preprocessing)
  (alt expr 'start '() preprocessing))

(: alt-equal? (-> alt alt Boolean))
(define (alt-equal? x y)
  (equal? (alt-expr x) (alt-expr y)))

(: alt-add-event (-> alt Event alt))
(define (alt-add-event altn event)
  (alt (alt-expr altn) event (list altn) (alt-preprocessing altn)))

(: alt-add-preprocessing (-> alt (Listof Preprocessing) alt))
(define (alt-add-preprocessing altn preprocessing)
  (alt (alt-expr altn) 'add-preprocessing (list altn) preprocessing))

(: alt-cost (-> alt representation Real))
(define (alt-cost altn repr)
  (define expr-cost (platform-cost-proc (*active-platform*)))
  (expr-cost (alt-expr altn) repr))

(: alt-map (-> (-> alt alt) alt alt))
(define (alt-map f altn)
  (f (struct-copy alt altn [prevs (map (curry alt-map f) (alt-prevs altn))])))

;; A useful parameter for many of Herbie's subsystems, though
;; ultimately one that should be located somewhere else or perhaps
;; exorcised

(define *start-prog* (make-parameter #f))
(define *all-alts* (make-parameter '()))
