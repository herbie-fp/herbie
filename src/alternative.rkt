#lang racket

(require "cost.rkt")
(provide (struct-out alt) make-alt alt?
         alt-program alt-add-event *start-prog* *all-alts*
         alt-cost alt-equal? alt-map)

;; Alts are a lightweight audit trail.
;; An alt records a low-level view of how Herbie got
;; from one program to another.
;; They are a labeled linked list of changes.

(struct alt (program event prevs)
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (fprintf port "#<alt ~a>" (alt-program alt)))])

(define (make-alt prog)
  (alt prog 'start '()))

(define (alt-equal? x y)
  (equal? (alt-program x) (alt-program y)))

(define (alt-add-event altn event)
  (alt (alt-program altn) event (list altn)))

(define (alt-cost altn repr)
  (program-cost (alt-program altn) repr))

(define (alt-map f altn)
  (f
   (match altn
    [(alt prog 'start (list))
     altn]
    [(alt prog `(start ,strategy) `(,prev))
     (alt prog `(start ,strategy) `(,(alt-map f prev)))]
    [(alt p `(regimes ,splitpoints) prevs)
     (alt p `(regimes ,splitpoints) (map (curry alt-map f) prevs))]
    [(alt prog `(taylor ,pt ,var ,loc) `(,prev))
     (alt prog `(taylor ,pt ,var ,loc) `(,(alt-map f prev)))]
     [(alt prog `(simplify ,loc ,input ,proof ,soundiness) `(,prev))
      (alt prog `(simplify ,loc ,input ,proof ,soundiness) `(,(alt-map f prev)))]
    [(alt prog `initial-simplify `(,prev))
     (alt prog `initial-simplify `(,(alt-map f prev)))]
    [(alt prog `final-simplify `(,prev))
     (alt prog `final-simplify `(,(alt-map f prev)))]
    [(alt prog (list 'rr rules loc input-exprs iter-limit) `(,prev))
     (alt prog (list 'rr rules loc input-exprs iter-limit) `(,(alt-map f prev)))])))

;; A useful parameter for many of Herbie's subsystems, though
;; ultimately one that should be located somewhere else or perhaps
;; exorcised

(define *start-prog* (make-parameter '()))
(define *all-alts* (make-parameter '()))
