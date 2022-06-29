#lang racket

(require "cost.rkt")
(provide (struct-out change) (struct-out alt) make-alt alt?
         alt-program alt-add-event *start-prog* *all-alts*
         alt-cost alt-equal?)

;; Alts are a lightweight audit trail.
;; An alt records a low-level view of how Herbie got
;; from one program to another.
;; They are a labeled linked list of changes.

(struct change (rule location bindings) #:transparent)

(struct alt (program event prevs)
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (fprintf port "#<alt ~a>" (alt-program alt)))])

(define (make-alt prog)
  (alt prog 'start '()))

(define (alt-add-event altn event)
  (alt (alt-program altn) event (list altn)))

(define (alt-cost altn repr)
  (program-cost (alt-program altn) repr))

(define (alt-equal? x y)
  (equal? (alt-program x) (alt-program y)))

;; A useful parameter for many of Herbie's subsystems, though
;; ultimately one that should be located somewhere else or perhaps
;; exorcised

(define *start-prog* (make-parameter '()))
(define *all-alts* (make-parameter '()))
