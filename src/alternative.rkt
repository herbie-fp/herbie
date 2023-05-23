#lang racket

(require "cost.rkt" "programs.rkt")
(provide (struct-out alt) make-alt alt? alt-expr
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

(define (alt-expr alt)
  (program-body (alt-program alt)))

(define (alt-equal? x y)
  (equal? (alt-program x) (alt-program y)))

(define (alt-add-event altn event)
  (alt (alt-program altn) event (list altn)))

(define (alt-cost altn repr)
  (expr-cost (alt-expr altn) repr))

(define (alt-map f altn)
  (f (struct-copy alt altn [prevs (map (curry alt-map f) prevs)])))

;; A useful parameter for many of Herbie's subsystems, though
;; ultimately one that should be located somewhere else or perhaps
;; exorcised

(define *start-prog* (make-parameter #f))
(define *all-alts* (make-parameter '()))
