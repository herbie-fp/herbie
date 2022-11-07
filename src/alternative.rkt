#lang racket

(require "cost.rkt")
(provide (struct-out change) (struct-out alt) make-alt alt?
         alt-program alt-add-event *start-prog* *all-alts*
         alt-cost alt-equal? alt-map unsound-expr?)

;; Alts are a lightweight audit trail.
;; An alt records a low-level view of how Herbie got
;; from one program to another.
;; They are a labeled linked list of changes.

(struct change (rule location bindings) #:transparent)

(define (unsound-expr? expr)
  (cond
    [(list? expr)
     (or (and
          (equal? (length expr) 2)
          (or (equal? (first expr) `sqrt.f64)
              (equal? (first expr) `sqrt))
          (and (number? (second expr)) (negative? (second expr))))
         (for/or ([child expr])
           (unsound-expr? child)))]
    [else #f]))


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
    [(alt prog (list 'change cng) `(,prev))
     (alt prog (list 'change cng) `(,(alt-map f prev)))])))

;; A useful parameter for many of Herbie's subsystems, though
;; ultimately one that should be located somewhere else or perhaps
;; exorcised

(define *start-prog* (make-parameter '()))
(define *all-alts* (make-parameter '()))
