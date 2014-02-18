#lang racket

(require math/bigfloat)
(require data/order)

(provide reap println ->flonum *precision* cotan square ordinary-float?
         list= list< enumerate take-up-to *debug*)

; Precision for approximate evaluation
(define *precision* (make-parameter real->double-flonum))

(define *debug* (make-parameter #f))

(define-syntax (reap stx)
  "A reap/sow abstraction for filters and maps."
  (syntax-case stx ()
    [(_ [sow] body ...)
     #'(let* ([store '()]
              [sow (λ (elt) (set! store (cons elt store)) elt)])
         body ...
         (reverse store))]))

(define (println . args)
  (for ([val args])
    (if (string? val)
        (display val)
        (print val)))
  (newline)
  (when (not (null? args))
    (car args)))

(define (->flonum x)
  (cond
   [(real? x) ((*precision*) x)]
   [(bigfloat? x) ((*precision*) (bigfloat->flonum x))]))

; Functions used by our benchmarks
(define (cotan x)
  (/ 1 (tan x)))

(define (square x)
  (* x x))

(define (ordinary-float? x)
  (not (or (infinite? x) (nan? x))))

(define (=-or-nan? x1 x2)
  (or (= x1 x2)
      (and (nan? x1) (nan? x2))))

(define (list= l1 l2)
  (and l1 l2 (andmap =-or-nan? l1 l2)))

(define (list< list1 list2)
  "Compares lists lexicographically."
  ; Who picked this terrible API design of returning '< or '>
  (eq? (datum-order list1 list2) '<))

(define (enumerate . l)
  (apply map list (range (length (car l))) l))

(define (take-up-to l k)
  ; This is unnecessarily slow. It is O(l), not O(k).
  ; To be honest, it just isn't that big a deal for now.
  (take l (min k (length l))))
