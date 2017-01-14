#lang racket
(require "../common.rkt")
(require math/flonum)
(provide eval-sampler)

(module+ test
  (require rackunit))

(define (sample-float)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 32) 4 #f)))

(define (sample-double)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f)))

(define (sample-default)
  (((flag 'precision 'double) sample-double sample-float)))

(define (sample-uniform a b)
  (+ (* (random) (- b a)) a))

(define (sample-int)
  (- (random-exp 32) (expt 2 31)))

(define (sample-bounded lo hi #:left-closed? [left-closed? #t] #:right-closed? [right-closed? #t])
  (define lo* (exact->inexact lo))
  (define hi* (exact->inexact hi))
  (cond
   [(> lo* hi*) #f]
   [(= lo* hi*)
    (if (and left-closed? right-closed?) lo* #f)]
   [(< lo* hi*)
    (define ordinal (- (flonum->ordinal hi*) (flonum->ordinal lo*)))
    (define num-bits (ceiling (/ (log ordinal) (log 2))))
    (define random-num (random-exp (inexact->exact num-bits)))
    (if (or (and (not left-closed?) (equal? 0 random-num))
            (and (not right-closed?) (equal? ordinal random-num))
            (> random-num ordinal))
      ;; Happens with p < .5 so will not loop forever
      (sample-bounded lo hi #:left-closed? left-closed? #:right-closed? right-closed?)
      (ordinal->flonum (+ (flonum->ordinal lo*) random-num)))]))

(module+ test
  (check-true (<= 1.0 (sample-bounded 1 2) 2.0))
  (let ([a (sample-bounded 1 2 #:left-closed? #f)])
    (check-true (< 1 a))
    (check-true (<= a 2)))
  (check-false (sample-bounded 1 1.0 #:left-closed? #f) "Empty interval due to left openness")
  (check-false (sample-bounded 1 1.0 #:right-closed? #f) "Empty interval due to right openness")
  (check-false (sample-bounded 1 1.0 #:left-closed? #f #:right-closed? #f)
               "Empty interval due to both-openness")
  (check-false (sample-bounded 2.0 1.0) "Interval bounds flipped"))

(define (eval-op op)
  (match op ['> >] ['< <] ['>= >=] ['<= <=]))

(define-match-expander op
  (λ (stx)
    (syntax-case stx ()
      [(_ val)
       #'(and (or '< '> '>= '<=) (app eval-op val))])))

(define (eval-sampler expr)
  (match expr
    ['default sample-default]
    [(? number? x) (const x)]
    [`(uniform ,(? number? a) ,(? number? b)) (λ () (sample-uniform a b))]
    ['int sample-int]
    [(list (op op) (? number? lb) sub)
     (define sub* (eval-sampler sub))
     (λ () (let ([y (sub*)]) (and (op lb y) y)))]
    [(list (op op) sub (? number? ub))
     (define sub* (eval-sampler sub))
     (λ () (let ([y (sub*)]) (and (op y ub) y)))]
    [(list (op op) (? number? lb) sub (? number? ub))
     (define sub* (eval-sampler sub))
     (λ () (let ([y (sub*)]) (and (op lb y ub) y)))]))
