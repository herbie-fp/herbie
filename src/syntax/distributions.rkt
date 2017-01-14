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

(define (sample-bounded lo hi [left-closed #t] [right-closed #t])
  (cond [(> lo hi) #f]
        [(and (equal? (exact->inexact hi) (exact->inexact lo)) (or (not left-closed) (not right-closed))) #f]
        [(equal? (exact->inexact hi) (exact->inexact lo)) (exact->inexact lo)]
        [else
          (let* ([ordinal (- (flonum->ordinal (exact->inexact hi)) (flonum->ordinal (exact->inexact lo)))]
                 [num-bits (ceiling (/ (log ordinal) (log 2)))]
                 [random-num (random-exp (inexact->exact num-bits))])
            (if (or (and (not left-closed) (equal? 0 random-num))
                    (and (not right-closed) (equal? ordinal random-num))
                    (> random-num ordinal))
              (sample-bounded lo left-closed hi right-closed)
              (ordinal->flonum (+ (flonum->ordinal (exact->inexact lo)) random-num))))]))

(module+ test
  (check-true (<= 1.0 (sample-bounded 1 2) 2.0))
  (let ([a (sample-bounded 1 2 #f)])
    (check-true (< 1 a))
    (check-true (<= a 2)))
  (check-false (sample-bounded 1 1.0 #f) "Empty interval due to left openness")
  (check-false (sample-bounded 1 1.0 #t #f) "Empty interval due to right openness")
  (check-false (sample-bounded 1 1.0 #f #f) "Empty interval due to both-openness")
  (check-false (sample-bounded 2.0 1.0 "Interval bounds flipped")))

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
