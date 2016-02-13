#lang racket
(require "common.rkt")
(provide eval-sampler)

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
    [`(uniform ,(? number? a) ,(? number? b)) (curry sample-uniform a b)]
    ['int sample-int]
    [`(positive ,sub)
     (define sub* (eval-sampler sub))
     (λ () (let ([y (sub*)]) (and (not (zero? y)) (abs y))))]
    [(list (op op) (? number? lb) sub)
     (define sub* (eval-sampler sub))
     (λ () (let ([y (sub*)]) (and (op lb y) y)))]
    [(list (op op) sub (? number? ub))
     (define sub* (eval-sampler sub))
     (λ () (let ([y (sub*)]) (and (op y ub) y)))]
    [(list (op op) (? number? lb) sub (? number? ub))
     (define sub* (eval-sampler sub))
     (λ () (let ([y (sub*)]) (and (op lb y ub) y)))]))
