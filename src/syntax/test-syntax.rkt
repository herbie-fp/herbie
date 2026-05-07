#lang racket

(require "syntax.rkt"
         "platform.rkt"
         "sugar.rkt"
         "types.rkt")

(module+ test
  (require rackunit
           "../config.rkt"
           "../syntax/load-platform.rkt")

  (activate-platform! (*platform-name*))
  (define f64 (get-representation 'binary64))

  (define sin-proc (impl-info 'sin.f64 'fl))
  (check-equal? (sin-proc 0.0) 0.0 "sin(0) = 0")

  ; get-fpcore-impl
  (define (get-impl op props itypes)
    (get-fpcore-impl op props itypes))

  (check-equal? (get-impl '+ '((:precision . binary64)) (list f64 f64)) '+.f64)
  (check-equal? (get-impl '+ '((:precision . binary64) (:description . "test")) (list f64 f64))
                '+.f64)
  (check-equal? (get-impl 'sin '((:precision . binary64)) (list f64)) 'sin.f64)

  ; fpcore->spec
  (check-equal? (fpcore->spec '(log1p x)) '(log (+ 1 x)))
  (check-equal? (fpcore->spec '(hypot x y)) '(sqrt (+ (* x x) (* y y))))
  (check-equal? (fpcore->spec '(fma x y z)) '(+ (* x y) z))

  (void))
