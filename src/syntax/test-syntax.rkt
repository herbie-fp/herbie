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
  (define f64x3 (get-representation '(array binary64 3)))
  (define f64x3x3 (get-representation '(array (array binary64 3) 3)))

  (define sin-proc (impl-info 'sin.f64 'fl))
  (check-equal? (sin-proc 0.0) 0.0 "sin(0) = 0")

  ; get-fpcore-impl
  (define (get-impl op props itypes)
    (get-fpcore-impl op props itypes))

  (check-equal? (get-impl '+ '((:precision . binary64)) (list f64 f64)) '+.f64)
  (check-equal? (get-impl '+ '((:precision . binary64) (:description . "test")) (list f64 f64))
                '+.f64)
  (check-equal? (get-impl 'sin '((:precision . binary64)) (list f64)) 'sin.f64)
  (check-equal? (get-impl 'array '((:precision . binary64)) (list f64x3 f64x3 f64x3)) 'array3x3.f64)
  (check-equal? (get-impl 'ref '((:precision . binary64)) (list f64x3x3 f64)) 'ref.r3r3.f64)
  (check-equal? (fpcore->spec '(ref A 2 1)) '(ref (ref A 2) 1))

  (void))
