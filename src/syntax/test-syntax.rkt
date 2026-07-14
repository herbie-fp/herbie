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

  (define sincos-proc (impl-info 'sincos.f64 'fl))
  (check-equal? (sincos-proc 0.0) #(0.0 1.0) "sincos(0) = [0, 1]")

  ; get-fpcore-impl
  (define (get-impl op props itypes)
    (get-fpcore-impl op props itypes))

  (check-equal? (get-impl '+ '((:precision . binary64)) (list f64 f64)) '+.f64)
  (check-equal? (get-impl '+ '((:precision . binary64) (:description . "test")) (list f64 f64))
                '+.f64)
  (check-equal? (get-impl 'sin '((:precision . binary64)) (list f64)) 'sin.f64)
  (check-equal? (get-impl 'sincos '((:precision . binary64)) (list f64)) 'sincos.f64)

  (define ctx (context '(x) f64 (list f64)))
  (check-equal? (fpcore->prog '(ref (sincos x) 0) ctx)
                '(ref.f64 (sincos.f64 x) #s(literal 0 binary64)))
  (check-equal? (fpcore->prog '(ref (sincos x) 1) ctx)
                '(ref.f64 (sincos.f64 x) #s(literal 1 binary64)))

  (void))
