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
                '(ref.0.array<binary64:binary64> (sincos.f64 x)))
  (check-equal? (fpcore->prog '(ref (sincos x) 1) ctx)
                '(ref.1.array<binary64:binary64> (sincos.f64 x)))
  ; fpcore->spec
  (check-equal? (fpcore->spec '(log1p x)) '(log (+ 1 x)))
  (check-equal? (fpcore->spec '(hypot x y)) '(sqrt (+ (* x x) (* y y))))
  (check-equal? (fpcore->spec '(fma x y z)) '(+ (* x y) z))
  (check-equal? (fpcore->spec '(+ x y z)) '(+ (+ x y) z))
  (check-equal? (fpcore->spec '(!= x y z)) '(and (and (!= x y) (!= x z)) (!= y z)))
  (check-equal? (fpcore->spec '(let ([x (+ x 1)]
                                     [y (+ x 2)])
                                 (+ x y)))
                '(+ (+ x 1) (+ x 2)))
  (check-equal? (fpcore->spec '(let* ([x (+ x 1)]
                                      [y (+ x 2)])
                                 y))
                '(+ (+ x 1) 2))
  (check-equal? (fpcore->spec '(or)) '(FALSE))
  (check-equal?
   (fpcore->prog '(let ([y (+ x 1)]) (array (! :precision binary32 y) (! :precision binary64 y))) ctx)
   `(array<binary64:binary64> (+.f64 x ,(literal 1 'binary64)) (+.f64 x ,(literal 1 'binary64))))

  (void))
