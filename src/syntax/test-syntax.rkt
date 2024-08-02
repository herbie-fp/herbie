#lang racket

(require "load-plugin.rkt"
         "syntax.rkt"
         "types.rkt"
         (submod "syntax.rkt" internals))

(module+ test
  (require rackunit
           math/bigfloat)

  (load-herbie-builtins)

  ; log1pmd(x) = log1p(x) - log1p(-x)

  (define-operator (log1pmd real) real)

  (define-operator-impl (log1pmd log1pmd.f64 binary64)
                        binary64
                        [spec (lambda (x) (- (log (+ 1 x)) (log (+ 1 (neg x)))))]
                        [fpcore (! :precision binary64 (log1pmd x))])

  (define log1pmd-proc (impl-info 'log1pmd.f64 'fl))
  (define log1pmd-vals '((0.0 . 0.0) (0.5 . 1.0986122886681098) (-0.5 . -1.0986122886681098)))
  (for ([(pt out) (in-dict log1pmd-vals)])
    (check-equal? (log1pmd-proc pt) out (format "log1pmd(~a) = ~a" pt out)))

  ; fast sine

  (define-operator-impl (sin fast-sin.f64 binary64)
                        binary64
                        [spec (lambda (x) (sin x))]
                        [fpcore (! :precision binary64 :math-library fast (sin x))]
                        [fl
                         (lambda (x)
                           (parameterize ([bf-precision 12])
                             (bigfloat->flonum (bfsin (bf x)))))])

  (define sin-proc (impl-info 'fast-sin.f64 'fl))
  (define sin-vals '((0.0 . 0.0) (1.0 . 0.841552734375) (-1.0 . -0.841552734375)))
  (for ([(pt out) (in-dict sin-vals)])
    (check-equal? (sin-proc pt) out (format "sin(~a) = ~a" pt out)))

  ; get-fpcore-impl

  (define f64 (get-representation 'binary64))
  (define (get-impl expr itypes props)
    (get-fpcore-impl expr itypes props #:impls (all-operator-impls)))

  (check-equal? (get-impl '(+ x y) (list f64 f64) '((:precision . binary64))) '+.f64)
  (check-equal? (get-impl '(+ a b) (list f64 f64) '((:precision . binary64))) '+.f64)
  (check-equal? (get-impl '(+ a b) (list f64 f64) '((:precision . binary64) (:description . "test")))
                '+.f64)

  (check-equal? (get-impl '(log1pmd x) (list f64) '((:precision . binary64))) 'log1pmd.f64)
  (check-equal? (get-impl '(sin x) (list f64) '((:precision . binary64))) 'sin.f64)
  (check-equal? (get-impl '(sin x) (list f64) '((:precision . binary64) (:math-library . fast)))
                'fast-sin.f64)

  (void))
