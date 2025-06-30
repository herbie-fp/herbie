#lang racket

(require "syntax.rkt"
         "platform.rkt"
         "types.rkt")

(module+ test
  (require rackunit
           math/bigfloat
           "../config.rkt")

  (activate-platform! (*platform-name*))
  (define platform (platform-copy (*active-platform*)))
  (define binary64 (get-representation 'binary64))
  ; log1pmd(x) = log1p(x) - log1p(-x)

  (parameterize ([*active-platform* platform])

    (platform-register-implementation!
     platform
     (make-operator-impl (log1pmd.f64 [x : binary64])
                         binary64
                         #:spec (- (log (+ 1 x)) (log (+ 1 (neg x))))
                         #:fpcore (! :precision binary64 (log1pmd x))
                         #:cost 6400))
    (define log1pmd-proc (impl-info 'log1pmd.f64 'fl))
    (define log1pmd-vals '((0.0 . 0.0) (0.5 . 1.0986122886681098) (-0.5 . -1.0986122886681098)))
    (for ([(pt out) (in-dict log1pmd-vals)])
      (check-equal? (log1pmd-proc pt) out (format "log1pmd(~a) = ~a" pt out)))

    ; fast sine
    (platform-register-implementation!
     platform
     (make-operator-impl (fast-sin.f64 [x : binary64])
                         binary64
                         #:spec (sin x)
                         #:fpcore (! :precision binary64 :math-library fast (sin x))
                         #:fl (lambda (x)
                                (parameterize ([bf-precision 12])
                                  (bigfloat->flonum (bfsin (bf x)))))
                         #:cost 123))

    (define sin-proc (impl-info 'fast-sin.f64 'fl))
    (define sin-vals '((0.0 . 0.0) (1.0 . 0.841552734375) (-1.0 . -0.841552734375)))
    (for ([(pt out) (in-dict sin-vals)])
      (check-equal? (sin-proc pt) out (format "sin(~a) = ~a" pt out)))

    ; get-fpcore-impl

    (define f64 (get-representation 'binary64))
    (define (get-impl op props itypes)
      (get-fpcore-impl op props itypes))

    (check-equal? (get-impl '+ '((:precision . binary64)) (list f64 f64)) '+.f64)
    (check-equal? (get-impl '+ '((:precision . binary64)) (list f64 f64)) '+.f64)
    (check-equal? (get-impl '+ '((:precision . binary64) (:description . "test")) (list f64 f64))
                  '+.f64)

    (check-equal? (get-impl 'log1pmd '((:precision . binary64)) (list f64)) 'log1pmd.f64)
    (check-equal? (get-impl 'sin '((:precision . binary64)) (list f64)) 'sin.f64)
    (check-equal? (get-impl 'sin '((:precision . binary64) (:math-library . fast)) (list f64))
                  'fast-sin.f64))

  (void))
