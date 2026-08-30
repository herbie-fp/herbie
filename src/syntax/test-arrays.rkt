#lang racket

(require math/flonum
         "float.rkt"
         "platform.rkt"
         "platform-state.rkt"
         "sugar.rkt"
         "type-check.rkt"
         "types.rkt")

(module+ test
  (require rackunit
           "../config.rkt"
           "load-platform.rkt")

  (activate-platform! (*platform-name*))

  (define <b32> (get-representation 'binary32))
  (define <b64> (get-representation 'binary64))
  (define mixed (get-representation '(array binary32 binary64)))

  ;; type identity

  (check-equal? (representation-name mixed) '(array binary32 binary64))
  (check-equal? (representation-type mixed) '(array real real))
  (check-equal? mixed (make-array-representation #:slots (list <b32> <b64>)))
  (check-not-equal? mixed (get-representation '(array binary64 binary32)))
  (check-equal? (get-representation (representation-name mixed)) mixed)
  (check-equal? (repr->prop mixed) '((:precision . binary32)))

  ;; generated impls

  (parameterize ([*active-platform* (platform-copy (*active-platform*))])
    (ensure-array-impls! mixed)
    (define ctor (array-impl-name mixed))
    (check-equal? ctor 'array<binary32:binary64>)
    (check-equal? (impl-info ctor 'itype) (list <b32> <b64>))
    (check-equal? (impl-info ctor 'otype) mixed)
    (check-equal? ((impl-info ctor 'fl) 1.0 2.0) (vector 1.0 2.0))
    (check-equal? (impl-info (array-ref-impl-name mixed 1) 'otype) <b64>)
    (check-equal? ((impl-info (array-ref-impl-name mixed 1) 'fl) (vector 1.0 2.0)) 2.0)
    (define ctx (context '(a b) mixed (list <b32> <b64>)))
    (check-equal? (fpcore->prog '(array a b) ctx) '(array<binary32:binary64> a b)))

  ;; distance, measured per slot in that slot's own representation

  (define (repr-next repr x)
    ((representation-ordinal->repr repr) (add1 ((representation-repr->ordinal repr) x))))

  (let ([ulps (repr-ulps mixed)])
    (check-equal? (ulps (vector 1.0 1.0) (vector 1.0 1.0)) 2)
    (check-equal? (ulps (vector 1.0 1.0) (vector (repr-next <b32> 1.0) 1.0)) 3)
    (check-equal? (ulps (vector 1.0 1.0) (vector 1.0 (repr-next <b64> 1.0))) 3))

  ;; real conversions

  (let ([v (real->repr (vector 1/3 1/3) mixed)])
    (check-equal? (vector-ref v 0) (flsingle (exact->inexact 1/3)))
    (check-equal? (vector-ref v 1) (exact->inexact 1/3)))
  (check-equal? (repr->real (real->repr (vector 0.5 0.25) mixed) mixed) (vector 1/2 1/4))

  ;; type checking

  (let-values ([(repr _ctx) (assert-program-typed! #'(FPCore ((! :precision binary32 a) b)
                                                             :precision
                                                             binary64
                                                             (array (! :precision binary32 (+ a 1))
                                                                    (* b 2))))])
    (check-equal? (representation-name repr) '(array binary32 binary64)))

  (let-values ([(repr _ctx)
                (assert-program-typed!
                 #'(FPCore ((! :precision binary32 a) b) :precision binary64 (ref (array a b) 1)))])
    (check-equal? (representation-name repr) 'binary64))

  (check-exn exn:fail?
             (lambda ()
               (assert-program-typed! #'(FPCore (x) :precision binary64 (ref (array x x) 2)))))

  (check-exn exn:fail?
             (lambda () (assert-program-typed! #'(FPCore (x) :precision binary64 (array (< x 5) x)))))

  ;; impls survive the platform re-activation before every run

  (parameterize ([*active-platform* (platform-copy (*active-platform*))]
                 [*platform-extensions* '()])
    (activate-platform! (platform-serialize))
    (check-equal? ((impl-info (array-impl-name mixed) 'fl) 1.0 2.0) (vector 1.0 2.0))
    (check-equal? ((impl-info (array-ref-impl-name mixed 1) 'fl) (vector 1.0 2.0)) 2.0))

  (void))
