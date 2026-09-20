#lang racket

(require rackunit
         "egg-herbie.rkt"
         "rules.rkt"
         "../syntax/block.rkt"
         "../syntax/load-platform.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt")

(activate-platform! "c")

(module+ test
  (define f64 (get-representation 'binary64))
  (define array64 (get-representation '(array binary64 2)))
  (define sincos-spec '(array (sin x) (cos x)))
  (define lowering-rules (array-lowering-rules 'sincos.f64 sincos-spec))

  (check-equal? (map rule-input lowering-rules) '((sin x) (cos x)))
  (check-equal? (map rule-output lowering-rules)
                '((ref (array (sin x) (cos x)) 0) (ref (array (sin x) (cos x)) 1)))

  (define ctx (context '(x) array64 (list f64)))
  (define-values (block vs) (progs->block (list sincos-spec) #:ctx ctx))
  (define runner (make-egraph block vs '(lower lower) ctx))

  (check-true (egraph-equal? runner sincos-spec '(sincos.f64 x)))
  (define best (first (first (egraph-best runner block (list array64)))))
  (check-equal? ((block-exprs block) best) '(sincos.f64 x))

  (define scalar-ctx (context '(x) f64 (list f64)))
  (define-values (scalar-block scalar-vs) (progs->block (list '(sin x)) #:ctx scalar-ctx))
  (define scalar-runner (make-egraph scalar-block scalar-vs '(lower lower) scalar-ctx))
  (check-true
   (egraph-equal? scalar-runner '(sin x) '(ref.f64 (sincos.f64 x) #s(literal 0 binary64)))))
