#lang racket

(require rackunit)
(require "../common.rkt" "../programs.rkt" "../float.rkt"
         "../ground-truth.rkt" "types.rkt" "../load-plugin.rkt"
         "rules.rkt" (submod "rules.rkt" internals))

(load-herbie-builtins)

(define num-test-points (make-parameter 100))

;; WARNING: These aren't treated as preconditions, they are only used for range inference
(define *conditions*
  `([asinh-2_binary64       . (>=.f64 x 0)]
    [asinh-2_binary32       . (>=.f32 x 0)]
    ;; These next three approximate pi so that range analysis will work
    [asin-sin-s_binary64    . (<=.f64 (fabs.f64 x) 1.5708)]
    [asin-sin-s_binary32    . (<=.f32 (fabs.f32 x) 1.5708)]
    [acos-cos-s_binary64    . (and (<=.f64 0 x) (<=.f64 x 3.1416))]
    [acos-cos-s_binary32    . (and (<=.f32 0 x) (<=.f64 x 3.1416))]
    [atan-tan-s_binary64    . (<=.f64 (fabs.f64 x) 1.5708)]
    [atan-tan-s_binary32    . (<=.f32 (fabs.f32 x) 1.5708)]))

(define (check-rule-correct test-rule)
  (match-define (rule name p1 p2 itypes repr) test-rule)
  (define fv (dict-keys itypes))
  (*context* (context fv repr (map (curry dict-ref itypes) fv)))

  (define precondition `(λ ,fv ,(dict-ref *conditions* name '(TRUE))))
  (define progs (list `(λ ,fv ,p1) `(λ ,fv ,p2)))
  (match-define (list pts exs1 exs2)
    (parameterize ([*num-points* (num-test-points)] [*max-find-range-depth* 0])
      (sample-points precondition progs (*context*))))

  (for ([pt (in-list pts)] [v1 (in-list exs1)] [v2 (in-list exs2)])
      (with-check-info (['point (map cons fv pt)] ['input v1] ['output v2])
       (check-eq? (ulp-difference v1 v2 repr) 1))))

(define (check-rule-fp-safe test-rule)
  (match-define (rule name p1 p2 itypes repr) test-rule)
  (define fv (dict-keys itypes))
  (*context* (context fv repr (map (curry dict-ref itypes) fv)))
  (define (make-point _)
    (for/list ([v (in-list fv)])
      (random-generate (dict-ref itypes v))))
  (define points (build-list (num-test-points) make-point))
  (define prog1 (eval-prog `(λ ,fv ,p1) 'fl repr))
  (define prog2 (eval-prog `(λ ,fv ,p2) 'fl repr))
  (define ex1 (map (curry apply prog1) points))
  (define ex2 (map (curry apply prog2) points))
  (for ([pt points])
    (with-check-info (['point (map list fv pt)])
      (define v1 (apply prog1 pt))
      (define v2 (apply prog1 pt))
      (check-equal? v1 v2))))

(module+ main
  (*needed-reprs* (map get-representation '(binary64 binary32 bool)))
  (define _ (*simplify-rules*))  ; force an update
  (num-test-points (* 100 (num-test-points)))
  (command-line
   #:args names
   (for ([name names])
     (eprintf "Checking ~a...\n" name)
     (define rule (first (filter (λ (x) (equal? (~a (rule-name x)) name)) (*rules*))))
     (check-rule-correct rule)
     (when (set-member? (*fp-safe-simplify-rules*) rule)
      (check-rule-fp-safe rule)))))

(module+ test
  (*needed-reprs* (map get-representation '(binary64 binary32 bool)))
  (define _ (*simplify-rules*))  ; force an update
  (for* ([test-ruleset (*rulesets*)] [test-rule (first test-ruleset)])
    (test-case (~a (rule-name test-rule))
      (check-rule-correct test-rule)))

  (for* ([test-ruleset (*rulesets*)]
         [test-rule (first test-ruleset)]
         #:when (set-member? (*fp-safe-simplify-rules*) test-rule))
    (test-case (~a (rule-name test-rule))
      (check-rule-fp-safe test-rule))))
