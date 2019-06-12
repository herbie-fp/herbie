#lang racket

(module+ test
  (require "../programs.rkt" (submod "../points.rkt" internals) math/bigfloat)
  (require "softposit.rkt" "posits.rkt")

  ;; WARNING: These aren't treated as preconditions, they are only used for range inference
  (define *conditions*
    `([acosh-def  . (>= x 1)]
      [atanh-def  . (< (fabs x) 1)]
      [asin-acos  . (<= -1 x 1)]
      [acos-asin  . (<= -1 x 1)]
      [acosh-2    . (>= x 1)]
      [asinh-2    . (>= x 0)]
      [sinh-acosh . (> (fabs x) 1)]
      [sinh-atanh . (< (fabs x) 1)]
      [cosh-atanh . (< (fabs x) 1)]
      [tanh-acosh . (> (fabs x) 1)]
      ;; These next three unquote the pi computation so that range analysis will work
      [asin-sin-s . (<= (fabs x) ,(/ pi 2))]
      [acos-cos-s . (<= 0 x ,pi)]
      [atan-tan-s . (<= (fabs x) ,(/ pi 2))]))

  (for* ([test-ruleset (*rulesets*)] [test-rule (first test-ruleset)]
         ;; The posit rules currently fail, possibly due to halfpoints sampling
         #:unless (set-member? (second test-ruleset) 'posit))
    (match-define (rule name p1 p2 itypes) test-rule)
    (test-case (~a name)
      (define fv (dict-keys itypes))

      (define make-point
        (let ([sample (make-sampler `(λ ,fv ,(dict-ref *conditions* name 'TRUE)))])
          (λ ()
            (if (dict-has-key? *conditions* name)
                (sample)
                (for/list ([v fv] [i (in-naturals)])
                  (match (dict-ref (rule-itypes test-rule) v)
                    ['real (sample-double)]
                    ['bool (if (< (random) .5) false true)]
                    ['complex (make-rectangular (sample-double) (sample-double))]
                    ['posit8 (random-posit8)]
                    ['posit16 (random-posit16)]
                    ['posit32 (random-posit32)]
                    ['quire8 (random-quire8)]
                    ['quire16 (random-quire16)]
                    ['quire32 (random-quire32)]))))))

      (define-values (method prog1 prog2 points)
        (cond
         [(and (expr-supports? p1 'ival) (expr-supports? p2 'ival))
          (define prog1 (curry ival-eval (eval-prog `(λ ,fv ,p1) 'ival)))
          (define prog2 (curry ival-eval (eval-prog `(λ ,fv ,p2) 'ival)))
          (define points (for/list ([n (in-range num-test-points)]) (make-point)))
          (values 'ival prog1 prog2 points)]
         [else
          (unless (or (set-member? (second test-ruleset) 'complex)
                      (set-member? (second test-ruleset) 'posit))
            (fail-check "Real or boolean rule not supported by intervals"))
          (when (dict-has-key? *conditions* name)
            (fail-check "Using bigfloat sampling on a rule with a condition"))
          (define ((with-hiprec f) x) (parameterize ([bf-precision 2000]) (f x)))
          (define prog1 (with-hiprec (compose ->flonum (eval-prog `(λ ,fv ,p1) 'bf))))
          (define prog2 (with-hiprec (compose ->flonum (eval-prog `(λ ,fv ,p2) 'bf))))
          (define points (for/list ([n (in-range num-test-points)]) (make-point)))
          (values 'bf prog1 prog2 points)]))

      (define ex1 (map prog1 points))
      (define ex2 (map prog2 points))
      (define errs
        (for/list ([pt points] [v1 ex1] [v2 ex2]
                   #:when (and (ordinary-value? v1) (ordinary-value? v2)))
          (with-check-info (['point (map cons fv pt)] ['method method]
                            ['input v1] ['output v2])
            (check-eq? (ulp-difference v1 v2) 0))))
      (when (< (length errs) 100)
        (fail-check "Not enough points sampled to test rule")))))

(module+ test
  (require math/bigfloat "../programs.rkt" "../float.rkt")

  (for* ([test-ruleset (*rulesets*)]
         [test-rule (first test-ruleset)]
         #:when (set-member? (*fp-safe-simplify-rules*) test-rule))
    (test-case (~a (rule-name test-rule))
      (match-define (rule name p1 p2 _) test-rule)
      (define fv (free-variables p1))
      (define (make-point)
        (for/list ([v fv])
          (match (dict-ref (rule-itypes test-rule) v)
            ['real (sample-double)]
            ['bool (if (< (random) .5) false true)]
            ['complex (make-rectangular (sample-double) (sample-double))])))
      (define point-sequence (in-producer make-point))
      (define points (for/list ([n (in-range num-test-points)] [pt point-sequence]) pt))
      (define prog1 (eval-prog `(λ ,fv ,p1) 'fl))
      (define prog2 (eval-prog `(λ ,fv, p2) 'fl))
      (define ex1 (map prog1 points))
      (define ex2 (map prog2 points))
      (for ([pt points] [v1 ex1] [v2 ex2])
        (with-check-info (['point (map list fv pt)])
          (check-equal? v1 v2))))))
