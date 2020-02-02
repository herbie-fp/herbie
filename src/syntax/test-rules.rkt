#lang racket

(require rackunit math/bigfloat)
(require "../common.rkt" "../programs.rkt" (submod "../points.rkt" internals))
(require "rules.rkt" (submod "rules.rkt" internals) "../interface.rkt")
(require "../programs.rkt" "../float.rkt")

(define num-test-points 1000)

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

(define (ival-ground-truth fv p repr)
  (λ (x) (ival-eval (eval-prog `(λ ,fv ,p) 'ival repr) x repr)))

(define ((with-hiprec f) x)
  (parameterize ([bf-precision 2000]) (apply f x)))

(define (bf-ground-truth fv p repr)
  (with-hiprec (compose (representation-bf->repr repr) (eval-prog `(λ ,fv ,p) 'bf repr))))

(define (check-rule-correct test-rule ground-truth)
  (match-define (rule name p1 p2 itypes otype) test-rule)
  (define fv (dict-keys itypes))
  (*var-reprs* (for/list ([(v t) (in-dict itypes)]) (cons v (get-representation* t))))
  (define repr (get-representation (match otype ['real 'binary64] [x x])))

  (define make-point
    (let ([sample (make-sampler `(λ ,fv ,(dict-ref *conditions* name 'TRUE)) (get-representation 'binary64))])
      (λ ()
        (if (dict-has-key? *conditions* name)
            (sample)
            (for/list ([v fv] [i (in-naturals)])
              (match (dict-ref (rule-itypes test-rule) v)
                ['real (sample-double)]
                ['complex (make-rectangular (sample-double) (sample-double))]
                [rname (random-generate (get-representation rname))]))))))

  (define points (for/list ([n (in-range num-test-points)]) (make-point)))
  (define prog1 (ground-truth fv p1 repr))
  (define prog2 (ground-truth fv p2 repr))

  (define ex1 (map prog1 points))
  (define ex2 (map prog2 points))
  (define errs
    (for/list ([pt points] [v1 ex1] [v2 ex2]
               #:unless (or (eq? v1 +nan.0) (eq? v2 +nan.0))
               #:when (and (ordinary-value? v1 repr)
                           (ordinary-value? v2 repr)))
      (with-check-info (['point (map cons fv pt)] ['method (object-name ground-truth)]
                        ['input v1] ['output v2])
        (check-eq? (ulp-difference v1 v2 repr) 0))))
  (when (< (length errs) (/ num-test-points 10))
    (fail-check "Not enough points sampled to test rule")))

(define (check-rule-fp-safe test-rule)
  (match-define (rule name p1 p2 itypes otype) test-rule)
  (define fv (dict-keys itypes))
  (*var-reprs* (for/list ([(v t) (in-dict itypes)]) (cons v (get-representation* t))))
  (define repr (get-representation (match otype ['real 'binary64] [x x])))
  (define (make-point)
    (for/list ([v fv])
      (match (dict-ref (rule-itypes test-rule) v)
        ['real (sample-double)]
        ['bool (if (< (random) .5) false true)]
        ['complex (make-rectangular (sample-double) (sample-double))])))
  (define point-sequence (in-producer make-point))
  (define points (for/list ([n (in-range num-test-points)] [pt point-sequence]) pt))
  (define prog1 (eval-prog `(λ ,fv ,p1) 'fl repr))
  (define prog2 (eval-prog `(λ ,fv ,p2) 'fl repr))
  (define-values (ex1 ex2)
    (for/lists (ex1 ex2) ([pt points])
      (values (apply prog1 pt) (apply prog2 pt))))
  (for ([pt points] [v1 ex1] [v2 ex2])
    (with-check-info (['point (map list fv pt)])
      (check-equal? v1 v2))))

(module+ main
  (command-line
   #:args names
   (for ([name names])
     (eprintf "Checking ~a...\n" name)
     (define rule (first (filter (λ (x) (equal? (~a (rule-name x)) name)) (*rules*))))
     (check-rule-correct rule ival-ground-truth))))

(module+ test
  (for* ([test-ruleset (*rulesets*)] [test-rule (first test-ruleset)])
    (define ground-truth
      (cond
       [(and (expr-supports? (rule-input test-rule) 'ival)
             (expr-supports? (rule-output test-rule) 'ival))
        ival-ground-truth]
       [else
        (unless (set-member? (second test-ruleset) 'complex)
          (fail-check "Real or boolean rule not supported by intervals"))
        (when (dict-has-key? *conditions* (rule-name test-rule))
          (fail-check "Using bigfloat sampling on a rule with a condition"))
        bf-ground-truth]))

    (test-case (~a (rule-name test-rule))
      (check-rule-correct test-rule ground-truth)))

  (for* ([test-ruleset (*rulesets*)]
         [test-rule (first test-ruleset)]
         #:when (set-member? (*fp-safe-simplify-rules*) test-rule))
    (test-case (~a (rule-name test-rule))
      (check-rule-fp-safe test-rule))))
