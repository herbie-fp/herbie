#lang racket

(require rackunit)
(require "../utils/common.rkt"
         "../syntax/matcher.rkt"
         "programs.rkt"
         "rules.rkt")

(define (undefined-conditions x)
  (reap [sow]
        (for ([subexpr (in-list (all-subexpressions x))])
          (match subexpr
            [`(acos ,x) (sow `(< 1 (fabs ,x)))]
            [`(acosh ,x) (sow `(< ,x 1))]
            [`(asin ,x) (sow `(< 1 (fabs ,x)))]
            [`(atanh ,x) (sow `(<= 1 (fabs ,x)))]
            [`(fmod ,x ,y) (sow `(== ,y 0))]
            [`(lgamma ,x) (sow `(and (<= ,x 0) (integer? ,x)))]
            [`(log ,x) (sow `(<= ,x 0))]
            [`(log10 ,x) (sow `(<= ,x 0))]
            [`(log2 ,x) (sow `(<= ,x 0))]
            [`(logb ,x) (sow `(== ,x 0))]
            [`(remainder ,x ,y) (sow `(== ,y 0))]
            [`(sqrt ,x) (sow `(< ,x 0))]
            [`(tan ,x) (sow `(== (cos ,x) 0))]
            [`(tgamma ,x) (sow `(and (<= ,x 0) (integer? ,x)))]
            [`(pow ,a ,b)
             (sow `(and (< ,a 0) (even-denominator? ,b)))
             (sow `(and (== ,a 0) (< ,b 0)))]
            [`(/ ,a ,b) (sow `(== ,b 0))]
            [_ (void)]))))

(define (reify c)
  (if c
      '(TRUE)
      '(FALSE)))

;; In general, the normal forms are:
;; - Only use ==, < conditions
;; - One side of a comparison is always a constant
;; - For ==, the constant is on the right

(define (rewrite-all expr a b)
  ;; This is an ugly / slow way to do this but I guess it works
  (define matches
    (for/list ([subexpr (in-list (all-subexpressions expr))]
               #:when (pattern-match a subexpr))
      (cons subexpr (pattern-substitute b (pattern-match a subexpr)))))
  (for/fold ([expr expr]) ([(from to) (in-dict matches)])
    (replace-expression expr from to)))

(define simplify-patterns
  (list '[(cos (neg a)) . (cos a)]
        '[(sin (neg a)) . (neg (sin a))]
        '[(cos (+ a (PI))) . (neg (cos a))]
        '[(cos (+ a (/ (PI) 2))) . (neg (sin a))]
        '[(cos (acos a)) . a]
        '[(cos (asin a)) . (sqrt (- 1 (* a a)))]
        '[(fabs (neg a)) . (fabs a)]
        '[(fabs (fabs a)) . (fabs a)]))

(define (simplify-expression expr)
  (for/fold ([expr expr]) ([(a b) (in-dict simplify-patterns)])
    (rewrite-all expr a b)))

(define (simplify-condition term)
  (match term
    [`(== ,(? number? a) ,(? number? b)) (reify (= a b))]
    [`(< ,(? number? a) ,(? number? b)) (reify (< a b))]
    [`(> ,(? number? a) ,(? number? b)) (reify (> a b))]
    [`(== (PI) ,(? number?)) '(FALSE)]
    [`(== ,(? number? a) ,b) `(== ,b ,a)] ; canonicalize
    [`(== (+ ,(? number? a) ,b) ,c) `(== (+ ,b ,a) ,c)] ; canonicalize
    [`(== (- ,(? number? a) ,b) ,c) `(== (neg (- ,b ,a)) ,c)] ; canonicalize
    [`(<= ,a ,b) `(or (< ,a ,b) (== ,a ,b))] ; canonicalize

    [`(== (cbrt ,a) 0) `(== ,a 0)]
    [`(== (sqrt ,a) 0) `(== ,a 0)]
    [`(== (neg ,a) 0) `(== ,a 0)]
    [`(== (fabs ,a) 0) `(== ,a 0)]
    [`(== (* ,a ,b) 0) `(or (== ,a 0) (== ,b 0))]
    [`(== (/ ,a ,b) 0) `(== ,a 0)]
    [`(== (pow ,a ,b) 0) `(and (== ,a 0) (> ,b 0))]

    [`(== (fabs ,a) 1) `(or (== ,a 1) (== ,a -1))]
    [`(== (+ ,x 1) 0) `(== ,x -1)]
    [`(== (- ,a 1) 0) `(== ,a 1)]
    [`(== (* ,a ,a) 1) `(== (fabs ,a) 1)]

    [`(< (* ,a ,a) 0) '(FALSE)]
    [`(< (sqrt ,a) 0) '(FALSE)]
    [`(,(or '< '==) (cosh ,a) ,(? (conjoin number? (curryr < 1)))) '(FALSE)]
    [`(,(or '< '==) (exp ,a) ,(? (conjoin number? (curryr <= 0)))) '(FALSE)]
    [`(,(or '< '==) (* ,a ,a) ,(? (conjoin number? (curryr < 0)))) '(FALSE)]
    [`(,(or '< '==) (fabs ,a) ,(? (conjoin number? (curryr < 0)))) '(FALSE)]

    [`(< (/ 1 ,a) 0) `(< ,a 0)]
    [`(< (neg ,a) 0) `(> ,a 0)]
    [`(< (* 2 ,a) ,(? number? b)) `(< ,a ,(/ b 2))]
    [`(< (+ 1 ,a) 0) `(< ,a -1)]
    [`(< (/ ,x ,(? (conjoin number? positive?))) 0) `(< ,x 0)]
    [`(< (+ ,x 1) 0) `(< ,x -1)]
    [`(< (- ,a 1) ,(? number? b)) `(< ,a ,(+ b 1))]
    [`(< (- 1 ,x) 0) `(< 1 ,x)]
    [`(< (cbrt ,a) 0) `(< ,a 0)]

    [`(< (* ,a ,a) 1) `(< (fabs ,a) 1)]
    [`(< 1 (* ,a ,a)) `(< 1 (fabs ,a))]

    [`(== (+ ,x (sqrt (+ (* ,x ,x) 1))) 0) '(FALSE)]
    [`(== (+ ,x (sqrt (- (* ,x ,x) 1))) 0) '(FALSE)]
    [`(< (+ ,x (sqrt (+ (* ,x ,x) 1))) 0) '(FALSE)]
    [`(< (+ ,x (sqrt (- (* ,x ,x) 1))) 0) `(<= x -1)]

    [`(< 1 (fabs (,(or 'cos 'sin) x))) '(FALSE)]

    [`(== (/ (+ 1 ,x) (- 1 ,x)) 0) `(== ,x -1)]
    [`(< (/ (+ 1 ,x) (- 1 ,x)) 0) `(< 1 (fabs x))]

    [`(== (+ (cos ,a) (cos ,b)) 0) `(or (== (cos (/ (+ ,a ,b) 2)) 0) (== (cos (/ (- ,a ,b) 2)) 0))]
    [`(== (cos (* 2 ,a)) 0) `(or (== (tan ,a) 1) (== (tan ,a) -1))]
    [`(== (tan ,a) 0) `(== (sin ,a) 0)]

    [`(even-denominator? (neg ,b)) `(even-denominator? ,b)]
    [`(even-denominator? (+ ,b 1)) `(even-denominator? ,b)]
    [`(even-denominator? (/ ,b 3)) `(even-denominator? ,b)]
    [`(even-denominator? ,(? rational? a))
     (if (even? (denominator a))
         '(TRUE)
         '(FALSE))]

    [`(and ,sub ...)
     (define subs (map (compose simplify-conditions list) sub))
     (define conjunctions (apply cartesian-product subs))
     (cons 'or
           (for/list ([conj (in-list conjunctions)])
             (match (set-remove conj '(TRUE))
               ['(FALSE) '(TRUE)]
               [(list a) a]
               [(list as ...) (cons 'and as)])))]
    [_ term]))

(define (simplify-conditions xs)
  (define simple1
    (reap [sow]
          (for ([x (remove-duplicates xs)])
            (define out (simplify-condition (simplify-expression x)))
            (match out
              [`(or ,terms ...) (for-each sow terms)]
              [`(FALSE) (void)]
              [_ (sow out)]))))
  (if (equal? simple1 xs)
      xs
      (simplify-conditions simple1)))

(define soundness-proofs
  '((pow-plus (implies (< b -1) (< b 0)))
    (pow-sqr (implies (even-denominator? (* 2 b)) (even-denominator? b)))
    (hang-0p-tan (implies (== (cos (/ a 2)) 0) (== (cos a) -1)))
    (hang-0p-tan-rev (implies (== (cos (/ a 2)) 0) (== (cos a) -1)))
    (hang-0m-tan (implies (== (cos (/ a 2)) 0) (== (cos a) -1)))
    (hang-0m-tan-rev (implies (== (cos (/ a 2)) 0) (== (cos a) -1)))
    (tanh-sum (implies (== (* (tanh x) (tanh y)) -1) (FALSE)))
    (tanh-def-a (implies (== (+ (exp x) (exp (neg x))) 0) (FALSE)))
    (acosh-def (implies (< x 1) (or (< x -1) (== x -1) (< (fabs x) 1))))
    (acosh-def-rev (implies (< x 1) (or (< x -1) (== x -1) (< (fabs x) 1))))
    (sqrt-undiv (implies (< (/ x y) 0) (or (< x 0) (< y 0))))
    (sqrt-unprod (implies (< (* x y) 0) (or (< x 0) (< y 0))))
    (tan-sum-rev (implies (== (cos (+ x y)) 0) (== (* (tan x) (tan y)) 1)))
    (sum-log (implies (< (* x y) 0) (or (< x 0) (< y 0))))
    (diff-log (implies (< (/ x y) 0) (or (< x 0) (< y 0))))
    (exp-to-pow (implies (and a b) a))
    (sinh-acosh (implies (< (fabs x) 1) (< x 1)))
    (acosh-2-rev (implies (< (fabs x) 1) (< x 1)))
    (tanh-acosh (implies (< (fabs x) 1) (< x 1)) (implies (== x 0) (< x 1)))
    (hang-p0-tan (implies (== (cos (/ a 2)) 0) (== (sin a) 0)))
    (hang-m0-tan (implies (== (cos (/ a 2)) 0) (== (sin a) 0)))
    (sqrt-pow2 (implies (and a b) a))
    (pow-div (implies (< (- b c) 0) (or (< b 0) (> c 0)))
             (implies (even-denominator? (- b c)) (or (even-denominator? b) (even-denominator? c))))
    (pow-prod-up (implies (< (+ b c) 0) (or (< b 0) (< c 0)))
                 (implies (even-denominator? (+ b c))
                          (or (even-denominator? b) (even-denominator? c))))
    (pow-prod-down (implies (< (* b c) 0) (or (< b 0) (< c 0))))
    (log-pow-rev (implies (and a b) a) (implies (< (pow a b) 0) (< a 0)))))

(define (execute-proof proof terms)
  (for/fold ([terms (simplify-conditions terms)]) ([step (in-list proof)])
    (match step
      [`(implies ,a ,b) (simplify-conditions (map (curryr rewrite-all a b) terms))])))

(define (potentially-unsound)
  (define num 0)
  (for ([rule (in-list (*sound-rules*))])
    (test-case (~a (rule-name rule))
      (define proof (dict-ref soundness-proofs (rule-name rule) '()))
      (define lhs-bad (execute-proof proof (undefined-conditions (rule-input rule))))
      (define rhs-bad (execute-proof proof (undefined-conditions (rule-output rule))))
      (define extra (set-remove (set-subtract rhs-bad lhs-bad) '(FALSE)))
      (with-check-info (('lhs-bad lhs-bad)) (check-equal? empty extra)))))

(module+ test
  (potentially-unsound))
