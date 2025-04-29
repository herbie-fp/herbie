#lang racket

(require rackunit)
(require "../syntax/matcher.rkt"
         "programs.rkt"
         "rules.rkt")

(define (validity-conditions x)
  (append (match x
            [`(acos ,x) (list `(< 1 (fabs ,x)))]
            [`(acosh ,x) (list `(< ,x 1))]
            [`(asin ,x) (list `(< 1 (fabs ,x)))]
            [`(atanh ,x) (list `(<= 1 (fabs ,x)))]
            [`(fmod ,x ,y) (list `(== ,y 0))]
            [`(lgamma ,x) (list `(and (<= ,x 0) (integer? ,x)))]
            [`(log ,x) (list `(<= ,x 0))]
            [`(log10 ,x) (list `(<= ,x 0))]
            [`(log2 ,x) (list `(<= ,x 0))]
            [`(logb ,x) (list `(== ,x 0))]
            [`(remainder ,x ,y) (list `(== ,y 0))]
            [`(sqrt ,x) (list `(< ,x 0))]
            [`(tan ,x) (list `(== (cos ,x) 0))]
            [`(tgamma ,x) (list `(and (<= ,x 0) (integer? ,x)))]
            [`(pow ,a ,b) (list `(and (< ,a 0) (even-fraction? ,b)) `(and (== ,a 0) (< ,b 0)))]
            [`(/ ,a ,b) (list `(== ,b 0))]
            [else '()])
          (if (list? x)
              (append-map validity-conditions (cdr x))
              '())))

(define (reify c)
  (if c
      '((TRUE))
      '()))

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

(define (simplify-expression expr)
  (define patterns
    (list '[(cos (neg a)) . (cos a)]
          '[(sin (neg a)) . (sin a)]
          '[(cos (+ a (PI))) . (neg (cos a))]
          '[(cos (+ a (/ (PI) 2))) . (sin a)]
          '[(cos (acos a)) . a]
          '[(cos (asin a)) . (sqrt (- 1 (* a a)))]
          '[(fabs (neg a)) . (fabs a)]
          '[(fabs (fabs a)) . (fabs a)]))
  (for/fold ([expr expr]) ([(a b) (in-dict patterns)])
    (rewrite-all expr a b)))

(define (simplify-condition term)
  (match (simplify-expression term)
    [`(== ,(? number? a) ,(? number? b)) (reify (= a b))]
    [`(< ,(? number? a) ,(? number? b)) (reify (< a b))]
    [`(> ,(? number? a) ,(? number? b)) (reify (> a b))]
    [`(== (PI) ,(? number?)) '()]
    [`(== ,(? number? a) ,b) `((== ,b ,a))] ; canonicalize
    [`(== (+ ,(? number? a) ,b) ,c) `((== (+ ,b ,a) ,c))] ; canonicalize
    [`(== (- ,(? number? a) ,b) ,c) `((== (neg (- ,b ,a)) ,c))] ; canonicalize
    [`(<= ,a ,b) (list `(< ,a ,b) `(== ,a ,b))] ; canonicalize

    [`(== (cbrt ,a) 0) (list `(== ,a 0))]
    [`(== (sqrt ,a) 0) (list `(== ,a 0))]
    [`(== (neg ,a) 0) (list `(== ,a 0))]
    [`(== (fabs ,a) 0) (list `(== ,a 0))]
    [`(== (* ,a ,b) 0) (list `(== ,a 0) `(== ,b 0))]
    [`(== (/ ,a ,b) 0) (list `(== ,a 0))]
    [`(== (pow ,a ,b) 0) (list `(and (== ,a 0) (> ,b 0)))]

    [`(== (fabs ,a) 1) (list `(== ,a 1) `(== ,a -1))]
    [`(== (+ ,x 1) 0) (list `(== ,x -1))]
    [`(== (- ,a 1) 0) (list `(== ,a 1))]
    [`(== (* ,a ,a) 1) (list `(== (fabs ,a) 1))]

    [`(< (* ,a ,a) 0) '()]
    [`(< (sqrt ,a) 0) '()]
    [`(,(or '< '==) (cosh ,a) ,(? (conjoin number? (curryr < 1)))) '()]
    [`(,(or '< '==) (exp ,a) ,(? (conjoin number? (curryr <= 0)))) '()]
    [`(,(or '< '==) (* ,a ,a) ,(? (conjoin number? (curryr < 0)))) '()]
    [`(,(or '< '==) (fabs ,a) ,(? (conjoin number? (curryr < 0)))) '()]

    [`(< (/ 1 ,a) 0) (list `(< ,a 0))]
    [`(< (neg ,a) 0) (list `(> ,a 0))]
    [`(< (* 2 ,a) ,(? number? b)) (list `(< ,a ,(/ b 2)))]
    [`(< (+ 1 ,a) 0) (list `(< ,a -1))]
    [`(< (/ ,x 2) 0) (list `(< ,x 0))]
    [`(< (+ ,x 1) 0) (list `(< ,x -1))]
    [`(< (- ,a 1) ,(? number? b)) (list `(< ,a ,(+ b 1)))]
    [`(< (- 1 ,x) 0) (list `(< 1 ,x))]

    [`(< (* ,a ,a) 1) (list `(< (fabs ,a) 1))]
    [`(< 1 (* ,a ,a)) (list `(< 1 (fabs ,a)))]

    [`(== (+ ,x (sqrt (+ (* ,x ,x) 1))) 0) '()]
    [`(== (+ ,x (sqrt (- (* ,x ,x) 1))) 0) '()]
    [`(< (+ ,x (sqrt (+ (* ,x ,x) 1))) 0) '()]
    [`(< (+ ,x (sqrt (- (* ,x ,x) 1))) 0) (list `(<= x -1))]

    [`(< 1 (fabs (,(or 'cos 'sin) x))) '()]

    [`(== (/ (+ 1 ,x) (- 1 ,x)) 0) (list `(== ,x -1))]
    [`(< (/ (+ 1 ,x) (- 1 ,x)) 0) (list `(< 1 (fabs x)))]

    [`(== (+ (cos ,a) (cos ,b)) 0) (list `(== (cos (/ (+ ,a ,b) 2)) 0) `(== (cos (/ (- ,a ,b) 2)) 0))]
    [`(== (cos (* 2 ,a)) 0) (list `(== (tan ,a) 1) `(== (tan ,a) -1))]
    [`(== (tan ,a) 0) (list `(== (sin ,a) 0))]

    [`(even-fraction? (neg ,b)) (list `(even-fraction? ,b))]
    [`(even-fraction? (+ ,b 1)) (list `(even-fraction? ,b))]
    [`(even-fraction? ,(? rational? a))
     (if (even? (denominator a))
         '((TRUE))
         '())]

    [`(or ,sub ...) sub]
    [`(and ,sub ...)
     (define subs (map (compose simplify-conditions list) sub))
     (define conjunctions (apply cartesian-product subs))
     (for/list ([conj (in-list conjunctions)])
       (match (set-remove conj '(TRUE))
         ['() '(TRUE)]
         [(list a) a]
         [(list as ...) (cons 'and as)]))]
    [x (list x)]))

(define (simplify-conditions xs)
  (define simple1
    (apply append
           (for/list ([x (remove-duplicates xs)])
             (simplify-condition x))))
  (if (equal? simple1 xs)
      xs
      (simplify-conditions simple1)))

(define soundness-proofs
  '((pow-plus (implies (< b -1) (< b 0)))
    (pow-sqr (implies (even-fraction? (* 2 b)) (even-fraction? b)))
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
             (implies (even-fraction? (- b c)) (or (even-fraction? b) (even-fraction? c))))
    (pow-prod-up (implies (< (+ b c) 0) (or (< b 0) (< c 0)))
                 (implies (even-fraction? (+ b c)) (or (even-fraction? b) (even-fraction? c))))
    (pow-prod-down (implies (< (* b c) 0) (or (< b 0) (< c 0))))
    ;; If y / 2 is an even fraction, y cannot have a factor of 2 in the numerator
    (sqrt-pow1 (implies (and (< x 0) (even-fraction? (/ y 2)))
                        (or (and (< x 0) (even-fraction? y)) (< (pow x y) 0))))
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
      (define lhs-bad (execute-proof proof (validity-conditions (rule-input rule))))
      (define rhs-bad (execute-proof proof (validity-conditions (rule-output rule))))
      (define extra (set-remove (set-subtract rhs-bad lhs-bad) '(FALSE)))
      (when (not (null? extra))
        (eprintf "Cannot prove rule ~a valid\n" (rule-name rule))
        (for ([term (in-list extra)])
          (eprintf "  ~a\n" term))
        (eprintf "  --------------------\n")
        (for ([term (in-list lhs-bad)])
          (eprintf "  ~a\n" term))
        (fail)))))

(module+ test
  (potentially-unsound))
