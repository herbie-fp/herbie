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

(define (simplify-conditions xs)
  (define simple1
    (apply
     append
     (for/list ([x (remove-duplicates xs)])
       (match x
         [`(== ,(? number? a) ,(? number? b))
          (if (= a b)
              '((TRUE))
              '())]
         [`(< ,(? number? a) ,(? number? b))
          (if (< a b)
              '((TRUE))
              '())]
         [`(> ,(? number? a) ,(? number? b))
          (if (> a b)
              '((TRUE))
              '())]
         [`(<= (/ 1 ,a) 0) (list `(<= ,a 0))]
         [`(,(or '== '<= '<) (exp ,a) 0) '()]
         [`(== (PI) 0) '()]
         [`(== (* 2 (PI)) 0) '()]
         [`(== (cbrt ,a) 0) (list `(== ,a 0))]
         [`(== (sqrt ,a) 0) (list `(== ,a 0))]
         [`(== (neg ,a) 0) (list `(== ,a 0))]
         [`(== (- 1 ,a) 0) (list `(== ,a 1))]
         [`(== (fabs ,a) 0) (list `(== ,a 0))]
         [`(== (fabs ,a) 1) (list `(== ,a 1) `(== ,a -1))]
         [`(== (pow ,a 3) 0) (list `(== ,a 0))]
         [`(== (* ,a ,b) 0) (list `(== ,a 0) `(== ,b 0))]
         [`(== (cos (neg ,a)) 0) (list `(== (cos ,a) 0))]
         [`(== (cos (neg ,a)) -1) (list `(== (cos ,a) -1))]
         [`(== (cos (+ ,a (PI))) 0) (list `(== (cos ,a) 0))]
         [`(== ,(? number? a) ,b) `((== ,b ,a))] ; canonicalize
         [`(,(or '== '<= '<) (+ 1 (* ,a ,a)) 0) '()]
         [`(,(or '== '<= '<) (+ (* ,a ,a) 1) 0) '()]
         [`(< (* ,a ,a) 0) '()]
         [`(< (sqrt ,a) 0) '()]
         [`(< (neg ,a) 0) (list `(> ,a 0))]
         [`(== (pow ,a ,b) 0) (list `(and (== ,a 0) (> ,b 0)))]
         [`(< (* 2 ,a) 0) (list `(< ,a 0))]
         [`(<= ,a ,b) (list `(< ,a ,b) `(== ,a ,b))]
         [`(== (* ,a ,a) 1) (list `(== (fabs ,a) 1))]
         [`(< (- 1 (* ,a ,a)) 0) (list `(< 1 (fabs ,a)))]
         [`(< (- (* ,a ,a) 1) 0) (list `(< (fabs ,a) 1))]
         [`(== (+ (* ,a ,a) (,(or '- '+) (* ,b ,b) (* ,a ,b))) 0) (list `(and (== ,a 0) (== ,b 0)))]
         [`(< 1 (fabs (neg x))) (list `(< 1 (fabs x)))]
         [`(== (+ ,x (sqrt (+ (* ,x ,x) 1))) 0) '()]
         [`(== (+ ,x (sqrt (- (* ,x ,x) 1))) 0) '()]
         [`(< (+ ,x (sqrt (+ (* ,x ,x) 1))) 0) '()]
         [`(< 1 (fabs (,(or 'cos 'sin) x))) '()]
         [`(< (+ ,x (sqrt (- (* ,x ,x) 1))) 0) (list `(<= x -1))]
         [`(== (/ (+ 1 ,x) (- 1 ,x)) 0) (list `(== ,x -1))]
         [`(< (/ (+ 1 ,x) (- 1 ,x)) 0) (list `(< 1 (fabs x)))]
         [`(,(or '< '== '<=) (fabs ,a) ,(? (conjoin number? negative?) b)) '()]
         [`(== (/ ,a ,b) 0) (list `(== ,a 0))]

         [`(< (/ ,x 2) 0) (list `(< ,x 0))]
         [`(< (+ ,x 1) 0) (list `(< ,x -1))]
         [`(== (+ ,x 1) 0) (list `(== ,x -1))]
         [`(== (+ 1 ,x) 0) (list `(== ,x -1))]
         [`(,(or '< '==) (cosh ,x) 0) '()]
         [`(,(or '< '==) (cosh ,x) -1) '()]
         [`(== (exp ,a) -1) '()]
         [`(== (exp ,a) 0) '()]
         [`(== (+ (exp ,a) (exp ,b)) 0) '()]

         [`(== (* (tan ,x) (tan ,y)) 1) (list `(== (cos (+ ,x ,y)) 0))]

         [`(== (+ (cos ,a) (cos ,b)) 0)
          (list `(== (cos (/ (+ ,a ,b) 2)) 0) `(== (cos (/ (- ,a ,b) 2)) 0))]

         [`(== (cos (* 2 ,a)) 0) (list `(== (tan ,a) 1) `(== (tan ,a) -1))]
         [`(== (cos (acos ,a)) 0) (list `(== ,a 0))]
         [`(== (cos (asin ,a)) 0) (list `(== ,a 1) `(== ,a -1))]

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
         [x (list x)]))))
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
    (acosh-def (implies (< x 1) (or (< x -1) (== x -1) (< (fabs x) 1)))
               (rewrite (fabs (fabs x)) (fabs x)))
    (acosh-def-rev (implies (< x 1) (or (< x -1) (== x -1) (< (fabs x) 1)))
                   (rewrite (fabs (fabs x)) (fabs x)))
    (sqrt-undiv (implies (< (/ x y) 0) (or (< x 0) (< y 0))))
    (sqrt-unprod (implies (< (* x y) 0) (or (< x 0) (< y 0))))
    (sum-log (implies (< (* x y) 0) (or (< x 0) (< y 0))))
    (diff-log (implies (< (/ x y) 0) (or (< x 0) (< y 0))))
    (exp-to-pow (implies (and a b) a))
    (sinh-acosh (implies (< (fabs x) 1) (< x 1)))
    (tanh-acosh (implies (< (fabs x) 1) (< x 1)) (implies (== x 0) (< x 1)))
    (hang-p0-tan (implies (== (cos (/ a 2)) 0) (== (sin a) 0)))
    (hang-m0-tan (implies (== (cos (/ a 2)) 0) (== (sin a) 0)) (rewrite (sin (neg a)) (neg (sin a))))
    (sqrt-pow2 (implies (and a b) a))
    (pow-div (implies (< (- b c) 0) (or (< b 0) (> c 0)))
             (implies (even-fraction? (- b c)) (or (even-fraction? b) (even-fraction? c))))
    (pow-prod-up (implies (< (+ b c) 0) (or (< b 0) (< c 0)))
                 (implies (even-fraction? (+ b c)) (or (even-fraction? b) (even-fraction? c))))
    (pow-prod-down (implies (< (* b c) 0) (or (< b 0) (< c 0))))))

(define (execute-proof-step step term)
  (match step
    [`(,(or 'implies 'rewrite) ,a ,b)
     (define matches
       (for/list ([subexpr (in-list (all-subexpressions term))]
                  #:when (pattern-match a subexpr))
         (cons subexpr (pattern-substitute b (pattern-match a subexpr)))))
     (for/fold ([term term]) ([(from to) (in-dict matches)])
       (replace-expression term from to))]))

(define (execute-proof proof terms)
  (for/fold ([terms (simplify-conditions terms)]) ([step (in-list proof)])
    (simplify-conditions (map (curry execute-proof-step step) terms))))

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
        (for ([term (in-list lhs-bad)])
          (eprintf "  ~a\n" term))
        (eprintf "  --------------------\n")
        (for ([term (in-list extra)])
          (eprintf "  ~a\n" term))
        (fail)))))

(module+ test
  (potentially-unsound))
