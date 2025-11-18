#lang racket

;; Arithmetic identities for rewriting programs.

(require "../utils/common.rkt"
         "../syntax/syntax.rkt")

(provide *rules*
         add-unsound
         (struct-out rule))

;; A rule represents "find-and-replacing" `input` by `output`. Both
;; are patterns, meaning that symbols represent pattern variables.
(struct rule (name input output tags)
  #:methods gen:custom-write
  [(define (write-proc rule port mode)
     (fprintf port "#<rule ~a>" (rule-name rule)))])

(define *all-rules* '())

(define (rule-enabled? rule)
  (ormap (curry flag-set? 'rules) (rule-tags rule)))

(define (*rules*)
  (filter rule-enabled? *all-rules*))

(define-syntax-rule (define-rule rname group input output)
  (set! *all-rules* (cons (rule 'rname 'input 'output '(group)) *all-rules*)))

(define-syntax-rule (define-rules group
                      [rname input output flags ...] ...)
  (begin
    (define-rule rname group input output flags ...) ...))

; Commutativity
(define-rules arithmetic
  [+-commutative (+ a b) (+ b a)]
  [*-commutative (* a b) (* b a)])

; Associativity
(define-rules arithmetic
  [associate-+r+ (+ a (+ b c)) (+ (+ a b) c)]
  [associate-+l+ (+ (+ a b) c) (+ a (+ b c))]
  [associate-+r- (+ a (- b c)) (- (+ a b) c)]
  [associate-+l- (+ (- a b) c) (- a (- b c))]
  [associate--r+ (- a (+ b c)) (- (- a b) c)]
  [associate--l+ (- (+ a b) c) (+ a (- b c))]
  [associate--l- (- (- a b) c) (- a (+ b c))]
  [associate--r- (- a (- b c)) (+ (- a b) c)]
  [associate-*r* (* a (* b c)) (* (* a b) c)]
  [associate-*l* (* (* a b) c) (* a (* b c))]
  [associate-*r/ (* a (/ b c)) (/ (* a b) c)]
  [associate-*l/ (* (/ a b) c) (/ (* a c) b)]
  [associate-/r* (/ a (* b c)) (/ (/ a b) c)]
  [associate-/r/ (/ a (/ b c)) (* (/ a b) c)]
  [associate-/l/ (/ (/ b c) a) (/ b (* c a))]
  [associate-/l* (/ (* b c) a) (* b (/ c a))])

; Identity
(define-rules arithmetic
  [remove-double-div (/ 1 (/ 1 a)) a]
  [rgt-mult-inverse (* a (/ 1 a)) 1]
  [lft-mult-inverse (* (/ 1 a) a) 1]
  [+-inverses (- a a) 0]
  [div0 (/ 0 a) 0]
  [mul0-lft (* 0 a) 0]
  [mul0-rgt (* a 0) 0]
  [*-inverses (/ a a) 1]
  [+-lft-identity (+ 0 a) a]
  [+-rgt-identity (+ a 0) a]
  [--rgt-identity (- a 0) a]
  [sub0-neg (- 0 a) (neg a)]
  [remove-double-neg (neg (neg a)) a]
  [*-lft-identity (* 1 a) a]
  [*-rgt-identity (* a 1) a]
  [/-rgt-identity (/ a 1) a]
  [mul-1-neg (* -1 a) (neg a)])

; Counting
(define-rules arithmetic
  [count-2 (+ x x) (* 2 x)]
  [2-split 2 (+ 1 1)]
  [count-2-rev (* 2 x) (+ x x)])

; Distributivity
(define-rules arithmetic
  [distribute-lft-in (* a (+ b c)) (+ (* a b) (* a c))]
  [distribute-rgt-in (* a (+ b c)) (+ (* b a) (* c a))]
  [distribute-lft-out (+ (* a b) (* a c)) (* a (+ b c))]
  [distribute-lft-out-- (- (* a b) (* a c)) (* a (- b c))]
  [distribute-rgt-out (+ (* b a) (* c a)) (* a (+ b c))]
  [distribute-rgt-out-- (- (* b a) (* c a)) (* a (- b c))]
  [distribute-lft1-in (+ (* b a) a) (* (+ b 1) a)]
  [distribute-rgt1-in (+ a (* c a)) (* (+ c 1) a)])

; Safe Distributiviity
(define-rules arithmetic
  [distribute-lft-neg-in (neg (* a b)) (* (neg a) b)]
  [distribute-rgt-neg-in (neg (* a b)) (* a (neg b))]
  [distribute-lft-neg-out (* (neg a) b) (neg (* a b))]
  [distribute-rgt-neg-out (* a (neg b)) (neg (* a b))]
  [distribute-neg-in (neg (+ a b)) (+ (neg a) (neg b))]
  [distribute-neg-out (+ (neg a) (neg b)) (neg (+ a b))]
  [distribute-frac-neg (/ (neg a) b) (neg (/ a b))]
  [distribute-frac-neg2 (/ a (neg b)) (neg (/ a b))]
  [distribute-neg-frac (neg (/ a b)) (/ (neg a) b)]
  [distribute-neg-frac2 (neg (/ a b)) (/ a (neg b))]
  [fp-cancel-sign-sub (- a (* (neg b) c)) (+ a (* b c))]
  [fp-cancel-sub-sign (+ a (* (neg b) c)) (- a (* b c))]
  [fp-cancel-sign-sub-inv (+ a (* b c)) (- a (* (neg b) c))]
  [fp-cancel-sub-sign-inv (- a (* b c)) (+ a (* (neg b) c))])

(define-rules arithmetic
  [sub-flip (- a b) (+ a (neg b))]
  [sub-flip-reverse (+ a (neg b)) (- a b)]
  [sub-negate (neg (- b a)) (- a b)]
  [sub-negate-rev (- a b) (neg (- b a))]
  [add-flip (+ a b) (- a (neg b))]
  [add-flip-rev (- a (neg b)) (+ a b)])

; Difference of squares
(define-rules polynomials
  [swap-sqr (* (* a b) (* a b)) (* (* a a) (* b b))]
  [unswap-sqr (* (* a a) (* b b)) (* (* a b) (* a b))]
  [difference-of-squares (- (* a a) (* b b)) (* (+ a b) (- a b))]
  [difference-of-sqr-1 (- (* a a) 1) (* (+ a 1) (- a 1))]
  [difference-of-sqr--1 (+ (* a a) -1) (* (+ a 1) (- a 1))]
  [pow-sqr (* (pow a b) (pow a b)) (pow a (* 2 b))]
  [sum-square-pow (pow (+ a b) 2) (+ (+ (pow a 2) (* 2 (* a b))) (pow b 2))]
  [sub-square-pow (pow (- a b) 2) (+ (- (pow a 2) (* 2 (* a b))) (pow b 2))]
  [sum-square-pow-rev (+ (+ (pow a 2) (* 2 (* a b))) (pow b 2)) (pow (+ a b) 2)]
  [sub-square-pow-rev (+ (- (pow a 2) (* 2 (* a b))) (pow b 2)) (pow (- a b) 2)]
  [difference-of-sqr-1-rev (* (+ a 1) (- a 1)) (- (* a a) 1)]
  [difference-of-sqr--1-rev (* (+ a 1) (- a 1)) (+ (* a a) -1)]
  [difference-of-squares-rev (* (+ a b) (- a b)) (- (* a a) (* b b))])

; Mul/div flip
(define-rules arithmetic
  [mult-flip (/ a b) (* a (/ 1 b))]
  [mult-flip-rev (* a (/ 1 b)) (/ a b)]
  [div-flip (/ a b) (sound-/ 1 (sound-/ b a 0) (/ a b))]
  [div-flip-rev (/ 1 (/ b a)) (/ a b)])

; Fractions
(define-rules arithmetic
  #;[sum-to-mult (+ a b) (* (+ 1 (/ b a)) a) #:unsound] ; unsound @ a = 0, b = 1
  [sum-to-mult-rev (* (+ 1 (/ b a)) a) (+ a b)]
  #;[sub-to-mult (- a b) (* (- 1 (/ b a)) a) #:unsound] ; unsound @ a = 0, b = 1
  [sub-to-mult-rev (* (- 1 (/ b a)) a) (- a b)]
  [add-to-fraction (+ c (/ b a)) (/ (+ (* c a) b) a)]
  [add-to-fraction-rev (/ (+ (* c a) b) a) (+ c (/ b a))]
  [sub-to-fraction (- c (/ b a)) (/ (- (* c a) b) a)]
  [sub-to-fraction-rev (/ (- (* c a) b) a) (- c (/ b a))]
  [common-denominator (+ (/ a b) (/ c d)) (/ (+ (* a d) (* c b)) (* b d))])

(define-rules polynomials
  #;[sqr-pow (pow a b) (* (pow a (/ b 2)) (pow a (/ b 2))) #:unsound] ; unsound @ a = -1, b = 1
  [flip-+ (+ (sqrt a) (sqrt b)) (sound-/ (- a b) (- (sqrt a) (sqrt b)) (+ (sqrt a) (sqrt b)))]
  [flip-- (- (sqrt a) (sqrt b)) (sound-/ (- a b) (+ (sqrt a) (sqrt b)) (- (sqrt a) (sqrt b)))])

; Difference of cubes
(define-rules polynomials
  [sum-cubes (+ (pow a 3) (pow b 3)) (* (+ (* a a) (- (* b b) (* a b))) (+ a b))]
  [difference-cubes (- (pow a 3) (pow b 3)) (* (+ (* a a) (+ (* b b) (* a b))) (- a b))]
  [difference-cubes-rev (* (+ (* a a) (+ (* b b) (* a b))) (- a b)) (- (pow a 3) (pow b 3))]
  [sum-cubes-rev (* (+ (* a a) (- (* b b) (* a b))) (+ a b)) (+ (pow a 3) (pow b 3))])

(define-rules polynomials
  [flip3-+
   (+ (cbrt a) (cbrt b))
   (sound-/ (+ a b)
            (+ (* (cbrt a) (cbrt a)) (- (* (cbrt b) (cbrt b)) (* (cbrt a) (cbrt b))))
            (+ (cbrt a) (cbrt b)))]
  [flip3--
   (- (cbrt a) (cbrt b))
   (sound-/ (- a b)
            (+ (* (cbrt a) (cbrt a)) (+ (* (cbrt b) (cbrt b)) (* (cbrt a) (cbrt b))))
            (- (cbrt a) (cbrt b)))])

; Dealing with fractions
(define-rules fractions
  [div-sub (/ (- a b) c) (- (/ a c) (/ b c))]
  [times-frac (/ (* a b) (* c d)) (* (/ a c) (/ b d))]
  [div-add (/ (+ a b) c) (+ (/ a c) (/ b c))]
  [div-add-rev (+ (/ a c) (/ b c)) (/ (+ a b) c)]
  [sub-div (- (/ a c) (/ b c)) (/ (- a b) c)]
  [frac-add (+ (/ a b) (/ c d)) (/ (+ (* a d) (* b c)) (* b d))]
  [frac-sub (- (/ a b) (/ c d)) (/ (- (* a d) (* b c)) (* b d))]
  [frac-times (* (/ a b) (/ c d)) (/ (* a c) (* b d))]
  [frac-2neg (/ a b) (/ (neg a) (neg b))]
  [frac-2neg-rev (/ (neg a) (neg b)) (/ a b)])

; Square root
(define-rules arithmetic
  [rem-square-sqrt (* (sqrt x) (sqrt x)) x]
  [rem-sqrt-square (sqrt (* x x)) (fabs x)]
  [rem-sqrt-square-rev (fabs x) (sqrt (* x x))]
  [sqr-neg (* (neg x) (neg x)) (* x x)]
  [sqr-abs (* (fabs x) (fabs x)) (* x x)]
  [sqr-abs-rev (* x x) (* (fabs x) (fabs x))]
  [sqr-neg-rev (* x x) (* (neg x) (neg x))]
  [sqrt-cbrt (sqrt (cbrt x)) (cbrt (sqrt x))]
  [cbrt-sqrt (cbrt (sqrt x)) (sqrt (cbrt x))])

; Absolute value
(define-rules arithmetic
  [fabs-fabs (fabs (fabs x)) (fabs x)]
  [fabs-sub (fabs (- a b)) (fabs (- b a))]
  [fabs-add (fabs (+ (fabs a) (fabs b))) (+ (fabs a) (fabs b))]
  [fabs-neg (fabs (neg x)) (fabs x)]
  [fabs-sqr (fabs (* x x)) (* x x)]
  [fabs-mul (fabs (* a b)) (* (fabs a) (fabs b))]
  [fabs-div (fabs (/ a b)) (/ (fabs a) (fabs b))]
  [neg-fabs (fabs x) (fabs (neg x))]
  [mul-fabs (* (fabs a) (fabs b)) (fabs (* a b))]
  [div-fabs (/ (fabs a) (fabs b)) (fabs (/ a b))]
  [sqrt-fabs (fabs (sqrt a)) (sqrt a)]
  [sqrt-fabs-rev (sqrt a) (fabs (sqrt a))]
  [fabs-lhs-div (/ (fabs x) x) (copysign 1 x)]
  [fabs-rhs-div (/ x (fabs x)) (copysign 1 x)]
  [fabs-cbrt (fabs (/ (cbrt a) a)) (/ (cbrt a) a)]
  [fabs-cbrt-rev (/ (cbrt a) a) (fabs (/ (cbrt a) a))])

; Copysign
(define-rules arithmetic
  [copysign-neg (copysign a (neg b)) (neg (copysign a b))]
  [neg-copysign (neg (copysign a b)) (copysign a (neg b))]
  [copysign-other-neg (copysign (neg a) b) (copysign a b)]
  [copysign-fabs (copysign a (fabs b)) (fabs a)]
  [copysign-other-fabs (copysign (fabs a) b) (copysign a b)]
  [fabs-copysign (fabs (copysign a b)) (fabs a)])

; Square root
(define-rules arithmetic
  [sqrt-pow2 (pow (sqrt x) y) (pow x (/ y 2))]
  [sqrt-unprod (* (sqrt x) (sqrt y)) (sqrt (* x y))]
  [sqrt-undiv (/ (sqrt x) (sqrt y)) (sqrt (/ x y))])

(define-rules arithmetic
  [sqrt-prod (sqrt (* x y)) (* (sqrt (fabs x)) (sqrt (fabs y)))]
  [sqrt-div (sqrt (/ x y)) (/ (sqrt (fabs x)) (sqrt (fabs y)))]
  [add-sqr-sqrt x (copysign (* (sqrt (fabs x)) (sqrt (fabs x))) x)])

; Cubing
(define-rules arithmetic
  [rem-cube-cbrt (pow (cbrt x) 3) x]
  [rem-cbrt-cube (cbrt (pow x 3)) x]
  [rem-3cbrt-lft (* (* (cbrt x) (cbrt x)) (cbrt x)) x]
  [rem-3cbrt-rft (* (cbrt x) (* (cbrt x) (cbrt x))) x]
  [cube-neg (pow (neg x) 3) (neg (pow x 3))]
  [cube-neg-rev (neg (pow x 3)) (pow (neg x) 3)]
  [cube-prod (pow (* x y) 3) (* (pow x 3) (pow y 3))]
  [cube-div (pow (/ x y) 3) (/ (pow x 3) (pow y 3))]
  [cube-mult (pow x 3) (* x (* x x))]
  [cube-prod-rev (* (pow x 3) (pow y 3)) (pow (* x y) 3)]
  [cube-div-rev (/ (pow x 3) (pow y 3)) (pow (/ x y) 3)])

; Cube root
(define-rules arithmetic
  [cbrt-prod (cbrt (* x y)) (* (cbrt x) (cbrt y))]
  [cbrt-div (cbrt (/ x y)) (/ (cbrt x) (cbrt y))]
  [cbrt-unprod (* (cbrt x) (cbrt y)) (cbrt (* x y))]
  [cbrt-undiv (/ (cbrt x) (cbrt y)) (cbrt (/ x y))]
  [pow-cbrt (pow (cbrt x) y) (pow x (/ y 3))]
  [cbrt-pow (cbrt (pow x y)) (pow x (/ y 3))]
  [add-cube-cbrt x (* (* (cbrt x) (cbrt x)) (cbrt x))]
  [add-cbrt-cube x (cbrt (* (* x x) x))]
  [cube-unmult (* x (* x x)) (pow x 3)]
  [cbrt-neg (cbrt (neg x)) (neg (cbrt x))]
  [cbrt-neg-rev (neg (cbrt x)) (cbrt (neg x))]
  [cbrt-fabs (cbrt (fabs x)) (fabs (cbrt x))]
  [cbrt-fabs-rev (fabs (cbrt x)) (cbrt (fabs x))]
  [cbrt-div-cbrt (/ (cbrt x) (fabs (cbrt x))) (copysign 1 x)]
  [cbrt-div-cbrt2 (/ (fabs (cbrt x)) (cbrt x)) (copysign 1 x)])

; Min and max
(define-rules arithmetic
  [fmin-swap (fmin a b) (fmin b a)]
  [fmax-swap (fmax a b) (fmax b a)])

; Exponentials
(define-rules exponents
  [add-log-exp x (log (exp x))]
  #;[add-exp-log x (exp (log x)) #:unsound] ; unsound @ x = -1
  [rem-exp-log (exp (log x)) x]
  [rem-log-exp (log (exp x)) x])

(define-rules exponents
  [exp-0 (exp 0) 1]
  [exp-1-e (exp 1) (E)]
  [1-exp 1 (exp 0)]
  [e-exp-1 (E) (exp 1)]
  [exp-fabs (exp x) (fabs (exp x))]
  [fabs-exp (fabs (exp x)) (exp x)])

(define-rules exponents
  [exp-sum (exp (+ a b)) (* (exp a) (exp b))]
  [exp-neg (exp (neg a)) (/ 1 (exp a))]
  [exp-diff (exp (- a b)) (/ (exp a) (exp b))]
  [prod-exp (* (exp a) (exp b)) (exp (+ a b))]
  [rec-exp (/ 1 (exp a)) (exp (neg a))]
  [div-exp (/ (exp a) (exp b)) (exp (- a b))]
  [exp-prod (exp (* a b)) (pow (exp a) b)]
  [exp-sqrt (exp (/ a 2)) (sqrt (exp a))]
  [exp-cbrt (exp (/ a 3)) (cbrt (exp a))]
  [exp-lft-sqr (exp (* a 2)) (* (exp a) (exp a))]
  [exp-lft-cube (exp (* a 3)) (pow (exp a) 3)]
  [exp-cbrt-rev (cbrt (exp a)) (exp (/ a 3))]
  [exp-lft-cube-rev (pow (exp a) 3) (exp (* a 3))]
  [exp-sqrt-rev (sqrt (exp a)) (exp (/ a 2))]
  [exp-lft-sqr-rev (* (exp a) (exp a)) (exp (* a 2))])

; Powers
(define-rules exponents
  [unpow-1 (pow a -1) (/ 1 a)]
  [unpow1 (pow a 1) a]
  [unpow0 (pow a 0) 1]
  [pow-base-1 (pow 1 a) 1]
  [pow1 a (pow a 1)]
  [unpow1/2 (pow a 1/2) (sqrt a)]
  [unpow2 (pow a 2) (* a a)]
  [unpow3 (pow a 3) (* (* a a) a)]
  [unpow1/3 (pow a 1/3) (cbrt a)]
  [pow-base-0 (pow 0 a) 0]
  [inv-pow (/ 1 a) (pow a -1)])

(define-rules exponents
  [pow1/2 (sqrt a) (pow a 1/2)]
  [pow2 (* a a) (pow a 2)]
  [pow1/3 (cbrt a) (pow a 1/3)]
  [pow3 (* (* a a) a) (pow a 3)])

(define-rules exponents
  [exp-to-pow (exp (* (log a) b)) (pow a b)]
  [pow-plus (* (pow a b) a) (pow a (+ b 1))]
  [pow-exp (pow (exp a) b) (exp (* a b))]
  [pow-prod-down (* (pow b a) (pow c a)) (pow (* b c) a)]
  [pow-prod-up (* (pow a b) (pow a c)) (pow a (+ b c))]
  [pow-flip (/ 1 (pow a b)) (pow a (neg b))]
  [pow-div (/ (pow a b) (pow a c)) (pow a (- b c))])

(define-rules exponents
  [pow-plus-rev (pow a (+ b 1)) (* (sound-pow a b 1) a)]
  [pow-neg (pow a (neg b)) (sound-/ 1 (sound-pow a b 0) 0)])

(define-rules exponents
  #;[pow-to-exp (pow a b) (exp (* (log a) b)) #:unsound] ; unsound @ a = -1, b = 1
  #;[pow-add (pow a (+ b c)) (* (pow a b) (pow a c)) #:unsound] ; unsound @ a = -1, b = c = 1/2
  #;[pow-sub (pow a (- b c)) (/ (pow a b) (pow a c)) #:unsound] ; unsound @ a = -1, b = c = 1/2
  #;
  [unpow-prod-down (pow (* b c) a) (* (pow b a) (pow c a)) #:unsound]) ; unsound @ a = 1/2, b = c = -1

; Logarithms
(define-rules exponents
  [log-rec (log (/ 1 a)) (neg (log a))]
  [log-E (log (E)) 1]
  [log-pow-rev (* b (log a)) (log (pow a b))])

(define-rules exponents
  [log-prod (log (* a b)) (+ (log (fabs a)) (log (fabs b)))]
  [log-div (log (/ a b)) (- (log (fabs a)) (log (fabs b)))]
  [log-pow (log (pow a b)) (* b (sound-log (fabs a) 0))])

(define-rules exponents
  [sum-log (+ (log a) (log b)) (log (* a b))]
  [diff-log (- (log a) (log b)) (log (/ a b))]
  [neg-log (neg (log a)) (log (/ 1 a))])

; Trigonometry
(define-rules trigonometry
  [sin-0 (sin 0) 0]
  [cos-0 (cos 0) 1]
  [tan-0 (tan 0) 0])

(define-rules trigonometry
  [sin-neg (sin (neg x)) (neg (sin x))]
  [cos-neg (cos (neg x)) (cos x)]
  [cos-fabs (cos (fabs x)) (cos x)]
  [tan-neg (tan (neg x)) (neg (tan x))]
  [cos-neg-rev (cos x) (cos (neg x))]
  [cos-fabs-rev (cos x) (cos (fabs x))]
  [sin-neg-rev (neg (sin x)) (sin (neg x))]
  [tan-neg-rev (neg (tan x)) (tan (neg x))])

(define-rules trignometry
  [sqr-sin-b (* (sin x) (sin x)) (- 1 (* (cos x) (cos x)))]
  [sqr-cos-b (* (cos x) (cos x)) (- 1 (* (sin x) (sin x)))]
  [sqr-cos-b-rev (- 1 (* (sin x) (sin x))) (* (cos x) (cos x))]
  [sqr-sin-b-rev (- 1 (* (cos x) (cos x))) (* (sin x) (sin x))])

(define-rules trigonometry
  [sin-asin (sin (asin x)) x]
  [cos-acos (cos (acos x)) x]
  [tan-atan (tan (atan x)) x]
  [atan-tan (atan (tan x)) (remainder x (PI))]
  [asin-sin (asin (sin x)) (- (fabs (remainder (+ x (/ (PI) 2)) (* 2 (PI)))) (/ (PI) 2))]
  [acos-cos (acos (cos x)) (fabs (remainder x (* 2 (PI))))]
  [acos-cos-rev (fabs (remainder x (* 2 (PI)))) (acos (cos x))]
  [asin-sin-rev (- (fabs (remainder (+ x (/ (PI) 2)) (* 2 (PI)))) (/ (PI) 2)) (asin (sin x))])

(define-rules trigonometry
  [cos-sin-sum (+ (* (cos a) (cos a)) (* (sin a) (sin a))) 1]
  [1-sub-cos (- 1 (* (cos a) (cos a))) (* (sin a) (sin a))]
  [1-sub-sin (- 1 (* (sin a) (sin a))) (* (cos a) (cos a))]
  [-1-add-cos (+ (* (cos a) (cos a)) -1) (neg (* (sin a) (sin a)))]
  [-1-add-sin (+ (* (sin a) (sin a)) -1) (neg (* (cos a) (cos a)))]
  [sub-1-cos (- (* (cos a) (cos a)) 1) (neg (* (sin a) (sin a)))]
  [sub-1-sin (- (* (sin a) (sin a)) 1) (neg (* (cos a) (cos a)))]
  [sin-PI/6 (sin (/ (PI) 6)) 1/2]
  [sin-PI/4 (sin (/ (PI) 4)) (/ (sqrt 2) 2)]
  [sin-PI/3 (sin (/ (PI) 3)) (/ (sqrt 3) 2)]
  [sin-PI/2 (sin (/ (PI) 2)) 1]
  [sin-PI (sin (PI)) 0]
  [sin-+PI (sin (+ x (PI))) (neg (sin x))]
  [sin-+PI/2 (sin (+ x (/ (PI) 2))) (cos x)]
  [cos-PI/6 (cos (/ (PI) 6)) (/ (sqrt 3) 2)]
  [cos-PI/4 (cos (/ (PI) 4)) (/ (sqrt 2) 2)]
  [cos-PI/3 (cos (/ (PI) 3)) 1/2]
  [cos-PI/2 (cos (/ (PI) 2)) 0]
  [cos-PI (cos (PI)) -1]
  [cos-+PI (cos (+ x (PI))) (neg (cos x))]
  [cos-+PI/2 (cos (+ x (/ (PI) 2))) (neg (sin x))]
  [tan-PI/6 (tan (/ (PI) 6)) (/ 1 (sqrt 3))]
  [tan-PI/4 (tan (/ (PI) 4)) 1]
  [tan-PI/3 (tan (/ (PI) 3)) (sqrt 3)]
  [tan-PI (tan (PI)) 0]
  [tan-+PI (tan (+ x (PI))) (tan x)]
  [hang-0p-tan (/ (sin a) (+ 1 (cos a))) (tan (/ a 2))]
  [hang-0m-tan (/ (neg (sin a)) (+ 1 (cos a))) (tan (/ (neg a) 2))]
  [hang-p0-tan (/ (- 1 (cos a)) (sin a)) (tan (/ a 2))]
  [hang-m0-tan (/ (- 1 (cos a)) (neg (sin a))) (tan (/ (neg a) 2))]
  [hang-p-tan (/ (+ (sin a) (sin b)) (+ (cos a) (cos b))) (tan (/ (+ a b) 2))]
  [hang-m-tan (/ (- (sin a) (sin b)) (+ (cos a) (cos b))) (tan (/ (- a b) 2))])

(define-rules trigonometry
  [1-sub-sin-rev (* (cos a) (cos a)) (- 1 (* (sin a) (sin a)))]
  [hang-0m-tan-rev (tan (/ (neg a) 2)) (/ (neg (sin a)) (+ 1 (cos a)))]
  [hang-0p-tan-rev (tan (/ a 2)) (/ (sin a) (+ 1 (cos a)))]
  [tan-+PI-rev (tan x) (tan (+ x (PI)))]
  [cos-+PI/2-rev (neg (sin x)) (cos (+ x (/ (PI) 2)))]
  [sin-+PI/2-rev (cos x) (sin (+ x (/ (PI) 2)))]
  [sin-+PI-rev (neg (sin x)) (sin (+ x (PI)))]
  [cos-+PI-rev (neg (cos x)) (cos (+ x (PI)))]
  [neg-tan-+PI/2-rev (/ -1 (tan x)) (tan (+ x (/ (PI) 2)))]
  [tan-+PI/2-rev (/ 1 (tan x)) (tan (+ (neg x) (/ (PI) 2)))])

(define-rules trigonometry
  [sin-sum (sin (+ x y)) (+ (* (sin x) (cos y)) (* (cos x) (sin y)))]
  [cos-sum (cos (+ x y)) (- (* (cos x) (cos y)) (* (sin x) (sin y)))]
  [sin-diff (sin (- x y)) (- (* (sin x) (cos y)) (* (cos x) (sin y)))]
  [cos-diff (cos (- x y)) (+ (* (cos x) (cos y)) (* (sin x) (sin y)))]
  [sin-2 (sin (* 2 x)) (* 2 (* (sin x) (cos x)))]
  [sin-3 (sin (* 3 x)) (- (* 3 (sin x)) (* 4 (pow (sin x) 3)))]
  [2-sin (* 2 (* (sin x) (cos x))) (sin (* 2 x))]
  [3-sin (- (* 3 (sin x)) (* 4 (pow (sin x) 3))) (sin (* 3 x))]
  [cos-2 (cos (* 2 x)) (- (* (cos x) (cos x)) (* (sin x) (sin x)))]
  [cos-3 (cos (* 3 x)) (- (* 4 (pow (cos x) 3)) (* 3 (cos x)))]
  [2-cos (- (* (cos x) (cos x)) (* (sin x) (sin x))) (cos (* 2 x))]
  [3-cos (- (* 4 (pow (cos x) 3)) (* 3 (cos x))) (cos (* 3 x))])

(define-rules trigonometry
  [cos-diff-rev (+ (* (cos x) (cos y)) (* (sin x) (sin y))) (cos (- x y))]
  [sin-diff-rev (- (* (sin x) (cos y)) (* (cos x) (sin y))) (sin (- x y))]
  [sin-sum-rev (+ (* (sin x) (cos y)) (* (cos x) (sin y))) (sin (+ x y))]
  [tan-sum-rev (/ (+ (tan x) (tan y)) (- 1 (* (tan x) (tan y)))) (tan (+ x y))]
  [cos-sum-rev (- (* (cos x) (cos y)) (* (sin x) (sin y))) (cos (+ x y))])

(define-rules trigonometry
  [sqr-sin-a (* (sin x) (sin x)) (- 1/2 (* 1/2 (cos (* 2 x))))]
  [sqr-cos-a (* (cos x) (cos x)) (+ 1/2 (* 1/2 (cos (* 2 x))))]
  [diff-sin (- (sin x) (sin y)) (* 2 (* (sin (/ (- x y) 2)) (cos (/ (+ x y) 2))))]
  [diff-cos (- (cos x) (cos y)) (* -2 (* (sin (/ (- x y) 2)) (sin (/ (+ x y) 2))))]
  [sum-sin (+ (sin x) (sin y)) (* 2 (* (sin (/ (+ x y) 2)) (cos (/ (- x y) 2))))]
  [sum-cos (+ (cos x) (cos y)) (* 2 (* (cos (/ (+ x y) 2)) (cos (/ (- x y) 2))))]
  [cos-mult (* (cos x) (cos y)) (/ (+ (cos (+ x y)) (cos (- x y))) 2)]
  [sin-mult (* (sin x) (sin y)) (/ (- (cos (- x y)) (cos (+ x y))) 2)]
  [sin-cos-mult (* (sin x) (cos y)) (/ (+ (sin (- x y)) (sin (+ x y))) 2)]
  [diff-atan (- (atan x) (atan y)) (atan2 (- x y) (+ 1 (* x y)))]
  [sum-atan (+ (atan x) (atan y)) (atan2 (+ x y) (- 1 (* x y)))]
  [tan-quot (tan x) (/ (sin x) (cos x))]
  [quot-tan (/ (sin x) (cos x)) (tan x)]
  [2-tan (/ (* 2 (tan x)) (- 1 (* (tan x) (tan x)))) (tan (* 2 x))])

(define-rules trigonometry
  [diff-cos-rev (* -2 (* (sin (/ (- x y) 2)) (sin (/ (+ x y) 2)))) (- (cos x) (cos y))]
  [diff-sin-rev (* 2 (* (sin (/ (- x y) 2)) (cos (/ (+ x y) 2)))) (- (sin x) (sin y))]
  [diff-atan-rev (atan2 (- x y) (+ 1 (* x y))) (- (atan x) (atan y))]
  [sum-sin-rev (* 2 (* (sin (/ (+ x y) 2)) (cos (/ (- x y) 2)))) (+ (sin x) (sin y))]
  [sum-cos-rev (* 2 (* (cos (/ (+ x y) 2)) (cos (/ (- x y) 2)))) (+ (cos x) (cos y))]
  [sum-atan-rev (atan2 (+ x y) (- 1 (* x y))) (+ (atan x) (atan y))]
  [sqr-cos-a-rev (+ 1/2 (* 1/2 (cos (* 2 x)))) (* (cos x) (cos x))]
  [sqr-sin-a-rev (- 1/2 (* 1/2 (cos (* 2 x)))) (* (sin x) (sin x))]
  [cos-mult-rev (/ (+ (cos (+ x y)) (cos (- x y))) 2) (* (cos x) (cos y))]
  [sin-mult-rev (/ (- (cos (- x y)) (cos (+ x y))) 2) (* (sin x) (sin y))]
  [sin-cos-mult-rev (/ (+ (sin (- x y)) (sin (+ x y))) 2) (* (sin x) (cos y))])

(define-rules trigonometry
  [cos-asin (cos (asin x)) (sqrt (- 1 (* x x)))]
  [tan-asin (tan (asin x)) (/ x (sqrt (- 1 (* x x))))]
  [sin-acos (sin (acos x)) (sqrt (- 1 (* x x)))]
  [tan-acos (tan (acos x)) (/ (sqrt (- 1 (* x x))) x)]
  [sin-atan (sin (atan x)) (/ x (sqrt (+ 1 (* x x))))]
  [cos-atan (cos (atan x)) (/ 1 (sqrt (+ 1 (* x x))))]
  [asin-acos (asin x) (- (/ (PI) 2) (acos x))]
  [acos-asin (acos x) (- (/ (PI) 2) (asin x))]
  [asin-neg (asin (neg x)) (neg (asin x))]
  [acos-neg (acos (neg x)) (- (PI) (acos x))]
  [atan-neg (atan (neg x)) (neg (atan x))])

(define-rules trigonometry
  [acos-asin-rev (- (/ (PI) 2) (asin x)) (acos x)]
  [asin-acos-rev (- (/ (PI) 2) (acos x)) (asin x)]
  [asin-neg-rev (neg (asin x)) (asin (neg x))]
  [atan-neg-rev (neg (atan x)) (atan (neg x))]
  [acos-neg-rev (- (PI) (acos x)) (acos (neg x))]
  [cos-atan-rev (/ 1 (sqrt (+ 1 (* x x)))) (cos (atan x))]
  [tan-acos-rev (/ (sqrt (- 1 (* x x))) x) (tan (acos x))]
  [tan-asin-rev (/ x (sqrt (- 1 (* x x)))) (tan (asin x))]
  [cos-asin-rev (sqrt (- 1 (* x x))) (cos (asin x))]
  [sin-atan-rev (/ x (sqrt (+ 1 (* x x)))) (sin (atan x))]
  [sin-acos-rev (sqrt (- 1 (* x x))) (sin (acos x))])

; Hyperbolic trigonometric functions
(define-rules hyperbolic
  [sinh-def (sinh x) (/ (- (exp x) (exp (neg x))) 2)]
  [cosh-def (cosh x) (/ (+ (exp x) (exp (neg x))) 2)]
  [tanh-def-a (tanh x) (/ (- (exp x) (exp (neg x))) (+ (exp x) (exp (neg x))))]
  [tanh-def-b (tanh x) (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1))]
  [tanh-def-c (tanh x) (/ (- 1 (exp (* -2 x))) (+ 1 (exp (* -2 x))))]
  [sinh-cosh (- (* (cosh x) (cosh x)) (* (sinh x) (sinh x))) 1]
  [sinh-+-cosh (+ (cosh x) (sinh x)) (exp x)]
  [sinh---cosh (- (cosh x) (sinh x)) (exp (neg x))])

(define-rules hyperbolic
  [tanh-def-b-rev (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1)) (tanh x)]
  [tanh-def-c-rev (/ (- 1 (exp (* -2 x))) (+ 1 (exp (* -2 x)))) (tanh x)]
  [sinh-def-rev (/ (- (exp x) (exp (neg x))) 2) (sinh x)]
  [cosh-def-rev (/ (+ (exp x) (exp (neg x))) 2) (cosh x)]
  [sinh-+-cosh-rev (exp x) (+ (cosh x) (sinh x))]
  [sinh---cosh-rev (exp (neg x)) (- (cosh x) (sinh x))])

(define-rules hyperbolic
  [sinh-undef (- (exp x) (exp (neg x))) (* 2 (sinh x))]
  [cosh-undef (+ (exp x) (exp (neg x))) (* 2 (cosh x))]
  [tanh-undef (/ (- (exp x) (exp (neg x))) (+ (exp x) (exp (neg x)))) (tanh x)] ;
  [cosh-sum (cosh (+ x y)) (+ (* (cosh x) (cosh y)) (* (sinh x) (sinh y)))]
  [cosh-diff (cosh (- x y)) (- (* (cosh x) (cosh y)) (* (sinh x) (sinh y)))]
  [cosh-2 (cosh (* 2 x)) (+ (* (sinh x) (sinh x)) (* (cosh x) (cosh x)))]
  [cosh-1/2 (cosh (/ x 2)) (sqrt (/ (+ (cosh x) 1) 2))]
  [sinh-sum (sinh (+ x y)) (+ (* (sinh x) (cosh y)) (* (cosh x) (sinh y)))]
  [sinh-diff (sinh (- x y)) (- (* (sinh x) (cosh y)) (* (cosh x) (sinh y)))]
  [sinh-2 (sinh (* 2 x)) (* 2 (* (sinh x) (cosh x)))]
  [sinh-1/2 (sinh (/ x 2)) (/ (sinh x) (sqrt (* 2 (+ (cosh x) 1))))]
  [tanh-2 (tanh (* 2 x)) (/ (* 2 (tanh x)) (+ 1 (* (tanh x) (tanh x))))]
  [tanh-1/2 (tanh (/ x 2)) (/ (sinh x) (+ (cosh x) 1))]
  [sum-sinh (+ (sinh x) (sinh y)) (* 2 (* (sinh (/ (+ x y) 2)) (cosh (/ (- x y) 2))))]
  [sum-cosh (+ (cosh x) (cosh y)) (* 2 (* (cosh (/ (+ x y) 2)) (cosh (/ (- x y) 2))))]
  [diff-sinh (- (sinh x) (sinh y)) (* 2 (* (cosh (/ (+ x y) 2)) (sinh (/ (- x y) 2))))]
  [diff-cosh (- (cosh x) (cosh y)) (* 2 (* (sinh (/ (+ x y) 2)) (sinh (/ (- x y) 2))))]
  [tanh-sum (tanh (+ x y)) (/ (+ (tanh x) (tanh y)) (+ 1 (* (tanh x) (tanh y))))])

(define-rules hyperbolic
  [sinh-undef-rev (* 2 (sinh x)) (- (exp x) (exp (neg x)))]
  [cosh-undef-rev (* 2 (cosh x)) (+ (exp x) (exp (neg x)))]
  [diff-cosh-rev (* 2 (* (sinh (/ (+ x y) 2)) (sinh (/ (- x y) 2)))) (- (cosh x) (cosh y))]
  [diff-sinh-rev (* 2 (* (cosh (/ (+ x y) 2)) (sinh (/ (- x y) 2)))) (- (sinh x) (sinh y))]
  [cosh-diff-rev (- (* (cosh x) (cosh y)) (* (sinh x) (sinh y))) (cosh (- x y))]
  [sinh-diff-rev (- (* (sinh x) (cosh y)) (* (cosh x) (sinh y))) (sinh (- x y))]
  [tanh-1/2-rev (/ (sinh x) (+ (cosh x) 1)) (tanh (/ x 2))]
  [tanh-1/2*-rev (/ (- (cosh x) 1) (sinh x)) (tanh (/ x 2))]
  [tanh-2-rev (/ (* 2 (tanh x)) (+ 1 (* (tanh x) (tanh x)))) (tanh (* 2 x))]
  [sinh-1/2-rev (/ (sinh x) (sqrt (* 2 (+ (cosh x) 1)))) (sinh (/ x 2))]
  [cosh-1/2-rev (sqrt (/ (+ (cosh x) 1) 2)) (cosh (/ x 2))]
  [sinh-2-rev (* 2 (* (sinh x) (cosh x))) (sinh (* 2 x))]
  [cosh-2-rev (+ (* (sinh x) (sinh x)) (* (cosh x) (cosh x))) (cosh (* 2 x))]
  [sinh-sum-rev (+ (* (sinh x) (cosh y)) (* (cosh x) (sinh y))) (sinh (+ x y))]
  [tanh-sum-rev (/ (+ (tanh x) (tanh y)) (+ 1 (* (tanh x) (tanh y)))) (tanh (+ x y))]
  [cosh-sum-rev (+ (* (cosh x) (cosh y)) (* (sinh x) (sinh y))) (cosh (+ x y))]
  [sum-cosh-rev (* 2 (* (cosh (/ (+ x y) 2)) (cosh (/ (- x y) 2)))) (+ (cosh x) (cosh y))]
  [sum-sinh-rev (* 2 (* (sinh (/ (+ x y) 2)) (cosh (/ (- x y) 2)))) (+ (sinh x) (sinh y))])

(define-rules hyperbolic
  [sinh-neg (sinh (neg x)) (neg (sinh x))]
  [sinh-0 (sinh 0) 0]
  [sinh-0-rev 0 (sinh 0)]
  [cosh-neg (cosh (neg x)) (cosh x)]
  [cosh-0 (cosh 0) 1]
  [cosh-0-rev 1 (cosh 0)]
  [cosh-neg-rev (cosh x) (cosh (neg x))]
  [sinh-neg-rev (neg (sinh x)) (sinh (neg x))])

(define-rules hyperbolic
  [asinh-def (asinh x) (log (+ x (sqrt (+ (* x x) 1))))]
  [acosh-def (acosh x) (log (+ x (sqrt (- (* x x) 1))))]
  [atanh-def (atanh x) (/ (log (/ (+ 1 x) (- 1 x))) 2)]
  [sinh-asinh (sinh (asinh x)) x]
  [sinh-acosh (sinh (acosh x)) (sqrt (- (* x x) 1))]
  [sinh-atanh (sinh (atanh x)) (/ x (sqrt (- 1 (* x x))))]
  [cosh-asinh (cosh (asinh x)) (sqrt (+ (* x x) 1))]
  [cosh-acosh (cosh (acosh x)) x]
  [cosh-atanh (cosh (atanh x)) (/ 1 (sqrt (- 1 (* x x))))]
  [tanh-asinh (tanh (asinh x)) (/ x (sqrt (+ 1 (* x x))))]
  [tanh-acosh (tanh (acosh x)) (/ (sqrt (- (* x x) 1)) x)]
  [tanh-atanh (tanh (atanh x)) x])

(define-rules hyperbolic
  [asinh-def-rev (log (+ x (sqrt (+ (* x x) 1)))) (asinh x)]
  [atanh-def-rev (/ (log (/ (+ 1 x) (- 1 x))) 2) (atanh x)]
  [acosh-def-rev (log (+ x (sqrt (- (* x x) 1)))) (acosh x)]
  [tanh-asinh-rev (/ x (sqrt (+ 1 (* x x)))) (tanh (asinh x))]
  [cosh-asinh-rev (sqrt (+ (* x x) 1)) (cosh (asinh x))]
  [sinh-atanh-rev (/ x (sqrt (- 1 (* x x)))) (sinh (atanh x))]
  [cosh-atanh-rev (/ 1 (sqrt (- 1 (* x x)))) (cosh (atanh x))]
  [asinh-2 (acosh (+ (* 2 (* x x)) 1)) (* 2 (asinh (fabs x)))]
  [acosh-2-rev (* 2 (acosh x)) (acosh (- (* 2 (* x x)) 1))])

; Sound-X removal rules: run these before lowering
(define/reset unsound-counter 0)

(define (add-unsound expr)
  (match expr
    [(? number?) expr]
    [(? symbol?) expr]
    [(list (and (or '/ 'log 'pow) op) args ...)
     (unsound-counter (add1 (unsound-counter)))
     `(,(string->symbol (format "sound-~a" op)) ,@(map add-unsound args)
                                                ,(string->symbol (format "t_~a" (unsound-counter))))]
    [(list op args ...) (cons op (map add-unsound args))]))

(define-rules arithmetic
  [add-sound-/ '(/ a b) '(sound-/ a b 0)]
  [add-sound-pow '(pow a b) '(sound-pow a b 0)]
  [add-sound-log '(log a) '(sound-log a 0)]
  [remove-sound-/ '(sound-/ a 0 b) b]
  [remove-sound-log '(sound-log 0 b) b])
