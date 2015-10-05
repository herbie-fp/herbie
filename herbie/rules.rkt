#lang racket

;; Arithmetic identities for rewriting programs.

(require "common.rkt")
(require unstable/sequence)

(provide (struct-out rule) *rules* *simplify-rules* get-rule)

(struct rule (name input output) ; Input and output are patterns
        #:methods gen:custom-write
        [(define (write-proc rule port mode)
           (display "#<rule " port)
           (write (rule-name rule) port)
           (display ">" port))])

(define *rulesets* (make-parameter '()))

(define-syntax-rule (define-ruleset name groups [rname input output] ...)
  (begin (define name (list (rule 'rname 'input 'output) ...))
	 (*rulesets* (cons (cons name 'groups) (*rulesets*)))))

(define (get-rule name)
  (let ([results (filter (λ (rule) (eq? (rule-name rule) name)) (*rules*))])
    (if (null? results)
	(error "Could not find a rule by the name" name)
	(car results))))

; Commutativity
(define-ruleset commutivity (arithmetic simplify)
  [+-commutative     (+ a b)               (+ b a)]
  [*-commutative     (* a b)               (* b a)])

; Associativity
(define-ruleset associativity (arithmetic simplify)
  [associate-+r+     (+ a (+ b c))         (+ (+ a b) c)]
  [associate-+l+     (+ (+ a b) c)         (+ a (+ b c))]
  [associate-+r-     (+ a (- b c))         (- (+ a b) c)]
  [associate-+l-     (+ (- a b) c)         (- a (- b c))]
  [associate--r+     (- a (+ b c))         (- (- a b) c)]
  [associate--l+     (- (+ a b) c)         (+ a (- b c))]
  [associate--l-     (- (- a b) c)         (- a (+ b c))]
  [associate--r-     (- a (- b c))         (+ (- a b) c)]
  [associate-*r*     (* a (* b c))         (* (* a b) c)]
  [associate-*l*     (* (* a b) c)         (* a (* b c))]
  [associate-*r/     (* a (/ b c))         (/ (* a b) c)]
  [associate-*l/     (* (/ a b) c)         (/ (* a c) b)]
  [associate-/r*     (/ a (* b c))         (/ (/ a b) c)]
  [associate-/l*     (/ (* b c) a)         (/ b (/ a c))]
  [associate-/r/     (/ a (/ b c))         (* (/ a b) c)]
  [associate-/l/     (/ (/ b c) a)         (/ b (* a c))])

; Distributivity
(define-ruleset distributivity (arithmetic simplify)
  [distribute-lft-in     (* a (+ b c))         (+ (* a b) (* a c))]
  [distribute-rgt-in     (* a (+ b c))         (+ (* b a) (* c a))]
  [distribute-lft-out    (+ (* a b) (* a c))   (* a (+ b c))]
  [distribute-lft-out--  (- (* a b) (* a c))   (* a (- b c))]
  [distribute-rgt-out    (+ (* b a) (* c a))   (* a (+ b c))]
  [distribute-rgt-out--  (- (* b a) (* c a))   (* a (- b c))]
  [distribute-lft1-in    (+ (* b a) a)         (* (+ b 1) a)]
  [distribute-rgt1-in    (+ a (* c a))         (* (+ c 1) a)]
  [distribute-lft-neg-in (- (* a b))           (* (- a) b)]
  [distribute-rgt-neg-in (- (* a b))           (* a (- b))]
  [distribute-lft-neg-out (* (- a) b)          (- (* a b))]
  [distribute-rgt-neg-out (* a (- b))          (- (* a b))]
  [distribute-neg-in     (- (+ a b))           (+ (- a) (- b))]
  [distribute-neg-out    (+ (- a) (- b))       (- (+ a b))]
  [distribute-inv-in     (/ (* a b))           (* (/ a) (/ b))]
  [distribute-inv-out    (* (/ a) (/ b))       (/ (* a b))]
  [distribute-inv-neg    (/ (- a))             (- (/ a))]
  [distribute-neg-inv    (- (/ a))             (/ (- a))]
  [distribute-frac-neg   (/ (- a) b)           (- (/ a b))]
  [distribute-neg-frac   (- (/ a b))           (/ (- a) b)])
  
; Difference of squares
(define-ruleset difference-of-squares-canonicalize (polynomials simplify)
  [difference-of-squares (- (sqr a) (sqr b))   (* (+ a b) (- a b))]
  [difference-of-sqr-1   (- (sqr a) 1)         (* (+ a 1) (- a 1))]
  [difference-of-sqr--1  (+ (sqr a) -1)        (* (+ a 1) (- a 1))])

(define-ruleset difference-of-squares-flip (polynomials)
  [flip-+     (+ a b)  (/ (- (sqr a) (sqr b)) (- a b))]
  [flip--     (- a b)  (/ (- (sqr a) (sqr b)) (+ a b))])

; Difference of cubes
(define-ruleset difference-of-cubes (polynomials)
  [sum-cubes        (+ (expt a 3) (expt b 3)) (* (+ (sqr a) (- (sqr b) (* a b))) (+ a b))]
  [difference-cubes (- (expt a 3) (expt b 3)) (* (+ (sqr a) (+ (sqr b) (* a b))) (+ a b))]
  [flip3-+    (+ a b)  (/ (- (expt a 3) (expt b 3)) (+ (sqr a) (- (sqr b) (* a b))))]
  [flip3--    (- a b)  (/ (- (expt a 3) (expt b 3)) (+ (sqr a) (+ (sqr b) (* a b))))])

; Identity
(define-ruleset id-reduce (arithmetic simplify)
  [+-lft-identity    (+ 0 a)               a]
  [+-rgt-identity    (+ a 0)               a]
  [+-inverses        (- a a)               0]
  [sub0-neg          (- 0 b)               (- b)]
  [remove-double-neg (- (- a))             a]
  [*-lft-identity    (* 1 a)               a]
  [*-rgt-identity    (* a 1)               a]
  [*-inverses        (/ a a)               1]
  [remove-double-div (/ 1 (/ 1 a))         a]
  [div0              (/ 0 a)               0]
  [mul0              (* 0 a)               0]
  [mul-1-neg         (* -1 a)              (- a)])

(define-ruleset id-transform (arithmetic)
  [sub-neg           (- a b)               (+ a (- b))]
  [unsub-neg         (+ a (- b))           (- a b)]
  [neg-sub0          (- b)                 (- 0 b)]
  [*-un-lft-identity a                     (* 1 a)]
  [div-inv           (/ a b)               (* a (/ 1 b))]
  [un-div-inv        (* a (/ 1 b))         (/ a b)]
  [neg-mul-1         (- a)                 (* -1 a)]
  [clear-num         (/ a b)               (/ 1 (/ b a))])

; Dealing with fractions
(define-ruleset fractions-distribute (fractions simplify)
  [div-sub     (/ (- a b) c)        (- (/ a c) (/ b c))]
  [times-frac  (/ (* a b) (* c d))  (* (/ a c) (/ b d))])

(define-ruleset fractions-transform (fractions)
  [sub-div     (- (/ a c) (/ b c))  (/ (- a b) c)]
  [frac-add    (+ (/ a b) (/ c d))  (/ (+ (* a d) (* b c)) (* b d))]
  [frac-sub    (- (/ a b) (/ c d))  (/ (- (* a d) (* b c)) (* b d))]
  [frac-times  (* (/ a b) (/ c d))  (/ (* a c) (* b d))])

; Square root
(define-ruleset squares-reduce (arithmetic simplify)
  [rem-square-sqrt   (sqr (sqrt x))     x]
  [rem-sqrt-square   (sqrt (sqr x))     (abs x)]
  [sqr-neg           (sqr (- x))        (sqr x)])

(define-ruleset squares-distribute (arithmetic simplify)
  [square-prod       (sqr (* x y))      (* (sqr x) (sqr y))]
  [square-div        (sqr (/ x y))      (/ (sqr x) (sqr y))])

(define-ruleset squares-transform (arithmetic simplify)
  [sqrt-prod         (sqrt (* x y))     (* (sqrt x) (sqrt y))]
  [sqrt-div          (sqrt (/ x y))     (/ (sqrt x) (sqrt y))]
  [sqrt-unprod       (* (sqrt x) (sqrt y)) (sqrt (* x y))]
  [sqrt-undiv        (/ (sqrt x) (sqrt y)) (sqrt (/ x y))]
  [square-mult       (sqr x)            (* x x)]
  [add-sqr-sqrt      x                  (sqr (sqrt x))]
  [square-unprod     (* (sqr x) (sqr y)) (sqr (* x y))]
  [square-undiv      (/ (sqr x) (sqr y)) (sqr (/ x y))])

(define-ruleset squares-canonicalize (arithmetic simplify)
    [square-unmult     (* x x)            (sqr x)])

; Exponentials
(define-ruleset exp-expand (exponents)
  [add-exp-log  x                    (exp (log x))]
  [add-log-exp  x                    (log (exp x))])

(define-ruleset exp-reduce (exponents simplify)
  [rem-exp-log  (exp (log x))        x]
  [rem-log-exp  (log (exp x))        x])

(define-ruleset exp-distribute (exponents simplify)
  [exp-sum      (exp (+ a b))        (* (exp a) (exp b))]
  [exp-neg      (exp (- a))          (/ 1 (exp a))]
  [exp-diff     (exp (- a b))        (/ (exp a) (exp b))])

(define-ruleset exp-factor (exponents)
  [prod-exp     (* (exp a) (exp b))  (exp (+ a b))]
  [rec-exp      (/ 1 (exp a))        (exp (- a))]
  [div-exp      (/ (exp a) (exp b))  (exp (- a b))]
  [exp-prod     (exp (* a b))        (expt (exp a) b)])

; Powers
(define-ruleset pow-reduce (exponents simplify)
  [unexpt1         (expt a 1)                  a]
  [unexpt0         (expt a 0)                  1]
  [rem-cube-cbrt   (expt (expt a (/ 1 3)) 3)   a]
  [rem-cube-cbrt2  (expt (expt a 1/3) 3)       a])

(define-ruleset pow-expand (exponents)
  [expt1           a                           (expt a 1)])

(define-ruleset pow-canonicalize (exponents simplify)
  [exp-to-expt     (exp (* (log a) b))         (expt a b)]
  [expt-plus       (* (expt a b) a)            (expt a (+ b 1))]
  [unexpt2         (expt a 2)                  (sqr a)]
  [unexpt1/2       (expt a 1/2)                (sqrt a)])

(define-ruleset pow-transform (exponents)
  [expt-exp        (expt (exp a) b)            (exp (* a b))]
  [expt-to-exp     (expt a b)                  (exp (* (log a) b))]
  [expt-prod-up    (* (expt a b) (expt a c))   (expt a (+ b c))]
  [expt-prod-down  (* (expt b a) (expt c a))   (expt (* b c) a)]
  [unexpt-prod-down (expt (* b c) a)           (* (expt b a) (expt c a))]
  [inv-expt        (/ 1 a)                     (expt a -1)]
  [expt1/2         (sqrt a)                    (expt a 1/2)]
  [expt2           (sqr a)                     (expt a 2)])

; Logarithms
(define-ruleset log-distribute (exponents simplify)
  [log-prod     (log (* a b))        (+ (log a) (log b))]
  [log-div      (log (/ a b))        (- (log a) (log b))]
  [log-rec      (log (/ 1 a))        (- (log a))]
  [log-pow      (log (expt a b))     (* b (log a))])

(define-ruleset log-factor (exponents)
  [sum-log      (+ (log a) (log b))  (log (* a b))]
  [diff-log     (- (log a) (log b))  (log (/ a b))]
  [neg-log      (- (log a))          (log (/ 1 a))])

; Trigonometry
(define-ruleset trig-reduce (trigonometry simplify)
  [cos-sin-sum (+ (sqr (cos a)) (sqr (sin a))) 1]
  [1-sub-cos   (- 1 (sqr (cos a))) (sqr (sin a))]
  [1-sub-sin   (- 1 (sqr (sin a))) (sqr (cos a))]
  [-1-add-cos  (+ (sqr (cos a)) -1) (- (sqr (sin a)))]
  [-1-add-sin  (+ (sqr (sin a)) -1) (- (sqr (cos a)))]
  [sin-neg     (sin (- x))         (- (sin x))]
  [cos-neg     (cos (- x))         (cos x)])

(define-ruleset trig-expand (trigonometry)
  [sin-sum     (sin (+ x y))       (+ (* (sin x) (cos y)) (* (cos x) (sin y)))]
  [cos-sum     (cos (+ x y))       (- (* (cos x) (cos y)) (* (sin x) (sin y)))]
  [sin-diff    (sin (- x y))       (- (* (sin x) (cos y)) (* (cos x) (sin y)))]
  [cos-diff    (cos (- x y))       (+ (* (cos x) (cos y)) (* (sin x) (sin y)))]
  [diff-atan   (- (atan x) (atan y)) (atan2 (- x y) (+ 1 (* x y)))]
  [quot-tan    (/ (sin x) (cos x)) (tan x)]
  [tan-quot    (tan x)             (/ (sin x) (cos x))]
  [cotan-quot  (cotan x)           (/ (cos x) (sin x))]
  [quot-tan    (/ (sin x) (cos x)) (tan x)]
  [quot-cotan  (/ (cos x) (sin x)) (cotan x)]
  [cotan-tan   (cotan x)           (/ 1 (tan x))]
  [tan-cotan   (tan x)             (/ 1 (cotan x))])

; Specialized numerical functions
(define-ruleset special-numerical-reduce (numerics simplify)
  [expm1-def   (- (exp x) 1)              (expm1 x)]
  [log1p-def   (log (+ 1 x))              (log1p x)]
  [log1p-expm1 (log1p (expm1 x))          x]
  [expm1-log1p (expm1 (log1p x))          x]
  [hypot-def   (sqrt (+ (sqr x) (sqr y))) (hypot x y)]
  [hypot-1-def (sqrt (+ 1 (sqr y)))       (hypot 1 y)])

(define-ruleset special-numerical-expand (numerics)
  [expm1-udef    (expm1 x)   (- (exp x) 1)]
  [log1p-udef    (log1p x)   (log (+ 1 x))]
  [log1p-expm1-u x           (log1p (expm1 x))]
  [expm1-log1p-u x           (expm1 (log1p x))]
  [hypot-udef    (hypot x y) (sqrt (+ (sqr x) (sqr y)))])

(define (*rules*)
  (for/append ([(rules groups) (in-pairs (*rulesets*))])
    (if (ormap (λ (x) ((flag 'rules x) #t #f)) groups) rules '())))

(define (*simplify-rules*)
  (for/append ([(rules groups) (in-pairs (*rulesets*))])
    (if (and (ormap (λ (x) ((flag 'rules x) #t #f)) groups)
             (memq 'simplify groups))
        rules
        '())))
