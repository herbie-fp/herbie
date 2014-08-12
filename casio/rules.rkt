#lang racket

;; Arithmetic identities for rewriting programs.

(require rackunit)
(require casio/common)
(require casio/programs)
(require casio/points)

(provide (struct-out rule) *rules* rules-tests)

; A rule has a name and an input and output pattern.
; It also has a relative path into the output where simplification
; should happen if the rule applies.

(struct rule (name input output slocations)
        #:methods gen:custom-write
        [(define (write-proc rule port mode)
           (display "#<rule " port)
           (write (rule-name rule) port)
           (display ">" port))])

(define-syntax (define-rule stx)
  (syntax-case stx ()
    [(_ name input output #:simplify slocations)
     #'(set! *rules* (cons (rule 'name 'input 'output 'slocations) *rules*))]
    [(_ name input output)
     #'(set! *rules* (cons (rule 'name 'input 'output '()) *rules*))]))

(define *rules* '())

; Commutativity
(define-rule   +-commutative     (+ a b)               (+ b a))
(define-rule   *-commutative     (* a b)               (* b a))

; Associativity
(define-rule   associate-+-lft   (+ a (+ b c))         (+ (+ a b) c)     #:simplify ((2)))
(define-rule   associate-+-rgt   (+ (+ a b) c)         (+ a (+ b c))     #:simplify ((2)))
(define-rule   associate---lft   (+ a (- b c))         (- (+ a b) c)     #:simplify ((2)))
(define-rule   associate---rgt   (- (+ a b) c)         (+ a (- b c))     #:simplify ((2)))
(define-rule   associate-*-lft   (* a (* b c))         (* (* a b) c)     #:simplify ((2)))
(define-rule   associate-*-rgt   (* (* a b) c)         (* a (* b c))     #:simplify ((2)))
(define-rule   associate-/-lft   (* a (/ b c))         (/ (* a b) c)     #:simplify ((2)))
(define-rule   associate-/-rgt   (/ (* a b) c)         (* a (/ b c))     #:simplify ((2)))

; Distributivity
(define-rule   distribute-lft-in     (* a (+ b c))         (+ (* a b) (* a c))     #:simplify ((1) (2)))
(define-rule   distribute-rgt-in     (* a (+ b c))         (+ (* b a) (* c a))     #:simplify ((1) (2)))
(define-rule   distribute-lft-out    (+ (* a b) (* a c))   (* a (+ b c))           #:simplify ((2)))
(define-rule   distribute-lft-out--  (- (* a b) (* a c))   (* a (- b c))           #:simplify ((2)))
(define-rule   distribute-rgt-out    (+ (* b a) (* c a))   (* a (+ b c))           #:simplify ((2)))
(define-rule   distribute-rgt-out--  (- (* b a) (* c a))   (* a (- b c))           #:simplify ((2)))
(define-rule   distribute-lft1-in    (+ (* b a) a)         (* (+ b 1) a)           #:simplify ((1)))
(define-rule   distribute-rgt1-in    (+ a (* c a))         (* (+ c 1) a)           #:simplify ((1)))
(define-rule   distribute-lft-neg-in (- (* a b))           (* (- a) b))
(define-rule   distribute-rgt-neg-in (- (* a b))           (* a (- b)))
(define-rule   distribute-lft-neg-out (* (- a) b)          (- (* a b)))
(define-rule   distribute-rgt-neg-out (* a (- b))          (- (* a b)))
(define-rule   distribute-neg-in     (- (+ a b))           (+ (- a) (- b)))
(define-rule   distribute-neg-out    (+ (- a) (- b))       (- (+ a b)))
(define-rule   distribute-inv-in     (/ (* a b))           (* (/ a) (/ b)))
(define-rule   distribute-inv-out    (* (/ a) (/ b))       (/ (* a b)))
(define-rule   distribute-inv-neg    (/ (- a))             (- (/ a)))
(define-rule   distribute-neg-inv    (- (/ a))             (/ (- a)))

; Difference of squares
(define-rule   difference-of-squares (- (sqr a) (sqr b))   (* (+ a b) (- a b))     #:simplify ((1) (2)))
(define-rule   difference-of-sqr-1   (- (sqr a) 1)         (* (+ a 1) (- a 1))     #:simplify ((1) (2)))
(define-rule   difference-of-sqr--1  (+ (sqr a) -1)        (* (+ a 1) (- a 1))     #:simplify ((1) (2)))

; Identity
(define-rule   +-lft-identity    (+ 0 a)               a)
(define-rule   +-rgt-identity    (+ a 0)               a)
(define-rule   +-inverses        (- a a)               0)
(define-rule   sub-neg           (- a b)               (+ a (- b)))
(define-rule   unsub-neg         (+ a (- b))           (- a b))
(define-rule   neg-sub0          (- b)                 (- 0 b))
(define-rule   sub0-neg          (- 0 b)               (- b))
(define-rule   remove-double-neg (- (- a))             a)
(define-rule   *-lft-identity    (* 1 a)               a)
(define-rule   *-un-lft-identity a                     (* 1 a))
(define-rule   *-rgt-identity    (* a 1)               a)
(define-rule   *-inverses        (/ a a)               1)
(define-rule   div-inv           (/ a b)               (* a (/ b)))
(define-rule   un-div-inv        (* a (/ b))           (/ a b))
(define-rule   remove-double-div (/ (/ a))             a)
(define-rule   div1              (/ 1)                 1)
(define-rule   div0              (/ 0 a)               0)
(define-rule   mul0              (* 0 a)               0)
(define-rule   mul-1-neg         (* -1 a)              (- a))
(define-rule   neg-mul-1         (- a)                 (* -1 a))

; Dealing with fractions
(define-rule   div-sub     (/ (- a b) c)        (- (/ a c) (/ b c))
  #:simplify ((1) (2)))
(define-rule   sub-div     (- (/ a c) (/ b c))  (/ (- a b) c)
  #:simplify ((1)))
(define-rule   frac-add    (+ (/ a b) (/ c d))  (/ (+ (* a d) (* b c)) (* b d))
  #:simplify ((1)))
(define-rule   frac-sub    (- (/ a b) (/ c d))  (/ (- (* a d) (* b c)) (* b d))
  #:simplify ((1)))
(define-rule   frac-times  (* (/ a b) (/ c d))  (/ (* a c) (* b d))
  #:simplify ((1) (2)))
(define-rule   times-frac  (/ (* a b) (* c d))  (* (/ a c) (/ b d))
  #:simplify ((1) (2)))

; Square root
(define-rule   add-sqr-sqrt      x                  (sqr (sqrt x)))
(define-rule   rem-square-sqrt   (sqr (sqrt x))     x)
(define-rule   rem-sqrt-square   (sqrt (sqr x))     (abs x))
(define-rule   square-mult       (sqr x)            (* x x))
(define-rule   square-unmult     (* x x)            (sqr x))
(define-rule   square-prod       (sqr (* x y))      (* (sqr x) (sqr y))
  #:simplify ((1) (2)))
(define-rule   square-unprod     (* (sqr x) (sqr y)) (sqr (* x y))
  #:simplify ((1)))
(define-rule   square-div        (sqr (/ x y))      (/ (sqr x) (sqr y))
  #:simplify ((1) (2)))
(define-rule   square-undiv      (/ (sqr x) (sqr y)) (sqr (/ x y))
  #:simplify ((1)))
(define-rule   sqrt-prod         (sqrt (* x y))     (* (sqrt x) (sqrt y))
  #:simplify ((1) (2)))
(define-rule   sqrt-unprod       (* (sqrt x) (sqrt y)) (sqrt (* x y))
  #:simplify ((1)))
(define-rule   sqrt-div          (sqrt (/ x y))     (/ (sqrt x) (sqrt y))
  #:simplify ((1) (2)))
(define-rule   sqrt-undiv        (/ (sqrt x) (sqrt y)) (sqrt (/ x y))
  #:simplify ((1)))
(define-rule   sqr-neg           (sqr (- x))        (sqr x))

; Exponentials
(define-rule   add-exp-log  x                    (exp (log x)))
(define-rule   add-log-exp  x                    (log (exp x)))
(define-rule   rem-exp-log  (exp (log x))        x)
(define-rule   rem-log-exp  (log (exp x))        x)
(define-rule   exp-sum      (exp (+ a b))        (* (exp a) (exp b)))
(define-rule   prod-exp     (* (exp a) (exp b))  (exp (+ a b))
  #:simplify ((1)))
(define-rule   exp-neg      (exp (- a))          (/ (exp a)))
(define-rule   rec-exp      (/ (exp a))          (exp (- a)))
(define-rule   exp-diff     (exp (- a b))        (/ (exp a) (exp b)))
(define-rule   div-exp      (/ (exp a) (exp b))  (exp (- a b))
  #:simplify ((1)))
(define-rule   exp-prod     (exp (* a b))        (expt (exp a) b))

; Powers
(define-rule   expt-exp        (expt (exp a) b)            (exp (* a b))
  #:simplify ((1)))
(define-rule   expt-to-exp     (expt a b)                  (exp (* (log a) b)))
(define-rule   exp-to-expt     (exp (* (log a) b))         (expt a b))
(define-rule   expt-prod-up    (* (expt a b) (expt a c))   (expt a (+ b c)))
(define-rule   expt-prod-down  (* (expt b a) (expt c a))   (expt (* b c) a))
(define-rule   unexpt-prod-down (expt (* b c) a)           (* (expt b a) (expt c a)))
(define-rule   expt1           a                           (expt a 1))
(define-rule   unexpt1         (expt a 1)                  a)
(define-rule   unexpt0         (expt a 0)                  1)
(define-rule   unexpt2         (expt a 2)                  (sqr a))
(define-rule   expt2           (sqr a)                     (expt a 2))
(define-rule   unexpt1/2       (expt a 1/2)                (sqrt a))
(define-rule   expt1/2         (sqrt a)                    (expt a 1/2))
(define-rule   expt-plus       (* (expt a b) a)            (expt a (+ b 1)))
(define-rule   expt-expt       (expt (expt a b) c)         (expt a (* b c)))
(define-rule   inv-expt        (/ a)                       (expt a -1))

; Logarithms
(define-rule   sum-log      (+ (log a) (log b))  (log (* a b))
  #:simplify ((1)))
(define-rule   log-prod     (log (* a b))        (+ (log a) (log b)))
(define-rule   diff-log     (- (log a) (log b))  (log (/ a b))
  #:simplify ((1)))
(define-rule   log-div      (log (/ a b))        (- (log a) (log b)))
(define-rule   neg-log      (- (log a))          (log (/ a))
  #:simplify ((1)))
(define-rule   log-rec      (log (/ a))          (- (log a))
  #:simplify ((1)))
(define-rule   log-pow      (log (expt a b))     (* b (log a)))

; Multiplying by x / x
(define-rule   flip-+     (+ a b)  (/ (- (sqr a) (sqr b)) (- a b))
  #:simplify ((1) (2)))
(define-rule   flip--     (- a b)  (/ (- (sqr a) (sqr b)) (+ a b))
  #:simplify ((1) (2)))
(define-rule   clear-num  (/ a b)  (/ 1 (/ b a))
  #:simplify ((2)))

; Trigonometry
(define-rule   cos-sin-sum (+ (sqr (cos a))    (sqr (sin a))) 1)
(define-rule   1-sub-cos   (- 1 (sqr (cos a))) (sqr (sin a)) #:simplify ((1)))
(define-rule   1-sub-sin   (- 1 (sqr (sin a))) (sqr (cos a)) #:simplify ((1)))
(define-rule   -1-add-cos  (+ (sqr (cos a)) -1) (- (sqr (sin a))) #:simplify ((1)))
(define-rule   -1-add-sin  (+ (sqr (sin a)) -1) (- (sqr (cos a))) #:simplify ((1)))
(define-rule   sin-neg     (sin (- x))         (- (sin x)))
(define-rule   cos-neg     (cos (- x))         (cos x))
(define-rule   sin-sum     (sin (+ x y))       (+ (* (sin x) (cos y)) (* (cos x) (sin y))))
(define-rule   cos-sum     (cos (+ x y))       (- (* (cos x) (cos y)) (* (sin x) (sin y))))
(define-rule   sin-diff    (sin (- x y))       (- (* (sin x) (cos y)) (* (cos x) (sin y))))
(define-rule   cos-diff    (cos (- x y))       (+ (* (cos x) (cos y)) (* (sin x) (sin y))))
(define-rule   diff-atan   (- (atan x) (atan y)) (atan2 (- x y) (+ 1 (* x y))) #:simplify ((1)))
(define-rule   quot-tan    (/ (sin x) (cos x)) (tan x))
(define-rule   tan-quot    (tan x)             (/ (sin x) (cos x)))

(define rules-tests
  (test-suite "Test rewrite rules for soundness"
   (for ([rule *rules*])
     (let ([name (rule-name rule)] [p1 (rule-input rule)] [p2 (rule-output rule)])
       (test-case (~a (rule-name rule))
         (let*-values ([(fv) (free-variables p1)]
                       [(pts exs1) (prepare-points `(λ ,fv ,p1))]
                       [(exs2) (make-exacts `(λ ,fv ,p2) pts)])
           (for ([pt pts] [ex1 exs1] [ex2 exs2])
             (with-check-info (['point pt] ['prog1 p1] ['prog2 p2])
               (when (and (ordinary-float? ex1) (ordinary-float? ex2))
                 (check-= ex1 ex2 0))))))))))
