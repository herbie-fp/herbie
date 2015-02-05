#lang racket
(require "../herbie/test.rkt")

; Suggested by Akshay

(herbie-test (x y)
  "Logistic regression 2"

  (- (log (+ 1 (exp x))) (* x y))

  (if (<= x 0) ;;cases are flipped.
      (- (log (+ 1 (exp x))) (* x y))
      (- (log (+ 1 (exp (- x)))) (* (- x) (- 1 y)))))

; Suggested by Arvind

(herbie-test (a b)
  "Exp of sum of logs"
  (exp (+ (log a) (log b)))
  (* a b))

(herbie-test (a b)
  "Quotient of sum of exps"
  (/ (exp a) (+ (exp a) (exp b)))
  (/ 1 (+ 1 (exp (- b a)))))

(herbie-test (a1 a2 b1 b2)
  "Quotient of products"
  (/ (* a1 a2) (* b1 b2))
  (* (/ a1 b1) (/ a2 b2)))

; Suggested by Harley

(herbie-test ((e+ (positive default)) (e- (positive default)) t s)
  "Harley's example"
  (/ (* (expt (/ (+ 1 (exp (- s)))) e+) (expt (- 1 (/ (+ 1 (exp (- s))))) e-))
     (* (expt (/ (+ 1 (exp (- t)))) e+) (expt (- 1 (/ (+ 1 (exp (- t))))) e-)))
  (* (expt (/ (+ 1 (exp (- t))) (+ 1 (exp (- s)))) e+)
     (expt (/ (+ 1 (exp t)) (+ 1 (exp s))) e-)))
