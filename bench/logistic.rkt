#lang racket
(require casio/test)

; Suggested by Akshay

(casio-test (x y)
  "Logistic regression 2"

  (- (log (+ 1 (exp x))) (* x y))

  (if (<= x 0) ;;cases are flipped.
      (- (log (+ 1 (exp x))) (* x y))
      (- (log (+ 1 (exp (- x)))) (* (- x) (- 1 y)))))

; Suggested by Arvind

(casio-test (a b)
  "Exp of sum of logs"
  (exp (+ (log a) (log b)))
  (* a b))

(casio-test (a b)
  "Quotient of sum of exps"
  (/ (exp a) (+ (exp a) (exp b)))
  (/ 1 (+ 1 (exp (- b a)))))

(casio-test (a1 a2 b1 b2)
  "Quotient of products"
  (/ (* a1 a2) (* b1 b2))
  (* (/ a1 b1) (/ a2 b2)))
