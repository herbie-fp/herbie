#lang racket
(require casio/test)

(casio-test (x y)
  "Logistic regression 1"

  (- (/ 1 (+ 1 (exp (- x)))) y)

  (if (> x 0)
      (- (/ 1 (+ 1 (exp (- x)))) y)
      (- (- 1 y) (/ 1 (1+ (exp x))))))


(casio-test (x y)
  "Logistic regression 2"

  (- (log (+ 1 (exp x))) (* x y))

  (if (<= x 0) ;;cases are flipped.
      (- (log (1+ (exp x))) (* x y))
      (- (log (1+ (exp (- x)))) (* (- x) (- 1 y)))))
