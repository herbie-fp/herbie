
; Suggested by Akshay Srinivasan, from a logistic regression implementation

(herbie-test (x y)
  "Logistic regression 2"
  (- (log (+ 1 (exp x))) (* x y))
  (if (<= x 0) ;;cases are flipped.
      (- (log (+ 1 (exp x))) (* x y))
      (- (log (+ 1 (exp (- x)))) (* (- x) (- 1 y)))))
