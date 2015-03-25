
(herbie-test (x)
  "Kahan's exp quotient"
  (/ (- (exp x) 1) x)
  (if (and (< x 1) (> x -1))
      (/ (- (exp x) 1) (log (exp x)))
      (/ (- (exp x) 1) x)))
