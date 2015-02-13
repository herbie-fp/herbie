
(herbie-test (x)
  "NMSE section 3.11"
  (/ (exp x) (- (exp x) 1))
  (/ 1 (- 1 (exp (- x)))))
