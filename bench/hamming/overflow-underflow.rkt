
(lambda (x)
  #:name "NMSE section 3.11"
  (/ (exp x) (- (exp x) 1))
  #:target
  (/ 1 (- 1 (exp (- x)))))
