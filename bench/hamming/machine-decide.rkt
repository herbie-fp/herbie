
(lambda (a x)
  #:name "NMSE section 3.5"
  (- (exp (* a x)) 1)
  #:target
  (if (< (abs (* a x)) 1/10)
      (* (* a x) (+ 1 (+ (/ (* a x) 2) (/ (sqr (* a x)) 6))))
      (- (exp (* a x)) 1)))
