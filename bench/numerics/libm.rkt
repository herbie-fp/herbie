(lambda (x y z)
  #:name "simple fma test"
  (- (fma x y z) (+ 1 (+ (* x y) z)))
  #:target
  -1)
