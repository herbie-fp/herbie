
(herbie-test ([m (positive double)] [v (< 0 double 0.25)])
  "a parameter of renormalized beta distribution"
  (* (- (/ (* m (- 1 m)) v) 1) m))

(herbie-test ([m (positive double)] [v (< 0 double 0.25)])
  "b parameter of renormalized beta distribution"
  (* (- (/ (* m (- 1 m)) v) 1) (- 1 m)))

