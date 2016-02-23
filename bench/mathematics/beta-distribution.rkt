
(herbie-test ([m (positive default)] [v (< 0 default 0.25)])
  "a parameter of renormalized beta distribution"
  (* (- (/ (* m (- 1 m)) v) 1) m))

(herbie-test ([m (positive default)] [v (< 0 default 0.25)])
  "b parameter of renormalized beta distribution"
  (* (- (/ (* m (- 1 m)) v) 1) (- 1 m)))

