(FPCore
 ((m (positive default)) (v (< 0 default 0.25)))
 :name
 "a parameter of renormalized beta distribution"
 (* (- (/ (* m (- 1 m)) v) 1) m))
(FPCore
 ((m (positive default)) (v (< 0 default 0.25)))
 :name
 "b parameter of renormalized beta distribution"
 (* (- (/ (* m (- 1 m)) v) 1) (- 1 m)))
