(FPCore
 (x y)
 :name
 "Logistic regression 2"
 :target
 (if (<= x 0)
   (- (log (+ 1 (exp x))) (* x y))
   (- (log (+ 1 (exp (- x)))) (* (- x) (- 1 y))))
 (- (log (+ 1 (exp x))) (* x y)))
