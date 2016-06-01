(FPCore (x) :name "Hyperbolic sine" (/ (- (exp x) (exp (- x))) 2))
(FPCore
 (x)
 :name
 "Hyperbolic tangent"
 (/ (- (exp x) (exp (- x))) (+ (exp x) (exp (- x)))))
(FPCore (x) :name "Hyperbolic secant" (/ 2 (+ (exp x) (exp (- x)))))
(FPCore
 (x)
 :name
 "Hyperbolic arcsine"
 :target
 (if (< x 0)
   (log (/ -1 (- x (sqrt (+ (sqr x) 1)))))
   (log (+ x (sqrt (+ (sqr x) 1)))))
 (log (+ x (sqrt (+ (sqr x) 1)))))
(FPCore (x) :name "Hyperbolic arc-cosine" (log (+ x (sqrt (- (sqr x) 1)))))
(FPCore
 (x)
 :name
 "Hyperbolic arc-(co)tangent"
 (* (/ 1 2) (log (/ (+ 1 x) (- 1 x)))))
(FPCore
 (x)
 :name
 "Hyperbolic arc-(co)secant"
 (log (+ (/ 1 x) (/ (sqrt (- 1 (sqr x))) x))))
