(FPCore
 (a b)
 :name
 "Exp of sum of logs"
 :target
 (* a b)
 (exp (+ (log a) (log b))))
(FPCore
 (a b)
 :name
 "Quotient of sum of exps"
 :target
 (/ 1 (+ 1 (exp (- b a))))
 (/ (exp a) (+ (exp a) (exp b))))
(FPCore
 (a1 a2 b1 b2)
 :name
 "Quotient of products"
 :target
 (* (/ a1 b1) (/ a2 b2))
 (/ (* a1 a2) (* b1 b2)))
