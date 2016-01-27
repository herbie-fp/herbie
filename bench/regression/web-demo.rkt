(herbie-test (x y z)
  "abs fraction 1"
  (abs (- (/ (+ x 4) y) (* (/ x y) z))))

(herbie-test (a b)
  "abs fraction 2"
  (/ (abs (- a b)) 2))

(herbie-test (f n)
  "subtraction fraction"
  (/ (- (+ f n)) (- f n)))

(herbie-test (x)
  "exp neg sub"
  (exp (- (- 1 (* x x)))))

(herbie-test (x)
  "sqrt times"
  (* (sqrt (- x 1)) (sqrt x)))

(herbie-test (x)
  "neg log"
  (- (log (- (/ 1 x) 1))))

(herbie-test (B x)
  "VandenBroeck and Keller, Equation (24)"
  (+ (- (* x (cotan B))) (/ 1 (sin B))))
