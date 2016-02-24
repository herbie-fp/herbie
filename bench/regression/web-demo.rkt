(lambda (x y z)
  #:name "abs fraction 1"
  (abs (- (/ (+ x 4) y) (* (/ x y) z))))

(lambda (a b)
  #:name "abs fraction 2"
  (/ (abs (- a b)) 2))

(lambda (f n)
  #:name "subtraction fraction"
  (/ (- (+ f n)) (- f n)))

(lambda (x)
  #:name "exp neg sub"
  (exp (- (- 1 (* x x)))))

(lambda (x)
  #:name "sqrt times"
  (* (sqrt (- x 1)) (sqrt x)))

(lambda (x)
  #:name "neg log"
  (- (log (- (/ 1 x) 1))))

(lambda (B x)
  #:name "VandenBroeck and Keller, Equation (24)"
  (+ (- (* x (cotan B))) (/ 1 (sin B))))

(lambda (x)
  #:name "sqrt sqr"
  (- (/ x x) (* (/ 1 x) (sqrt (* x x))))
  #:target
  (if (< x 0) 2 0))

(lambda (a b c)
  #:name "jeff quadratic root 1"
  (let* ((d (sqrt (- (sqr b) (* 4 a c)))))
    (if (>= b 0)
      (/ (- (- b) d) (* 2 a))
      (/ (* 2 c) (+ (- b) d)))))

(lambda (a b c)
  #:name "jeff quadratic root 2"
  (let* ((d (sqrt (- (sqr b) (* 4 a c)))))
    (if (>= b 0)
      (/ (* 2 c) (- (- b) d))
      (/ (+ (- b) d) (* 2 a)))))
