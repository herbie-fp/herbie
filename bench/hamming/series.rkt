
(lambda ([x (< -0.00017 default)])
  #:name "NMSE example 3.7"
  (- (exp x) 1)
  #:target
  (* x (+ 1 (/ x 2) (/ (sqr x) 6))))

(lambda ([N (> default 6.8e15)])
  #:name "NMSE example 3.8"
  (- (* (+ N 1) (log (+ N 1))) (* N (log N)) 1)
  #:target
  (- (log (+ N 1)) (- (/ 1 (* 2 N)) (- (/ 1 (* 3 (sqr N))) (/ 4 (expt N 3)))))
  #:expected #f)

(lambda ([x (< -.026 default .026)])
  #:name "NMSE example 3.9"
  (- (/ 1 x) (cotan x))
  #:target
  (if (< (abs x) .026)
      (* (/ x 3) (+ 1 (/ (sqr x) 15)))
      (- (/ 1 x) (cotan x)))
  #:expected #f)

(lambda ([x (< -1 default 1)])
  #:name "NMSE example 3.10"
  (/ (log (- 1 x)) (log (+ 1 x)))
  #:target
  (- (+ 1 x (/ (sqr x) 2) (* 5/12 (expt x 3))))
  #:expected #f)

(lambda (x)
  #:name "NMSE problem 3.4.1"
  (/ (- 1 (cos x)) (sqr x)))

(lambda (a b [eps (< -1 default 1)])
  #:name "NMSE problem 3.4.2"
  (/ (* eps (- (exp (* (+ a b) eps)) 1))
     (* (- (exp (* a eps)) 1) (- (exp (* b eps)) 1)))
  #:target
  (/ (+ a b) (* a b)))

(lambda (eps)
  #:name "NMSE problem 3.4.3"
  (log (/ (- 1 eps) (+ 1 eps)))
  #:target
  (* -2 (+ eps (/ (expt eps 3) 3) (/ (expt eps 5) 5))))

(lambda (x)
  #:name "NMSE problem 3.4.4"
  (sqrt (/ (- (exp (* 2 x)) 1) (- (exp x) 1))))

(lambda (x)
  #:name "NMSE problem 3.4.5"
  (/ (- x (sin x)) (- x (tan x))))

(lambda (x n)
  #:name "NMSE problem 3.4.6"
  (- (expt (+ x 1) (/ 1 n)) (expt x (/ 1 n))))
