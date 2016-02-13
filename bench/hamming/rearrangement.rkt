
(lambda (x)
  #:name "NMSE example 3.1"
  (- (sqrt (+ x 1)) (sqrt x))
  #:target
  (/ 1 (+ (sqrt (+ x 1)) (sqrt x))))

(lambda (x eps)
  #:name "NMSE example 3.3"
  (- (sin (+ x eps)) (sin x))
  #:target
  (* 2 (* (cos (+ x (/ eps 2))) (sin (/ eps 2)))))

(lambda (x)
  #:name "NMSE example 3.4"
  (/ (- 1 (cos x)) (sin x))
  #:target
  (tan (/ x 2)))

(lambda (N)
  #:name "NMSE example 3.5"
  #:expected #f
  (- (atan (+ N 1)) (atan N))
  #:target
  (atan (/ 1 (+ 1 (* N (+ N 1))))))

(lambda (x)
  #:name "NMSE example 3.6"
  (- (/ 1 (sqrt x)) (/ 1 (sqrt (+ x 1))))
  #:target
  (/ 1 (+ (* (+ x 1) (sqrt x)) (* x (sqrt (+ x 1))))))

(lambda (x)
  #:name "NMSE problem 3.3.1"
  (- (/ 1 (+ x 1)) (/ 1 x)))

(lambda (x eps)
  #:name "NMSE problem 3.3.2"
  (- (tan (+ x eps)) (tan x))
  #:target
  (/ (sin eps) (* (cos x) (cos (+ x eps))))
  #:expected #f)

(lambda (x)
  #:name "NMSE problem 3.3.3"
  (+ (- (/ 1 (+ x 1)) (/ 2 x)) (/ 1 (- x 1)))
  #:target
  (/ 2 (* x (- (sqr x) 1)))
  #:expected #f)

(lambda (x)
  #:name "NMSE problem 3.3.4"
  (- (expt (+ x 1) (/ 1 3)) (expt x (/ 1 3))))

(lambda (x eps)
  #:name "NMSE problem 3.3.5"
  (- (cos (+ x eps)) (cos x)))

(lambda (N)
  #:name "NMSE problem 3.3.6"
  (- (log (+ N 1)) (log N)))

(lambda (x)
  #:name "NMSE problem 3.3.7"
  (+ (- (exp x) 2) (exp (- x)))
  #:target
  (* 4 (sqr (sinh (/ x 2)))))
