#lang racket

(require casio/test)

(casio-test (x)
  "NMSE example 3.1"
  (- (sqrt (+ x 1)) (sqrt x))
  (/ 1 (+ (sqrt (+ x 1)) (sqrt x))))

(casio-test (x)
  "NMSE example 3.2"
  (/ (sin x) x)
  (/ (sin x) x))

(casio-test (x eps)
  "NMSE example 3.3"
  (- (sin (+ x eps)) (sin x))
  (* 2 (* (cos (+ x (/ eps 2))) (sin (/ eps 2)))))

(casio-test (x)
  "NMSE example 3.4"
  (/ (- 1 (cos x)) (sin x))
  (tan (/ x 2)))

(casio-test (N)
  "NMSE example 3.5"
  (- (atan (+ N 1)) (atan N))
  (atan (/ 1 (+ 1 (* N (- N 1))))))

(casio-test (x)
  "NMSE example 3.6"
  (- (/ 1 (sqrt (+ x 1))) (/ 1 (sqrt x)))
  (/ -1 (+ (* (+ x 1) (sqrt x)) (* x (sqrt (+ x 1))))))

(casio-test (x)
  "NMSE problem 3.3.1"
  (- (/ 1 (+ x 1)) (/ 1 x))
  (/ -1 (* x (+ x 1))))

(casio-test (x eps)
  "NMSE problem 3.3.2"
  (- (tan (+ x eps)) (tan x))
  (/ (sin eps) (* (cos x) (cos (+ x eps)))))

(casio-test (x)
  "NMSE problem 3.3.3"
  (+ (- (/ 1 (+ x 1)) (/ 2 x)) (/ 1 (- x 1)))
  (/ 2 (* x (- (* x x) 1))))

(casio-test (x)
  "NMSE problem 3.3.4"
  (- (expt (+ x 1) (/ 1 3)) (expt x (/ 1 3)))
  (/ 1 (+ (+ (expt (+ x 1) (/ 2 3))
             (expt (* x (+ x 1)) (/ 1 3)))
          (expt x (/ 2 3)))))

(casio-test (x eps)
  "NMSE problem 3.3.5"
  (- (cos (+ x eps)) (cos x))
  (if (> (cos eps) .9)
      (- (cos (+ x eps)) (cos x))
      (- (* (cos x) (- (cos eps) 1)) (* (sin x) (sin eps)))))

(casio-test (N)
  "NMSE problem 3.3.6"
  (- (log (+ N 1)) (log N))
  (log (+ 1 (/ 1 N))))

(casio-test (x)
  "NMSE problem 3.3.7"
  (+ (- (exp x) 2) (exp (- x)))
  (* 4 (expt (sinh (/ x 2)) 2)))
