#lang racket
(require casio/test)

(casio-test (x)
  "NMSE example 3.7"
  (- (exp x) 1))

(casio-test (N)
  "NMSE example 3.8"
  (- (- (* (+ N 1) (log (+ N 1))) (* N (log N))) 1))

(casio-test (x)
  "NMSE example 3.9"
  (- (/ 1 x) (cotan x)))

(casio-test (x)
  "NMSE example 3.10"
  (/ (log (- 1 x)) (log (+ 1 x))))

(casio-test (x)
  "NMSE problem 3.4.1"
  (/ (- 1 (cos x)) (expt x 2))
  (/ (sqr (sin x)) (* (+ 1 (cos x)) (sqr x))))

(casio-test (a b eps)
  "NMSE problem 3.4.2"
  (/ (* eps (- (exp (* (+ a b) eps)) 1))
     (* (- (exp (* a eps)) 1) (- (exp (* b eps)) 1))))

(casio-test (x)
  "NMSE problem 3.4.3"
  (log (/ (- 1 x) (+ 1 x))))

(casio-test (x)
  "NMSE problem 3.4.4"
  (sqrt (/ (- (exp (* 2 x)) 1) (- (exp x) 1))))

(casio-test (x)
  "NMSE problem 3.4.5"
  (/ (- x (sin x)) (- x (tan x))))

(casio-test (x n)
  "NMSE problem 3.4.6"
  (- (expt (+ x 1) (/ 1 n)) (expt x (/ 1 n))))
