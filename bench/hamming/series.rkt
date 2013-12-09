#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) example 3.7, e^x - x"
  (- (exp x) x))

(casio-bench (N)
  "Hamming (NMSE) example 3.8, (N+1) ln(N+1) - N ln(N) - 1"
  (- (- (* (+ N 1) (log (+ N 1))) (* N (log N))) 1))

(casio-bench (x)
  "Hamming (NMSE) example 3.9, 1/x - ctn(x)"
  (- (/ 1 x) (cotan x)))

(casio-bench (x)
  "Hamming (NMSE) example 3.10, ln(1 - x)/ln(1+x)"
  (/ (log (- 1 x)) (log (+ 1 x))))

(casio-bench (x)
  "Hamming (NMSE) problem 3.4.1, (1 - (cos x)) / x^2"
  (/ (- 1 (cos x)) (expt x 2)))

(casio-bench (a b eps)
  "Hamming (NMSE) problem 3.4.2"
  (/ (* eps (- (exp (* (+ a b) eps)) 1))
     (* (- (exp (* a eps)) 1) (- (exp (* b eps)) 1))))

(casio-bench (x)
  "Hamming (NMSE) problem 3.4.3, log(1 - x / 1 + x)"
  (log (/ (- 1 x) (+ 1 x))))

(casio-bench (x)
  "Hamming (NMSE) problem 3.4.4, √(e^2x - 1 / e^x - 1)"
  (sqrt (/ (- (exp (* 2 x)) 1) (- (exp x) 1))))

(casio-bench (x)
  "Hamming (NMSE) problem 3.4.5, (x - sin(x)) / (x - tan(x))"
  (/ (- x (sin x)) (- x (tan x))))

(casio-bench (x n)
  "Hamming (NMSE) problem 3.4.6, (x + 1)^(1/n) - x^(1/n)"
  (- (expt (+ x 1) (/ 1 n)) (expt x (/ 1 n))))
