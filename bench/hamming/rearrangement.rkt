#lang racket

(require casio/test)

(casio-test (x)
  "Hamming (NMSE) example 3.1, √(x+1) - √x"
  (- (sqrt (+ x 1)) (sqrt x))
  (/ 1 (+ (sqrt (+ x 1)) (sqrt x))))

(casio-bench (x)
  "Hamming (NMSE) example 3.2, sin(x) / x"
  (if (= x 0)
      1
      (/ (sin x) x)))

(casio-test (x eps)
  "Hamming (NMSE) example 3.3, sin(x + ε) - sin(x)"
  (- (sin (+ x eps)) (sin x))
  (* 2 (* (cos (+ x (/ eps 2))) (sin (/ eps 2)))))

(casio-test (x)
  "Hamming (NMSE) example 3.4, (1 - cos(x)) / sin(x)"
  (/ (- 1 (cos x)) (sin x))
  (tan (/ x 2)))

(casio-test (N)
  "Hamming (NMSE) example 3.5, atan(N + 1) - atan(N)"
  (- (atan (+ N 1)) (atan N))
  (atan (/ 1 (+ 1 (* N (- N 1))))))

(casio-test (x)
  "Hamming (NMSE) example 3.6, 1/√(x + 1) - 1/√x"
  (- (/ 1 (sqrt (+ x 1))) (/ 1 (sqrt x)))
  (/ 1 (+ (* (+ x 1) (sqrt x)) (* x (sqrt (+ x 1))))))

(casio-bench (x)
  "Hamming (NMSE) problem 3.3.1, 1/(x + 1) - 1/x"
  (- (/ 1 (+ x 1)) (/ 1 x)))

(casio-test (x eps)
  "Hamming (NMSE) problem 3.3.2, tan(x + ε) - tan(x)"
  (- (tan (+ x eps)) (tan x))
  (/ (sin eps) (* (cos x) (cos (+ x eps)))))

(casio-test (x)
  "Hamming (NMSE) problem 3.3.3, 1/(x + 1) - 2/x + 1/(x - 1)"
  (+ (- (/ 1 (+ x 1)) (/ 2 x)) (/ 1 (- x 1)))
  (/ 2 (* x (- (* x x) 1))))

(casio-test (x)
  "Hamming (NMSE) problem 3.3.4, ³√(x + 1) - ³√x"
  (- (expt (+ x 1) (/ 1 3)) (expt x (/ 1 3))))

(casio-test (x eps)
  "Hamming (NMSE) problem 3.3.5, cos(x + ε) - cos(x)"
  (- (cos (+ x eps)) (cos x)))

(casio-bench (N)
  "Hamming (NMSE) problem 3.3.6, ln(N + 1) - ln(N)"
  (- (log (+ N 1)) (log N)))

(casio-test (x)
  "Hamming (NMSE) problem 3.3.7, e^x - 2 + e^-x"
  (+ (- (exp x) 2) (exp (- x)))
  (* 4 (expt (sinh (/ x 2)) 2)))
