#lang racket
(require herbie/test)

(casio-test (x)
  "Hyperbolic sine"
  (/ (- (exp x) (exp (- x))) 2))

(casio-test (x)
  "Hyperbolic tangent"
  (/ (- (exp x) (exp (- x)))
     (+ (exp x) (exp (- x)))))

(casio-test (x)
  "Hyperbolic secant"
  (/ 2 (+ (exp x) (exp (- x)))))

(casio-test (x)
  "Hyperbolic arcsine"
  (log (+ x (sqrt (+ (sqr x) 1))))
  (if (< x 0)
      (log (/ -1 (- x (sqrt (+ (sqr x) 1)))))
      (log (+ x (sqrt (+ (sqr x) 1))))))

(casio-test (x)
  "Hyperbolic arc-cosine"
  (log (+ x (sqrt (- (sqr x) 1)))))

(casio-test (x)
  "Hyperbolic arc-(co)tangent"
  (* (/ 1 2) (log (/ (+ 1 x) (- 1 x)))))

(casio-test (x)
  "Hyperbolic arc-(co)secant"
  (log (+ (/ 1 x) (/ (sqrt (- 1 (sqr x))) x))))
