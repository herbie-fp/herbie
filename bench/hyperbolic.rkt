#lang racket
(require casio/test)

(casio-bench (x)
  "Hyperbolic sine"
  (/ (- (exp x) (exp (- x))) 2))

(casio-bench (x)
  "Hyperbolic tangent"
  (/ (- (exp x) (exp (- x)))
     (+ (exp x) (exp (- x)))))

(casio-bench (x)
  "Hyperbolic secant"
  (/ 2 (+ (exp x) (exp (- x)))))

(casio-bench (x)
  "Hyperbolic arcsine"
  (log (+ x (sqrt (+ (sqr x) 1)))))

(casio-bench (x)
  "Hyperbolic arc-cosine"
  (log (+ x (sqrt (- (sqr x) 1)))))

(casio-bench (x)
  "Hyperbolic arc-(co)tangent"
  (* (/ 1 2) (log (/ (+ 1 x) (- 1 x)))))

(casio-bench (x)
  "Hyperbolic arc-(co)secant"
  (* (log (+ (/ 1 x) (/ (sqrt (- 1 (sqr x))) x)))))
