
; Simplistic definitions of the hyperbolic functions;
; the goal is to derive accurate forms automatically

(herbie-test (x)
  "Hyperbolic sine"
  (/ (- (exp x) (exp (- x))) 2))

(herbie-test (x)
  "Hyperbolic tangent"
  (/ (- (exp x) (exp (- x)))
     (+ (exp x) (exp (- x)))))

(herbie-test (x)
  "Hyperbolic secant"
  (/ 2 (+ (exp x) (exp (- x)))))

(herbie-test (x)
  "Hyperbolic arcsine"
  (log (+ x (sqrt (+ (sqr x) 1))))
  (if (< x 0)
      (log (/ -1 (- x (sqrt (+ (sqr x) 1)))))
      (log (+ x (sqrt (+ (sqr x) 1))))))

(herbie-test (x)
  "Hyperbolic arc-cosine"
  (log (+ x (sqrt (- (sqr x) 1)))))

(herbie-test (x)
  "Hyperbolic arc-(co)tangent"
  (* (/ 1 2) (log (/ (+ 1 x) (- 1 x)))))

(herbie-test (x)
  "Hyperbolic arc-(co)secant"
  (log (+ (/ 1 x) (/ (sqrt (- 1 (sqr x))) x))))
