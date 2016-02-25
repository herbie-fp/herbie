(lambda ((x default) (y default) (z default) (t default) (a default) (b default) (c default))
  #:name "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2"
  (/ x (+ x (* y (exp (* 2.0 (- (/ (* z (sqrt (+ t a))) t) (* (- b c) (- (+ a (/ 5.0 6.0)) (/ 2.0 (* t 3.0)))))))))))

(lambda ((x default) (y default) (z default) (t default) (a default) (b default))
  #:name "Numeric.SpecFunctions:incompleteBetaWorker from math-functions-0.1.5.2"
  (/ (* x (exp (- (+ (* y (log z)) (* (- t 1.0) (log a))) b))) y))

