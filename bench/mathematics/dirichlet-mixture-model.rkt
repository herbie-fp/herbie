
; From Harley Montgomery, a MCMC transition probability for a Dirichlet Mixture Model

(lambda ([c_p (positive default)] [c_n (positive default)] t s)
  #:name "Harley's example"
  (/ (* (pow (/ (+ 1 (exp (- s)))) c_p) (pow (- 1 (/ (+ 1 (exp (- s))))) c_n))
     (* (pow (/ (+ 1 (exp (- t)))) c_p) (pow (- 1 (/ (+ 1 (exp (- t))))) c_n)))
  #:target
  (* (pow (/ (+ 1 (exp (- t))) (+ 1 (exp (- s)))) c_p)
     (pow (/ (+ 1 (exp t)) (+ 1 (exp s))) c_n)))
