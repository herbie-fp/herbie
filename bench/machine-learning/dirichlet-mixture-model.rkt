
; From Harley Montgomery, a MCMC transition probability for a Dirichlet Mixture Model

(herbie-test ((c_p (positive default)) (c_n (positive default)) t s)
  "Harley's example"
  (/ (* (expt (/ (+ 1 (exp (- s)))) e+) (expt (- 1 (/ (+ 1 (exp (- s))))) e-))
     (* (expt (/ (+ 1 (exp (- t)))) e+) (expt (- 1 (/ (+ 1 (exp (- t))))) e-)))
  (* (expt (/ (+ 1 (exp (- t))) (+ 1 (exp (- s)))) e+)
     (expt (/ (+ 1 (exp t)) (+ 1 (exp s))) e-)))
