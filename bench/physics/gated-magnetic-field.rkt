
; From Daniel Bulmash, Poisson solver for a gated particle in a magnetic field

(herbie-test (NdChar Ec Vef EDonor mu KbT NaChar Ev EAccept)
  "Bulmash initializePoisson"
  (+ (/ NdChar (+ 1 (exp (/ (- (- Ec Vef EDonor mu)) KbT))))
     (/ NaChar (+ 1 (exp (/ (+ Ev Vef EAccept (- mu)) KbT))))))
