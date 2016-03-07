
; Pouring flows
; J. VandenBroeck and J. Keller
; Phys. Fluids 29, 3958 (1986), http://dx.doi.org/10.1063/1.865735

(herbie-test (F l)
  "VandenBroeck and Keller, Equation (6)"
  (- (* pi l) (* (/ (sqr F)) (tan (* pi l)))))

(herbie-test (f)
  "VandenBroeck and Keller, Equation (20)"
  (let* ([pi/4 (/ pi 4)]
         [exp+ (exp (* pi/4 f))]
         [exp- (exp (- (* pi/4 f)))])
    (- (* (/ pi/4) (log (/ (+ exp+ exp-) (- exp+ exp-)))))))

(herbie-test (F B x)
  "VandenBroeck and Keller, Equation (23)"
  (+ (- (* x (cotan B))) (* (/ F (sin B)) (pow (+ (sqr F) 2 (* 2 x)) (- (/ 1 2))))))

(herbie-test (B x)
  "VandenBroeck and Keller, Equation (24)"
  (+ (- (* x (cotan B))) (/ 1 (sin B))))
