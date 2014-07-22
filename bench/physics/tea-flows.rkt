#lang racket
(require casio/test)

; Pouring flows
; J. VandenBroeck and J. Keller
; Phys. Fluids 29, 3958 (1986), http://dx.doi.org/10.1063/1.865735

(casio-bench (F l)
  "VandenBroeck and Keller, Equation (6)"
  (- (* 3.141592653589793 l) (* (/ (sqr F)) (tan (* 3.141592653589793 l)))))

(casio-bench (f)
  "VandenBroeck and Keller, Equation (20)"
  (let* ([pi/4 (/ pi 4)]
         [exp+ (exp (* pi/4 f))]
         [exp- (exp (- (* pi/4 f)))])
    (- (* (/ pi/4) (log (/ (+ exp+ exp-) (- exp+ exp-)))))))

(casio-bench (F B x)
  "VandenBroeck and Keller, Equation (23)"
  (+ (- (* x (cotan B))) (* (/ F (sin B)) (expt (+ (sqr F) 2 (* 2 x)) (- (/ 1 2))))))

(casio-bench (B x)
  "VandenBroeck and Keller, Equation (24)"
  (+ (- (* x (cotan B))) (/ 1 (sin B))))
