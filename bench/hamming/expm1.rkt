#lang racket
(require casio/test)

(casio-test (a x)
  "Hamming (NMSE) example in section 3.5"
  (- (exp (* a x)) 1)
  (if (< (abs (* a x)) 1/10)
      (* (* a x) (+ 1 (+ (/ (* a x) 2) (/ (expt (* a x) 2) 6))))
      (- (exp (* a x)) 1)))
