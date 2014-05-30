#lang racket
(require casio/test)

(casio-test (x)
  "NMSE section 3.11"
  (/ (exp x) (- (exp x) 1))
  (/ 1 (- 1 (exp (- x)))))
