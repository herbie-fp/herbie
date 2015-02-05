#lang racket
(require "../herbie/test.rkt")

;; From Alexey Radul, in personal communication:
;
; This formula has a singular line at x=0, a=1, and another at x=1,
; b=1. In log space that manifests as log(0) * 0 = -inf * 0 = NaN,
; which is completely not the answer I want---when a=1 (a common
; parameter setting in Venture programs), I just want the x^{a-1} term
; dropped.
 
(herbie-test (a b c)
  "The Beta distribution, using Wikipedia's parameterization"
  (* (expt x (- a 1))
     (expt (- x 1) (- b 1))
     (/ (gamma (+ a b))
        (* (gamma a) (gamma b)))))
