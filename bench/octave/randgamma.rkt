#lang racket
(require casio/test)

(casio-test (a rand)
  "Code from GNU Octave 3.8, randgamma.c, oct_fill_randg"
  ; Test on a > 1
  (let* ([d (- a (/ 1.0 3.0))]
         [c (/ 1 (sqrt (* 9 d)))]
         [x rand]
         [v (+ 1 (* c x))]
         [v* (* v (* v v))]
         [xsq (* x x)])
    ; TODO : why do we compare (u = RUNI) on line 125
    (* d v)))
