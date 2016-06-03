
; From the GNU Octave 3.8 release, in file randgamma.c

(herbie-test (a rand)
  "Octave 3.8, oct_fill_randg"
  ; Test on a > 1
  (let* ([d (- a (/ 1.0 3.0))]
         [c (/ 1 (sqrt (* 9 d)))]
         [x rand]
         [v (+ 1 (* c x))]
         [v* (* v (* v v))]
         [xsq (* x x)])
    ; TODO : why do we compare (u = RUNI) on line 125
    (* d v)))
