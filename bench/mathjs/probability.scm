; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/

; /lib/function/probability/random.js normal
(lambda (u1 u2)
  (+ (* (* (/ 1 6) (expt (* -2 (log u1)) 0.5)) (cos (* (* 2 pi) u2))) 0.5))
