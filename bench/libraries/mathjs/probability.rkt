
; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/
; from files in
; /lib/function/probability/

(herbie-test ([u1 (uniform 0 1)] [u2 (uniform 0 1)])
  "normal distribution"
  (+ (* (* (/ 1 6) (pow (* -2 (log u1)) 0.5))
        (cos (* (* 2 pi) u2))) 0.5))
