
; Most interesting point: 77617 33096
(herbie-test ([x 77617] [y 33096])
             "some thing zach found"
             (+ (* 333.75 (expt y 6))
                (* (sqr x)
                   (+ (* 11 (sqr x) (sqr y))
                      (- (expt y 6))
                      (* -121 (expt y 4))
                      -2))
                (* 5.5 (expt y 8))
                (/ x (* 2 y))))
