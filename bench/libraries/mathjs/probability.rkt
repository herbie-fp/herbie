(FPCore
 ((u1 (uniform 0 1)) (u2 (uniform 0 1)))
 :name
 "normal distribution"
 (+ (* (* (/ 1 6) (pow (* -2 (log u1)) 0.5)) (cos (* (* 2 PI) u2))) 0.5))
