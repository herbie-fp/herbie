
;; https://github.com/malyzajko/rosa/blob/master/testcases/real/techreport/NBody.scala
(herbie-test ([x0 (< (< -6 double) 6)] [y0 (< (< -6 double) 6)] [z0 (< (< -0.2 double) 0.2)]
              [vx0 (< (< -3 double) 3)] [vy0 (< (< -3 double) 3)] [vz0 (< (< -0.1 double) 0.1)]
              [i0 (integer-range 0 100)])
  "N Body Simulation"
  (let ([dt 0.1]
        [solarMass 39.47841760435743])
    (do ([x (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                   [mag (/ dt (* distance distance distance))]
                   [vxNew (- vx (* x solarMass mag))])
              (+ x (* dt vxNew)))]
         [y (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                   [mag (/ dt (* distance distance distance))]
                   [vyNew (- vy (* y solarMass mag))])
              (+ y (* dt vyNew)))]
         [z (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                   [mag (/ dt (* distance distance distance))]
                   [vzNew (- vz (* z solarMass mag))])
              (+ z (* dt vzNew)))]
         [vx (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                    [mag (/ dt (* distance distance distance))]
                    [vxNew (- vx (* x solarMass mag))])
               vxNew)]
         [vy (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                    [mag (/ dt (* distance distance distance))]
                    [vyNew (- vy (* y solarMass mag))])
               vyNew)]
         [vz (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                    [mag (/ dt (* distance distance distance))]
                    [vzNew (- vz (* z solarMass mag))])
               vzNew)]
         [i i0 (+ i 1)])
        (< i 100)
        x)))
