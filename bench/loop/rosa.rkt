
;; https://github.com/malyzajko/rosa/blob/master/testcases/real/techreport/NBody.scala
(herbie-test ([x0 (< (< -6 double) 6)] [y0 (< (< -6 double) 6)] [z0 (< (< -0.2 double) 0.2)]
              [vx0 (< (< -3 double) 3)] [vy0 (< (< -3 double) 3)] [vz0 (< (< -0.1 double) 0.1)]
              [i0 (integer-range 0 100)])
  "N Body Simulation"
  (let ([dt 0.1]
        [solarMass 39.47841760435743])
    (do ([x x0 (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                      [mag (/ dt (* distance distance distance))]
                      [vxNew (- vx (* x solarMass mag))])
                 (+ x (* dt vxNew)))]
         [y y0 (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                      [mag (/ dt (* distance distance distance))]
                      [vyNew (- vy (* y solarMass mag))])
                 (+ y (* dt vyNew)))]
         [z z0 (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                      [mag (/ dt (* distance distance distance))]
                      [vzNew (- vz (* z solarMass mag))])
                 (+ z (* dt vzNew)))]
         [vx vx0 (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                        [mag (/ dt (* distance distance distance))]
                        [vxNew (- vx (* x solarMass mag))])
                   vxNew)]
         [vy vy0 (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                        [mag (/ dt (* distance distance distance))]
                        [vyNew (- vy (* y solarMass mag))])
                   vyNew)]
         [vz vz0 (let* ([distance (sqrt (+ (* x x) (* y y) (* z z)))]
                        [mag (/ dt (* distance distance distance))]
                        [vzNew (- vz (* z solarMass mag))])
                   vzNew)]
         [i i0 (+ i 1)])
        (< i 100)
        x)))

;; https://github.com/malyzajko/rosa/blob/master/testcases/real/techreport/Pendulum.scala
(herbie-test ([t0 (< (< -2 double) 2)] [w0 (< (< -5 double) 5)] [N (integer-range 0 1000)])
  "Pendulum"
  (let ([h 0.01]
        [L 2.0]
        [m 1.5]
        [g 9.80665])
    (do ([t t0 (let* ([k1w (* (/ (- g) L) (sin t))]
                      [k2t (+ w (* (/ h 2) k1w))])
                 (+ t (* h k2t)))]
         [w w0 (let* ([k2w (* (/ (- g) L) (sin (+ t (* (/ h 2) w))))])
                 (+ w (* h k2w)))]
         [n 0 (+ n 1)])
        (< n N)
      t)))

;; https://github.com/malyzajko/rosa/blob/master/testcases/real/techreport/SineNewton.scala
(herbie-test ([x0 (< (< -1.2 double) 1.2)])
  "Sine Newton"
  (do ([x x0 (- x (/ (+ (+ (- x (/ (expt x 3) 6.0)) (/ (expt x 5) 120.0)) (/ (expt x 7) 5040.0))
                     (+ (+ (- 1.0 (/ (* x x) 2.0)) (/ (expt x 4) 24.0)) (/ (expt x 6) 720.0))))]
       [i 0 (+ i 1)])
      (< i 10)
    x))


