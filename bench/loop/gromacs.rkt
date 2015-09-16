
;; inner.c L267
(herbie-test ([dxs (list 20 double .25)] [dys (list 20 double .25)] [dzs (list 20 double .25)]
              [c1 double] [c2 double])
  "Gromacs inner loop"
  (do-list ([fx 0.0 (let* ([r2 (+ (sqr dx) (sqr dy) (sqr dz))]
                           [ir2 (/ 1 r2)]
                           [ir6 (* ir2 ir2 ir2)]
                           [ir12 (* ir6 ir6)]
                           [ve (* (- (* 12 (* c1 ir12)) (* 6 (* c2 ir6))) ir2)])
                      (+ fx (* dx ve)))]
            [fy 0.0 (let* ([r2 (+ (sqr dx) (sqr dy) (sqr dz))]
                           [ir2 (/ 1 r2)]
                           [ir6 (* ir2 ir2 ir2)]
                           [ir12 (* ir6 ir6)]
                           [ve (* (- (* 12 (* c1 ir12)) (* 6 (* c2 ir6))) ir2)])
                      (+ fy (* dy ve)))]
            [fz 0.0 (let* ([r2 (+ (sqr dx) (sqr dy) (sqr dz))]
                           [ir2 (/ 1 r2)]
                           [ir6 (* ir2 ir2 ir2)]
                           [ir12 (* ir6 ir6)]
                           [ve (* (- (* 12 (* c1 ir12)) (* 6 (* c2 ir6))) ir2)])
                      (+ fz (* dz ve)))])
           ([dx dxs]
            [dy dys]
            [dz dzs])
           (+ fx fy fz)))
