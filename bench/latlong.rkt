
;; From http://www.movable-type.co.uk/scripts/latlong.html

(herbie-test (R lambda1 lambda2 phi1 phi2)
  "Distance on a great circle"
  (let* ([dlambda (- lambda1 lambda2)]
         [dphi (- phi1 phi2)]
         [a (+ (sqr (sin (/ dphi 2)))
               (* (cos phi1) (cos phi2)
                  (sin (/ dlambda 2)) (sin (/ dlambda 2))))]
         [c (* 2 (atan2 (sqrt a) (sqrt (- 1 a))))]
         [d (* R c)])
    d))

(herbie-test (R lambda1 lambda2 phi1 phi2)
  "Spherical law of cosines"
  (* (acos (+ (* (sin phi1) (sin phi2))
              (* (cos phi1) (cos phi2) (cos (- lambda1 lambda2)))))
     R))

(herbie-test (R lambda1 lambda2 phi1 phi2)
  "Equirectangular approximation to distance on a great circle"
  (let* ([x (* (- lambda1 lambda2) (cos (/ (+ phi1 phi2) 2)))]
         [y (- phi1 phi2)]
         [d (* R (sqrt (+ (sqr x) (sqr y))))])
    d))

(herbie-test (lambda1 lambda2 phi1 phi2)
  "Bearing on a great circle"
  (atan2 (* (sin (- lambda1 lambda2)) (cos phi2))
         (- (* (cos phi1) (sin phi2))
            (* (sin phi1) (cos phi2) (cos (- lambda1 lambda2))))))

(herbie-test (lambda1 lambda2 phi1 phi2)
  "Midpoint on a great circle"
  (let* ([dlambda (- lambda1 lambda2)]
         [Bx (* (cos phi2) (cos dlambda))]
         [By (* (cos phi2) (sin dlambda))]
         [phim (atan2 (+ (sin phi1 phi2) (sqrt (+ (sqr (+ (cos phi1) Bx)) (sqr By)))))]
         [lambdam (+ lambda1 (atan2 By (+ (cos phi1) Bx)))])
    lambdam))

(herbie-test (lambda1 phi1 phi2 delta theta)
  "Destination given bearing on a great circle"
  (let* ([phi2 (asin (+ (* (sin phi1) (cos delta))
                        (* (cos phi1) (sin delta) (cos theta))))]
         [lambda2 (+ lambda1 (atan2 (* (sin theta) (sin delta) (cos phi1))
                                    (- (cos delta) (* (sin phi1) (sin phi2)))))])
    lambda2))

;; Up to intersection of two paths given start points and bearings
