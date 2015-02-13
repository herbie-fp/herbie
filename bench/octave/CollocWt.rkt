
;; From the GNU Octave 3.8 release, in file CollocWt.cc

(herbie-test (alpha beta)
  "Octave 3.8, jcobi/1"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)])
    (/ (+ (/ ad (+ ab 2.0)) 1.0) 2.0)))

(herbie-test (alpha beta [i (positive integer)])
  "Octave 3.8, jcobi/2"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z (+ ab (* 2 i))])
    (/ (+ (/ (* ab ad) z (+ z 2.0)) 1.0) 2.0)))

(herbie-test (alpha beta [i (positive integer)])
  "Octave 3.8, jcobi/3"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z (+ ab (* 2 i))])
    (/ (+ ab ap 1.0) z z (+ z 1.0))))

(herbie-test (alpha beta [i (positive integer)])
  "Octave 3.8, jcobi/4"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z (+ ab (* 2 i))]
         [z* (* z z)]
         [y (* i (+ ab i))]
         [y* (* y (+ ap y))])
    (/ y* z* (- z* 1.0))))

