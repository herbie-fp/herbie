
;; From the GNU Octave 3.8 release, in file CollocWt.cc

(herbie-test ([alpha (> default -1)] [beta (> default -1)])
  "Octave 3.8, jcobi/1"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)])
    (/ (+ (/ ad (+ ab 2.0)) 1.0) 2.0)))

(herbie-test ([alpha (> default -1)] [beta (> default -1)] [i (> integer 0)])
  "Octave 3.8, jcobi/2"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z (+ ab (* 2 i))])
    (/ (+ (/ (* ab ad) z (+ z 2.0)) 1.0) 2.0)))

(herbie-test ([alpha (> default -1)] [beta (> default -1)])
  "Octave 3.8, jcobi/3"
  (let* ([i 1]
         [ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z1 i]
         [z (+ ab (* 2 z1))])
    (/ (+ ab ap 1.0) z z (+ z 1.0))))

(herbie-test ([alpha (> default -1)] [beta (> default -1)] [i (> integer 1)])
  "Octave 3.8, jcobi/4"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z (+ ab (* 2 i))]
         [z* (* z z)]
         [y (* i (+ ab i))]
         [y* (* y (+ ap y))])
    (/ y* z* (- z* 1.0))))

; As called by Octave; as in, with alpha and beta set to 0

(herbie-test ([i (> integer 0)])
  "Octave 3.8, jcobi/4, as called"
  (let* ([z (* 2 i)]
         [z* (* z z)]
         [y (* i i)]
         [y* (* y y)])
    (/ y* z* (- z* 1.0))))
