(FPCore
 ((alpha (> default -1)) (beta (> default -1)))
 :name
 "Octave 3.8, jcobi/1"
 (let ((ab (+ alpha beta)))
   (let ((ad (- beta alpha)))
     (let ((ap (* beta alpha))) (/ (+ (/ ad (+ ab 2.0)) 1.0) 2.0)))))
(FPCore
 ((alpha (> default -1)) (beta (> default -1)) (i (> integer 0)))
 :name
 "Octave 3.8, jcobi/2"
 (let ((ab (+ alpha beta)))
   (let ((ad (- beta alpha)))
     (let ((ap (* beta alpha)))
       (let ((z (+ ab (* 2 i)))) (/ (+ (/ (* ab ad) z (+ z 2.0)) 1.0) 2.0))))))
(FPCore
 ((alpha (> default -1)) (beta (> default -1)))
 :name
 "Octave 3.8, jcobi/3"
 (let ((i 1))
   (let ((ab (+ alpha beta)))
     (let ((ad (- beta alpha)))
       (let ((ap (* beta alpha)))
         (let ((z1 i))
           (let ((z (+ ab (* 2 z1)))) (/ (+ ab ap 1.0) z z (+ z 1.0)))))))))
(FPCore
 ((alpha (> default -1)) (beta (> default -1)) (i (> integer 1)))
 :name
 "Octave 3.8, jcobi/4"
 (let ((ab (+ alpha beta)))
   (let ((ad (- beta alpha)))
     (let ((ap (* beta alpha)))
       (let ((z (+ ab (* 2 i))))
         (let ((z* (* z z)))
           (let ((y (* i (+ ab i))))
             (let ((y* (* y (+ ap y)))) (/ y* z* (- z* 1.0))))))))))
(FPCore
 ((i (> integer 0)))
 :name
 "Octave 3.8, jcobi/4, as called"
 (let ((z (* 2 i)))
   (let ((z* (* z z)))
     (let ((y (* i i))) (let ((y* (* y y))) (/ y* z* (- z* 1.0)))))))
