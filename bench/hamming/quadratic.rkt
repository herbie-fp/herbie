#lang racket

(require casio/test)

; quadratic formula
(casio-test (a b c)
  "Hamming (NMSE) p42, the quadratic formula (first root)"
   (let* ((d (sqrt (- (* b b) (* 4 (* a c))))))
     (/ (+ (- b) d) (* 2 a)))
   (let* ((d (sqrt (- (* b b) (* 4 (* a c)))))
          (r1 (/ (+ (- b) d) (* 2 a)))
          (r2 (/ (- (- b) d) (* 2 a))))
     (if (< b 0)
         r1
         (/ c (* a r2)))))

(casio-test (a b c)
  "Hamming (NMSE) p42, the quadratic formula (second root)"
   (let* ((d (sqrt (- (* b b) (* 4 (* a c))))))
     (/ (+ (- b) d) (* 2 a)))
   (let* ((d (sqrt (- (* b b) (* 4 (* a c)))))
          (r1 (/ (+ (- b) d) (* 2 a)))
          (r2 (/ (- (- b) d) (* 2 a))))
     (if (< b 0)
         r2
         (/ c (* a r1)))))

(casio-bench (a b/2 c)
   "Hamming (NMSE) problem 3.2.1, the modified quadratic formula (first root)"
   (let* ((d (sqrt (- (* (* 2 b/2) (* 2 b/2)) (* 4 (* a c))))))
     (/ (+ (* -2 b/2) d) (* 2 a))))

(casio-bench (a b/2 c)
   "Hamming (NMSE) problem 3.2.1, the modified quadratic formula (second root)"
   (let* ((d (sqrt (- (* (* 2 b/2) (* 2 b/2)) (* 4 (* a c))))))
     (/ (+ (* -2 b/2) d) (* 2 a))))
