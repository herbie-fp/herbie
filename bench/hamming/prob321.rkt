#lang racket

(require casio/test)

(casio-bench (a b/2 c)
   "Hamming (NMSE) problem 3.2.1, the modified quadratic formula (first root)"
   (let* ((d (sqrt (- (* (* 2 b/2) (* 2 b/2)) (* 4 (* a c))))))
     (/ (+ (* -2 b/2) d) (* 2 a))))

(casio-bench (a b/2 c)
   "Hamming (NMSE) problem 3.2.1, the modified quadratic formula (second root)"
   (let* ((d (sqrt (- (* (* 2 b/2) (* 2 b/2)) (* 4 (* a c))))))
     (/ (+ (* -2 b/2) d) (* 2 a))))
