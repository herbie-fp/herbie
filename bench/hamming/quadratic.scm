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
