#lang racket

(require casio/test)

; quadratic formula
(casio-test (a b c)
  "NMSE p42"
   (let* ((d (sqrt (- (* b b) (* 4 (* a c))))))
     (/ (+ (- b) d) (* 2 a)))
   (let* ((d (sqrt (- (* b b) (* 4 (* a c)))))
          (r1 (/ (+ (- b) d) (* 2 a)))
          (r2 (/ (- (- b) d) (* 2 a))))
     (if (< b 0)
         r1
         (/ c (* a r2)))))

(casio-test (a b c)
  "NMSE p42"
   (let* ((d (sqrt (- (* b b) (* 4 (* a c))))))
     (/ (- (- b) d) (* 2 a)))
   (let* ((d (sqrt (- (* b b) (* 4 (* a c)))))
          (r1 (/ (+ (- b) d) (* 2 a)))
          (r2 (/ (- (- b) d) (* 2 a))))
     (if (< b 0)
         (/ c (* a r1))
         r2)))

(casio-bench (a b/2 c)
   "NMSE problem 3.2.1"
   (let* ((d (sqrt (- (* (* 2 b/2) (* 2 b/2)) (* 4 (* a c))))))
     (/ (+ (* -2 b/2) d) (* 2 a))))

(casio-bench (a b/2 c)
   "NMSE problem 3.2.1"
   (let* ((d (sqrt (- (* (* 2 b/2) (* 2 b/2)) (* 4 (* a c))))))
     (/ (+ (* -2 b/2) d) (* 2 a))))
