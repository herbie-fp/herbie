#lang racket

(require casio/test)

(casio-test (a b c)
  "The quadratic formula"

  (/ (+ (- b) (sqrt (- (sqr b) (* (* 4 a) c))))
     (* 2 a))

  (if (> b 0)
      (/ (* -2 c)
         (+ b (sqrt (- (sqr b) (* (* 4 a) c)))))
      (/ (+ (- b) (sqrt (- (sqr b) (* (* 4 a) c))))
         (* 2 a))))
