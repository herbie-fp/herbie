#lang racket
(require casio/test)

; Some tests on Casio's ability to reassociate and cancel
(casio-test (x y)
  "Cancel after commute"
  (- (+ x y) (+ y x))
  0)

(casio-test (x)
  "Cancel after associate"
  (- (+ 1 x) x)
  1)

(casio-test (x)
  "Cancel after associate and commutate"
  (- (+ x 1) x)
  1)

(casio-test (x y z)
  "Cancel after commute and associate"
  (- (+ (+ x y) z) (+ x (+ y z)))
  0)

(casio-test (a)
  "Expanding a square"
  (- (sqr (+ a 1)) 1)
  (* a (+ a 2)))

(casio-test (a b)
  "Difference of squares"
  (- (sqr a) (sqr b))
  (* (+ a b) (- a b)))
