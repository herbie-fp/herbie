#lang racket
(require casio/test)

; Some tests on Casio's ability to reassociate and cancel
(casio-test (x y)
  "Commute"
  (- (+ x y) (+ y x))
  0)

(casio-test (x)
  "Associate"
  (- (+ 1 x) x)
  1)

(casio-test (x)
  "Associate and commute"
  (- (+ x 1) x)
  1)

(casio-test (x y z)
  "Commute and associate"
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

(casio-test (a b c)
  "A la Freetype's FT_MulDiv"
  (/ (+ a (/ c 2)) b))
