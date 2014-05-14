#lang racket
(require casio/test)

; Some tests on Casio's ability to reassociate and cancel
(casio-test (x y)
  "Canceling after commutation"
  (- (+ x y) (+ y x))
  0)

(casio-test (x)
  "Canceling after reassociation"
  (- (+ 1 x) x)
  1)

(casio-test (x)
  "Canceling after reassociation and commutation"
  (- (+ x 1) x)
  1)

(casio-test (x y z)
  "Canceling after commutation and reassociation"
  (- (+ (+ x y) z) (+ x (+ y z)))
  0)

(casio-test (a)
  "Reducing (a+1)^2 - 1"
  (- (sqr (+ a 1)) 1)
  (* a (+ a 2)))

(casio-test (a b)
  "Reducing a^2 - b^2"
  (- (sqr a) (sqr b))
  (* (+ a b) (- a b)))
