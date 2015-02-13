
; Some tests on Herbie's ability to reassociate and cancel
(herbie-test (x y)
  "Commute"
  (- (+ x y) (+ y x))
  0)

(herbie-test (x)
  "Associate"
  (- (+ 1 x) x)
  1)

(herbie-test (x)
  "Associate and commute"
  (- (+ x 1) x)
  1)

(herbie-test (x y z)
  "Commute and associate"
  (- (+ (+ x y) z) (+ x (+ y z)))
  0)

(herbie-test (a)
  "Expanding a square"
  (- (sqr (+ a 1)) 1)
  (* a (+ a 2)))

(herbie-test (a b c)
  "A la Freetype's FT_MulDiv"
  (/ (+ a (/ c 2)) b))
