
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

; Suggested by Arvind

(herbie-test (a b)
  "Exp of sum of logs"
  (exp (+ (log a) (log b)))
  (* a b))

(herbie-test (a b)
  "Quotient of sum of exps"
  (/ (exp a) (+ (exp a) (exp b)))
  (/ 1 (+ 1 (exp (- b a)))))

(herbie-test (a1 a2 b1 b2)
  "Quotient of products"
  (/ (* a1 a2) (* b1 b2))
  (* (/ a1 b1) (/ a2 b2)))

; From Xi Wang

(herbie-test (a b c)
  "A la Freetype's FT_MulDiv"
  (/ (+ a (/ c 2)) b))
