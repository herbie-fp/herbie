
; Thanks for walking through the Herbie tutorial.
; These are a few example expressions to run Herbie on.

; Each of the blocks below is a separate expression;
; the syntax is (herbie-test (vars ...) "name" input)

(herbie-test (x)         ; The (x) declares one variable, x
  "Cancel like terms"    ; This is the name of the test; file/lineno is common
  (- (+ 1 x) x))         ; The floating point expression to improve.

(herbie-test (x)
  "Expanding a square"
  (- (sqr (+ x 1)) 1))

(herbie-test (x y z)
  "Commute and associate"
  (- (+ (+ x y) z) (+ x (+ y z))))
