
; Generation of universal linear optics by any beam splitter
; A. Bouland and S. Aaronson
; Phys. Rev. A 89, 062316 (2014), http://link.aps.org/doi/10.1103/PhysRevA.89.062316

(herbie-test (a b)
  "Bouland and Aaronson, Equation (24)"
  (- (+ (sqr (+ (sqr a) (sqr b))) (* 4 (+ (* (sqr a) (- 1 a)) (* (sqr b) (+ 3 a))))) 1))

(herbie-test (a b)
  "Bouland and Aaronson, Equation (25)"
  (- (+ (sqr (+ (sqr a) (sqr b))) (* 4 (+ (* (sqr a) (+ 1 a)) (* (sqr b) (- 1 (* 3 a)))))) 1))

(herbie-test (a b)
  "Bouland and Aaronson, Equation (26)"
  (- (+ (sqr (+ (sqr a) (sqr b))) (* 4 (sqr b))) 1))
