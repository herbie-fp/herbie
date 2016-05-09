
(herbie-test (x eps)
  "NMSE Section 6.1 mentioned"
  (/ (- (* (+ 1 (/ 1 eps)) (exp (- (* (- 1 eps) x))))
        (* (- (/ 1 eps) 1) (exp (- (* (+ 1 eps) x)))))
     2))

;; NMSE Section 6.1
(herbie-test (a b)
  "NMSE Section 6.1 mentioned"
  (* (/ PI 2) (/ 1 (- (sqr b) (sqr a))) (- (/ 1 a) (/ 1 b))))

(herbie-test (x y)
  "Radioactive exchange between two surfaces"
  (- (pow x 4) (pow y 4)))
