
; Superfluidity breakdown and multiple roton gaps in spin-orbit-coupled Bose-Einstein condensates in an optical lattice
; D. Toniolo and J. Linder
; Phys. Rev. A, 061605 (2014), http://journals.aps.org/pra/abstract/10.1103/PhysRevA.89.061605

(herbie-test (t l Om Omc)
  "Toniolo and Linder, Equation (2)"
  (asin (sqrt (/ (- 1 (sqr (/ Om Omc))) (+ 1 (* 2 (sqr (/ t l))))))))

(herbie-test (l Om kx ky)
  "Toniolo and Linder, Equation (3a)"
  (sqrt (* (/ 1 2) (+ 1 (/ (sqrt (+ 1 (* (sqr (/ (* 2 l) Om))
                                         (+ (sqr (sin kx)) (sqr (sin ky)))))))))))

(herbie-test (kx ky th)
  "Toniolo and Linder, Equation (3b), real"
  (* (/ (sin ky) (sqrt (+ (sqr (sin kx)) (sqr (sin ky))))) (sin th)))

(herbie-test (x l t)
  "Toniolo and Linder, Equation (7)"
  (/ (* (sqrt 2) t) (sqrt (- (* (/ (+ x 1) (- x 1)) (+ (sqr l) (* 2 (sqr t)))) (sqr l)))))

(herbie-test (t l k)
  "Toniolo and Linder, Equation (10+)"
  (/ 2 (* (/ (pow t 3) (sqr l)) (sin k) (tan k) (+ (+ 1 (sqr (/ k t))) 1))))

(herbie-test (t l k)
  "Toniolo and Linder, Equation (10-)"
  (/ 2 (* (/ (pow t 3) (sqr l)) (sin k) (tan k) (- (+ 1 (sqr (/ k t))) 1))))

(herbie-test (n U t l Om U*)
  "Toniolo and Linder, Equation (13)"
  (sqrt (* 2 n U (- t (* 2 (/ (sqr l) Om)) (* n (sqr (/ l Om)) (- U U*))))))
