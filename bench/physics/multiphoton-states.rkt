
; Multiphoton states related via linear optics
; P. Migdal, J. Rodrigues-Laguna, M. Oszmaniec, M. Lewenstein
; Phys. Rev. A 89, 062329 (2014), http://journals.aps.org/pra/abstract/10.1103/PhysRevA.89.062329

(herbie-test (k n)
  "Migdal et al, Equation (51)"
  (* (/ (sqrt k)) (pow (* 2 PI n) (/ (- 1 k) 2))))

(herbie-test (a1 a2 th)
  "Migdal et al, Equation (64)"
  (+ (* (/ (cos th) (sqrt 2)) (sqr a1))
     (* (/ (cos th) (sqrt 2)) (sqr a2))))
