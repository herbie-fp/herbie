
; Weak limit of the three-state quantum walk on the line
; S. Falkner and S. Boettcher
; Phys. Rev. A, 012307 (2014), http://link.aps.org/doi/10.1103/PhysRevA.90.012307

(herbie-test (v t)
  "Falkner and Boettcher, Equation (20:1,3)"
  (/ (- 1 (* 5 (sqr v))) (* pi t (sqrt (* 2 (- 1 (* 3 (sqr v))))) (- 1 (sqr v)))))

(herbie-test (v)
  "Falkner and Boettcher, Equation (22+)"
  (/ 4 (* 3 pi (- 1 (sqr v)) (sqrt (- 2 (* 6 (sqr v)))))))

(herbie-test (a k m)
  "Falkner and Boettcher, Appendix A"
  (/ (* a (pow k m)) (+ 1 (* 10 k) (sqr k))))

(herbie-test (v)
  "Falkner and Boettcher, Appendix B, 1"
  (acos (/ (- 1 (* 5 (sqr v))) (- (sqr v) 1))))

(herbie-test (v)
  "Falkner and Boettcher, Appendix B, 2"
  (* (/ (sqrt 2) 4) (sqrt (- 1 (* 3 (sqr v)))) (- 1 (sqr v))))
