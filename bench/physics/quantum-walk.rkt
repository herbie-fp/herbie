(FPCore
 (v t)
 :name
 "Falkner and Boettcher, Equation (20:1,3)"
 (/
  (- 1 (* 5 (sqr v)))
  (* PI t (sqrt (* 2 (- 1 (* 3 (sqr v))))) (- 1 (sqr v)))))
(FPCore
 (v)
 :name
 "Falkner and Boettcher, Equation (22+)"
 (/ 4 (* 3 PI (- 1 (sqr v)) (sqrt (- 2 (* 6 (sqr v)))))))
(FPCore
 (a k m)
 :name
 "Falkner and Boettcher, Appendix A"
 (/ (* a (pow k m)) (+ 1 (* 10 k) (sqr k))))
(FPCore
 (v)
 :name
 "Falkner and Boettcher, Appendix B, 1"
 (acos (/ (- 1 (* 5 (sqr v))) (- (sqr v) 1))))
(FPCore
 (v)
 :name
 "Falkner and Boettcher, Appendix B, 2"
 (* (/ (sqrt 2) 4) (sqrt (- 1 (* 3 (sqr v)))) (- 1 (sqr v))))
