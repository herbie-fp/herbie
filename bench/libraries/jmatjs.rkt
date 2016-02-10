
;; Code from Jmat.js, commit 79a386deb5e19c0ee7896e820f3f90855a27fa17
;; https://raw.githubusercontent.com/lvandeve/jmat/79a386deb5e19c0ee7896e820f3f90855a27fa17/jmat.js

(herbie-test (z)
  "Jmat.Real.gamma, branch z > 0.5"
  (let* ([z* (- z 1)]
         [g 7]
         [x (+ 0.99999999999980993
               (/ 676.5203681218851 (+ z* 1))
               (/ -1259.1392167224028 (+ z* 2))
               (/  771.32342877765313 (+ z* 3))
               (/ -176.61502916214059 (+ z* 4))
               (/ 12.507343278686905 (+ z* 5))
               (/ -0.13857109526572012 (+ z* 6))
               (/ 9.9843695780195716e-6 (+ z* 7))
               (/ 1.5056327351493116e-7 (+ z* 8)))]
         [t (+ z* g 0.5)])
    (* (sqrt (* pi 2)) (expt t (+ z* 0.5)) (exp (- t)) x)))

(herbie-test (z)
  "Jmat.Real.gamma, branch z < 0.5"
  (let* ([z* (- (- 1 z) 1)]
         [g 7]
         [x (+ 0.99999999999980993
               (/ 676.5203681218851 (+ z* 1))
               (/ -1259.1392167224028 (+ z* 2))
               (/  771.32342877765313 (+ z* 3))
               (/ -176.61502916214059 (+ z* 4))
               (/ 12.507343278686905 (+ z* 5))
               (/ -0.13857109526572012 (+ z* 6))
               (/ 9.9843695780195716e-6 (+ z* 7))
               (/ 1.5056327351493116e-7 (+ z* 8)))]
         [t (+ z* g 0.5)])
    (* (/ pi (sin (* pi z)))
       (* (sqrt (* pi 2)) (expt t (+ z* 0.5)) (exp (- t)) x))))

(herbie-test (x)
  "Jmat.Real.lambertw, estimator"
  (- (log x) (log (log x))))

(herbie-test (wj x)
  "Jmat.Real.lambertw, newton loop step"
  (let* ([ew (exp wj)])
    (- wj (/ (- (* wj ew) x) (+ ew (* wj ew)))))
  (let* ([ew (exp wj)])
    (- wj (- (/ wj (+ wj 1)) (/ x (+ ew (* wj ew)))))))

(herbie-test (x)
  "Jmat.Real.dawson"
  (let* ([x2 (* x x)]
         [x4 (* x2 x2)]
         [x6 (* x4 x2)]
         [x8 (* x6 x2)]
         [x10 (* x8 x2)]
         [x12 (* x10 x2)]
         [p1 0.1049934947] [p2 0.0424060604] [p3 0.0072644182] [p4 0.0005064034] [p5 0.0001789971]
         [q1 0.7715471019] [q2 0.2909738639] [q3 0.0694555761] [q4 0.0140005442] [q5 0.0008327945])

         (* (/ (+ 1 (* p1 x2) (* p2 x4) (* p3 x6) (* p4 x8) (* p5 x10))
               (+ 1 (* q1 x2) (* q2 x4) (* q3 x6) (* q4 x8) (* q5 x10) (* 2 p5 x12)))
            x)))

(herbie-test (x)
  "Jmat.Real.erfi, branch x <= 0.5"
  (let* ([sqrtpi (sqrt pi)]
         [ps (/ 1 sqrtpi)]
         [x* (abs x)]
         [x3 (* x* x* x*)]
         [x5 (* x3 x* x*)]
         [x7 (* x5 x* x*)]
         [t (+ (* 2 x*) (* (/ 2 3) x3) (* (/ 1 5) x5) (* (/ 1 21) x7))])
    (abs (* ps t))))

(herbie-test (x)
  "Jmat.Real.erfi, branch x >= 5"
  (let* ([sqrtpi (sqrt pi)]
         [ps (/ 1 sqrtpi)]
         [x* (abs x)]
         [xi (/ 1 x*)]
         [xi3 (* xi xi xi)]
         [xi5 (* xi3 xi xi)]
         [xi7 (* xi5 xi xi)]
         [e (exp (* x* x*))]
         [t (+ xi (* (/ 1 2) xi3) (* (/ 3 4) xi5) (* (/ 15 8) xi7))])
    (* ps e t)))

; TODO : Jmat.Real.erfi, branch 0.5 < x < 5

(herbie-test (x)
  "Jmat.Real.erf"
  (let* ([x* (abs x)]
         [t (/ 1 (+ 1 (* 0.3275911 x*)))]
         [p (* t (+ 0.254829592 (* t (+ -0.284496736 (* t (+ 1.421413741 (* t (+ -1.453152027 (* t 1.061405429)))))))))])
    (- 1 (* p (exp (- (* x* x*)))))))

; Got no further than erf
