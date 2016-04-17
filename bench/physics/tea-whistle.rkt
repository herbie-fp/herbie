
; The aeroacoustics of a steam kettle
; R. H. Henrywood and A. Agarwal
; Phys. Fluids 25, 107101 (2013); http://dx.doi.org/10.1063/1.4821782

(herbie-test (c0 A V l)
  "Henrywood and Agarwal, Equation (3)"
  (* c0 (sqrt (/ A (* V l)))))

(herbie-test (w0 M D h l d)
  "Henrywood and Agarwal, Equation (9a)"
  (* w0 (sqrt (- 1 (* (sqr (/ (* M D) (* 2 d))) (/ h l))))))

(herbie-test (d h l M D)
  "Henrywood and Agarwal, Equation (12)"
  (* (pow (/ d h) (/ 1 2)) (pow (/ d l) (/ 1 2)) (- 1 (* (/ 1 2) (sqr (/ (* M D) (* 2 d))) (/ h l)))))

(herbie-test (c0 w h D d M)
  "Henrywood and Agarwal, Equation (13)"
  (let* ([x (/ (* c0 (sqr d)) (* w h (sqr D)))])
    (* (/ c0 (* 2 w)) (+ x (sqrt (- (sqr x) (sqr M)))))))


