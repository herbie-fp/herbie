(FPCore
 (c0 A V l)
 :name
 "Henrywood and Agarwal, Equation (3)"
 (* c0 (sqrt (/ A (* V l)))))
(FPCore
 (w0 M D h l d)
 :name
 "Henrywood and Agarwal, Equation (9a)"
 (* w0 (sqrt (- 1 (* (sqr (/ (* M D) (* 2 d))) (/ h l))))))
(FPCore
 (d h l M D)
 :name
 "Henrywood and Agarwal, Equation (12)"
 (*
  (pow (/ d h) (/ 1 2))
  (pow (/ d l) (/ 1 2))
  (- 1 (* (/ 1 2) (sqr (/ (* M D) (* 2 d))) (/ h l)))))
(FPCore
 (c0 w h D d M)
 :name
 "Henrywood and Agarwal, Equation (13)"
 (let ((x (/ (* c0 (sqr d)) (* w h (sqr D)))))
   (* (/ c0 (* 2 w)) (+ x (sqrt (- (sqr x) (sqr M)))))))
