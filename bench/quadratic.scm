; The quadratic formula

; Good for b < 0
(lambda (a b c)
  (/ (+ (- b) (sqrt (- (expt b 2) (* (* 4 a) c))))
     (* 2 a)))

; Good for b > 0
(lambda (a b c)
  (/ (* -2 c)
     (+ b (sqrt (- (expt b 2) (* (* 4 a) c))))))
