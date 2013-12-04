; quadratic formula
(λ (a b c)
   (let ((d (sqrt (- (* b b) (* 4 (* a c))))))
     (list (/ (+ (- b) d) (* 2 a))
	   (/ (- (- b) d) (* 2 a)))))

; Hamming's rewriting of the quadratic formula
(λ (a b c)
   (let ((d (sqrt (- (* b b) (* 4 (* a c))))))
     (let* ((x1
	     (if (< b 0)
		 (/ (+ (- b) d) (* 2 a))
		 (/ (- (- b) d) (* 2 a)))
	     (x2 (/ c (* a x1)))))
       (list x1 x2))))
