(FPCore
 (a b c)
 :name
 "The quadratic formula (r1)"
 :target
 (let ((d (- (sqr b) (* 4 a c))))
   (let ((r1 (/ (+ (- b) (sqrt d)) (* 2 a))))
     (let ((r2 (/ (- (- b) (sqrt d)) (* 2 a))))
       (if (< b 0) r1 (/ c (* a r2))))))
 (let ((d (- (sqr b) (* 4 a c)))) (/ (+ (- b) (sqrt d)) (* 2 a))))
(FPCore
 (a b c)
 :name
 "The quadratic formula (r2)"
 :target
 (let ((d (sqrt (- (sqr b) (* 4 (* a c))))))
   (let ((r1 (/ (+ (- b) d) (* 2 a))))
     (let ((r2 (/ (- (- b) d) (* 2 a)))) (if (< b 0) (/ c (* a r1)) r2))))
 (let ((d (sqrt (- (sqr b) (* 4 (* a c)))))) (/ (- (- b) d) (* 2 a))))
(FPCore
 (a b)
 :name
 "Difference of squares"
 :target
 (* (+ a b) (- a b))
 (- (sqr a) (sqr b)))
(FPCore
 (a b c)
 :name
 "Area of a triangle"
 :target
 (/ (sqrt (* (+ a (+ b c)) (- c (- a b)) (+ c (- a b)) (+ a (- b c)))) 4)
 (let ((s (/ (+ a b c) 2))) (sqrt (* s (- s a) (- s b) (- s c)))))
(FPCore
 (x)
 :name
 "ln(1 + x)"
 :target
 (if (= (+ 1 x) 1) x (/ (* x (log (+ 1 x))) (- (+ 1 x) 1)))
 (log (+ 1 x)))
(FPCore
 (i n)
 :name
 "Compound Interest"
 :target
 (let ((lnbase
        (if (= (+ 1 (/ i n)) 1)
          (/ i n)
          (/ (* (/ i n) (log (+ 1 (/ i n)))) (- (+ (/ i n) 1) 1)))))
   (* 100 (/ (- (exp (* n lnbase)) 1) (/ i n))))
 (* 100 (/ (- (pow (+ 1 (/ i n)) n) 1) (/ i n))))
(FPCore
 (x)
 :name
 "x / (x^2 + 1)"
 :target
 (/ 1 (+ x (/ 1 x)))
 (/ x (+ (sqr x) 1)))
(FPCore
 (a b c d)
 :name
 "Complex division, real part"
 :target
 (if (< (fabs d) (fabs c))
   (/ (+ a (* b (/ d c))) (+ c (* d (/ d c))))
   (/ (+ b (* a (/ c d))) (+ d (* c (/ c d)))))
 (/ (+ (* a c) (* b d)) (+ (sqr c) (sqr d))))
(FPCore
 (a b c d)
 :name
 "Complex division, imag part"
 :target
 (if (< (fabs d) (fabs c))
   (/ (- b (* a (/ d c))) (+ c (* d (/ d c))))
   (/ (+ (- a) (* b (/ c d))) (+ d (* c (/ c d)))))
 (/ (- (* b c) (* a d)) (+ (sqr c) (sqr d))))
(FPCore (x) :name "arccos" (* 2 (atan (sqrt (/ (- 1 x) (+ 1 x))))))
