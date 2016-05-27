
; What Every Computer Scientist Should Know About Floating-Point Arithmetic
; David Goldberg
; Computing Surveys, March 1991
; http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html

(herbie-test (a b c)
  "The quadratic formula (r1)"
   (let* ((d (- (sqr b) (* 4 a c))))
     (/ (+ (- b) (sqrt d)) (* 2 a)))
   (let* ((d (- (sqr b) (* 4 a c)))
          (r1 (/ (+ (- b) (sqrt d)) (* 2 a)))
          (r2 (/ (- (- b) (sqrt d)) (* 2 a))))
     (if (< b 0)
         r1
         (/ c (* a r2)))))

(herbie-test (a b c)
  "The quadratic formula (r2)"
   (let* ((d (sqrt (- (sqr b) (* 4 (* a c))))))
     (/ (- (- b) d) (* 2 a)))
   (let* ((d (sqrt (- (sqr b) (* 4 (* a c)))))
          (r1 (/ (+ (- b) d) (* 2 a)))
          (r2 (/ (- (- b) d) (* 2 a))))
     (if (< b 0)
         (/ c (* a r1))
         r2)))

(herbie-test (a b)
  "Difference of squares"
  (- (sqr a) (sqr b))
  (* (+ a b) (- a b)))

(herbie-test ([a (< 0 default)] [b (< 0 default)] [c (< 0 default)]) ; TODO: restrict to a > b > c
  "Area of a triangle"
  (let* ([s (/ (+ a b c) 2)])
    (sqrt (* s (- s a) (- s b) (- s c))))
  (/ (sqrt (* (+ a (+ b c))
              (- c (- a b))
              (+ c (- a b))
              (+ a (- b c))))
     4))

(herbie-test (x)
  "ln(1 + x)"
  (log (+ 1 x))
  (if (= (+ 1 x) 1)
      x
      (/ (* x (log (+ 1 x)))
         (- (+ 1 x) 1))))

(herbie-test (i n)
  "Compound Interest"
  (* 100 (/ (- (pow (+ 1 (/ i n)) n) 1) (/ i n)))
  (let* ([lnbase
         (if (= (+ 1 (/ i n)) 1)
             (/ i n)
             (/ (* (/ i n) (log (+ 1 (/ i n))))
                (- (+ (/ i n) 1) 1)))])
    (* 100 (/ (- (exp (* n lnbase)) 1)
              (/ i n)))))

(herbie-test (x)
  "x / (x^2 + 1)"
  (/ x (+ (sqr x) 1))
  (/ 1 (+ x (/ 1 x))))

(herbie-test (a b c d)
  "Complex division, real part"
  (/ (+ (* a c) (* b d)) (+ (sqr c) (sqr d)))
  (if (< (fabs d) (fabs c))
      (/ (+ a (* b (/ d c))) (+ c (* d (/ d c))))
      (/ (+ b (* a (/ c d))) (+ d (* c (/ c d))))))

(herbie-test (a b c d)
  "Complex division, imag part"
  (/ (- (* b c) (* a d)) (+ (sqr c) (sqr d)))
  (if (< (fabs d) (fabs c))
      (/ (- b (* a (/ d c))) (+ c (* d (/ d c))))
      (/ (+ (- a) (* b (/ c d))) (+ d (* c (/ c d))))))

(herbie-test (x)
  "arccos"
  (* 2 (atan (sqrt (/ (- 1 x) (+ 1 x))))))
