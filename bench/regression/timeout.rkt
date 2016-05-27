(lambda (a b c)
  #:name "Random Jason Timeout Test 001"
  (+ c (asin (cosh c))))

(lambda (a b c d)
  #:name "Random Jason Timeout Test 002"
  (fmod (sinh c) (- c (sqr -2.9807307601812193E165))))

(lambda (a b c)
  #:name "Random Jason Timeout Test 003"
  (sin (pow (sqrt (atan2 b b)) (- b a))))

(lambda (a b c d)
  #:name "Random Jason Timeout Test 004"
  (fmod (cosh c) (log1p a)))

(lambda (a)
  #:name "Random Jason Timeout Test 006"
  (fabs (fmod (atan2 (expm1 (sin (expm1 a))) (atan a)) a)))

(lambda (a b c)
  #:name "Random Jason Timeout Test 009"
  (fabs (fmod c (asin (- 2.821952756469356E184 b)))))

(lambda (a)
  #:name "Random Jason Timeout Test 010"
  (/ a (- (acos a))))

(lambda (a)
  #:name "Random Jason Timeout Test 011"
  (pow (atan (fmod a (asin a))) (* a a)))

(lambda (a b c)
  #:name "Random Jason Timeout Test 012"
  (acos (pow (fmod (cosh a) (* a a)) (log1p a))))

(lambda (a b c d)
  #:name "Random Jason Timeout Test 014"
  (fmod (sinh c) (- c (sqr -2.9807307601812193E165))))

(lambda (a b c)
  #:name "Random Jason Timeout Test 015"
  (sin (pow (sqrt (atan2 b b)) (- b a))))

(lambda ([a (< 0 default)] [b (< 0 default)] [c (< 0 default)])
  #:name "Area of a triangle"
  (sqrt (* (* (* (/ (+ (+ a b) c) 2) (- (/ (+ (+ a b) c) 2) a)) (- (/ (+ (+ a b) c) 2) b)) (- (/ (+ (+ a b) c) 2) c))))

(lambda ((n default) (U default) (t default) (l default) (Om default) (U* default))
  #:name "Toniolo and Linder, Equation (13)"
  (sqrt (* (* (* 2 n) U) (- (- t (* 2 (/ (sqr l) Om))) (* (* n (sqr (/ l Om))) (- U U*))))))

(lambda ((x default) (y default) (z default) (t default) (a default))
  #:name "Numeric.SpecFunctions:logGammaL from math-functions-0.1.5.2"
  (+ (- (+ (log (+ x y)) (log z)) t) (* (- a 0.5) (log t))))

(lambda ((x default) (y default) (z default) (t default) (a default) (b default) (c default) (i default))
  #:name "Numeric.SpecFunctions:logGamma from math-functions-0.1.5.2"
  (/ (+ (* (+ (* (+ (* (+ (* x y) z) y) 27464.7644705) y) 230661.510616) y) t) (+ (* (+ (* (+ (* (+ y a) y) b) y) c) y) i)))

(lambda ((x default) (y default) (z default) (t default) (a default) (b default) (c default) (i default))
  #:name "Linear.V4:$cdot from linear-1.19.1.3"
  (+ (+ (+ (* x y) (* z t)) (* a b)) (* c i)))

(lambda ((x default) (y default) (z default) (t default) (a default) (b default) (c default) (i default) (j default) (k default) (y0 default) (y1 default) (y2 default) (y3 default) (y4 default) (y5 default))
  #:name "Linear.Matrix:det44 from linear-1.19.1.3"
  (+ (- (+ (+ (- (* (- (* x y) (* z t)) (- (* a b) (* c i))) (* (- (* x j) (* z k)) (- (* y0 b) (* y1 i)))) (* (- (* x y2) (* z y3)) (- (* y0 c) (* y1 a)))) (* (- (* t j) (* y k)) (- (* y4 b) (* y5 i)))) (* (- (* t y2) (* y y3)) (- (* y4 c) (* y5 a)))) (* (- (* k y2) (* j y3)) (- (* y4 y1) (* y5 y0)))))

(lambda ((x default) (y default) (z default) (t default) (a default) (b default) (c default) (i default) (j default))
  #:name "Linear.Matrix:det33 from linear-1.19.1.3"
  (+ (- (* x (- (* y z) (* t a))) (* b (- (* c z) (* i a)))) (* j (- (* c t) (* i y)))))

(lambda ((x default) (y default) (z default) (t default) (a default))
  #:name "Hakyll.Web.Tags:renderTagCloud from hakyll-4.7.2.3"
  (+ x (* (/ (- y z) (- (+ t 1.0) z)) (- a x))))

(lambda ((x default) (y default) (z default) (t default) (a default) (b default) (c default) (i default) (j default) (k default))
  #:name "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1"
  (- (- (+ (- (* (* (* (* x 18.0) y) z) t) (* (* a 4.0) t)) (* b c)) (* (* x 4.0) i)) (* (* j 27.0) k)))

(lambda (a b)
  #:name "Bouland and Aaronson, Equation (25)"
  (- (+ (sqr (+ (sqr a) (sqr b))) (* 4 (+ (* (sqr a) (+ 1 a)) (* (sqr b) (- 1 (* 3 a)))))) 1))

(lambda (a b c)
  #:name "The quadratic formula (r1)"
  (let* ((d (- (sqr b) (* 4 a c))))
    (/ (+ (- b) (sqrt d)) (* 2 a)))
  #:target
  (let* ((d (- (sqr b) (* 4 a c)))
         (r1 (/ (+ (- b) (sqrt d)) (* 2 a)))
         (r2 (/ (- (- b) (sqrt d)) (* 2 a))))
    (if (< b 0)
        r1
        (/ c (* a r2)))))

(lambda (a b/2 c)
  #:name "NMSE problem 3.2.1"
  (let* ((d (sqrt (- (sqr b/2) (* a c)))))
    (/ (- (- b/2) d) a)))
