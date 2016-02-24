(lambda (a b c)
  #:name "Random Jason Timeout Test 001"
  (+ c (asin (cosh c))))

(lambda (a b c d)
  #:name "Random Jason Timeout Test 002"
  (mod (sinh c) (- c (sqr -2.9807307601812193E165))))

(lambda (a b c)
  #:name "Random Jason Timeout Test 003"
  (sin (expt (sqrt (atan2 b b)) (- b a))))

(lambda (a b c d)
  #:name "Random Jason Timeout Test 004"
  (mod (cosh c) (log1p a)))

(lambda (a)
  #:name "Random Jason Timeout Test 005"
  (cotan (expm1 (log1p (+ (asin 5.5642831668124635E247) (sqrt a))))))

(lambda (a)
  #:name "Random Jason Timeout Test 006"
  (abs (mod (atan2 (expm1 (sin (expm1 a))) (atan a)) a)))

(lambda ((a default) (b default) (c default))
  #:name "Random Jason Timeout Test 007"
  (atan (log1p (+ (log -1.8207626529605104e-301) c))))

(lambda ((a default) (b default) (c default))
  #:name "Random Jason Timeout Test 008"
  (* (* (atan (atan 3.882919441585229e-190)) 4.568031610793939e-123) (mod (sqrt -1.0580015533610528e+239) b)))

(lambda ((a default) (b default) (c default))
  #:name "Area of a triangle"
  (sqrt (* (* (* (/ (+ (+ a b) c) 2) (- (/ (+ (+ a b) c) 2) a)) (- (/ (+ (+ a b) c) 2) b)) (- (/ (+ (+ a b) c) 2) c))))

(lambda ((n default) (U default) (t default) (l default) (Om default) (U* default))
  #:name "Toniolo and Linder, Equation (13)"
  (sqrt (* (* (* 2 n) U) (- (- t (* 2 (/ (sqr l) Om))) (* (* n (sqr (/ l Om))) (- U U*))))))
