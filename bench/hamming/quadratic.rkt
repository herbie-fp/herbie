
; quadratic formula
(lambda (a b c)
  #:name "NMSE p42"
  (let* ((d (sqrt (- (sqr b) (* 4 (* a c))))))
    (/ (+ (- b) d) (* 2 a)))
  #:target
  (let* ((d (sqrt (- (sqr b) (* 4 (* a c)))))
         (r1 (/ (+ (- b) d) (* 2 a)))
         (r2 (/ (- (- b) d) (* 2 a))))
    (if (< b 0)
        r1
        (/ c (* a r2)))))

(lambda (a b c)
  #:name "NMSE p42"
  (let* ((d (sqrt (- (sqr b) (* 4 (* a c))))))
    (/ (- (- b) d) (* 2 a)))
  #:target
  (let* ((d (sqrt (- (sqr b) (* 4 (* a c)))))
         (r1 (/ (+ (- b) d) (* 2 a)))
         (r2 (/ (- (- b) d) (* 2 a))))
    (if (< b 0)
        (/ c (* a r1))
        r2)))

(lambda (a b/2 c)
  #:name "NMSE problem 3.2.1"
  (let* ((d (sqrt (- (sqr b/2) (* a c)))))
    (/ (- (- b/2) d) a)))

(lambda (a b/2 c)
  #:name "NMSE problem 3.2.1"
  (let* ((d (sqrt (- (sqr b/2) (* a c)))))
    (/ (+ (- b/2) d) a)))
