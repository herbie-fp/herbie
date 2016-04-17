
;; All from the FastMath library for haskell, from
;; https://github.com/liyang/fast-math/blob/master/test/
;; These test Herbie's ability to do basic arithmetic

(lambda (d1 d2 d3)
  #:name "FastMath dist"
  (+ (* d1 d2) (* d1 d3))
  #:target (* d1 (+ d2 d3)))

(lambda (d)
  #:name "FastMath test1"
  (+ (* d 10) (* d 20))
  #:target (* d 30))

(lambda (d1 d2)
  #:name "FastMath test2"
  (+ (* d1 10) (* d1 d2) (* d1 20))
  #:target (* d1 (+ 30 d2)))

(lambda (d1 d2 d3)
  #:name "FastMath dist3"
  (+ (* d1 d2) (* (+ d3 5) d1) (* d1 32))
  #:target (* d1 (+ 37 d3 d2)))

(lambda (d1 d2 d3 d4)
  #:name "FastMath dist4"
  (- (+ (- (* d1 d2) (* d1 d3)) (* d4 d1)) (* d1 d1))
  #:target (* d1 (- (+ (- d2 d3) d4) d1)))

(lambda (d1 d2 d3)
  #:name "FastMath test3"
  (+ (* d1 3) (* d1 d2) (* d1 d3))
  #:target (* d1 (+ 3 d2 d3)))

(lambda (d1)
  #:name "FastMath repmul"
  (* d1 d1 d1 d1)
  #:target (pow d1 4))

(lambda (d1)
  #:name "FastMath test5"
  (* d1 (* d1 (* d1 d1) d1 d1 (* d1 d1) d1) d1)
  #:target (pow d1 10))
