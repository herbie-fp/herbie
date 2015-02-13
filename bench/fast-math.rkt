
; All from the FastMath library for haskell, from
; https://github.com/liyang/fast-math/blob/master/test/

(herbie-test (d1 d2 d3)
            "FastMath dist"
            (+ (* d1 d2) (* d1 d3))
            (* d1 (+ d2 d3)))

(herbie-test (d)
            "FastMath test1"
            (+ (* d 10) (* d 20))
            (* d 30))

(herbie-test (d1 d2)
            "FastMath test2"
            (+ (* d1 10) (* d1 d2) (* d1 20))
            (* d1 (+ 30 d2)))

(herbie-test (d1 d2 d3)
            "FastMath dist3"
            (+ (* d1 d2) (* (+ d3 5) d1) (* d1 32))
            (* d1 (+ 37 d3 d2)))

(herbie-test (d1 d2 d3 d4)
            "FastMath dist4"
            (- (+ (- (* d1 d2) (* d1 d3)) (* d4 d1)) (* d1 d1))
            (* d1 (- (+ (- d2 d3) d4) d1)))

(herbie-test (d1 d2 d3)
            "FastMath test3"
            (+ (* d1 3) (* d1 d2) (* d1 d3))
            (* d1 (+ 3 d2 d3)))

(herbie-test (d1)
            "FastMath repmul"
            (* d1 d1 d1 d1)
            (expt d1 4))

(herbie-test (d1)
            "FastMath test5"
            (* d1 (* d1 (* d1 d1) d1 d1 (* d1 d1) d1) d1)
            (expt d1 10))
