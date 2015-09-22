
;; Inspired by a SPEC Benchmark
(herbie-test ([coeffs (list 100 double .25)] [x (< 0.9 double 1.1)])
  "Calculate the value of a polynomial"
  (do-list ([sum 0.0 (+ sum (* coeff x-pow))]
            [x-pow 1 (* x-pow x)])
           ([coeff coeffs])
           sum))
