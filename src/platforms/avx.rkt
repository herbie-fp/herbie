#lang racket

(require "../plugin.rkt")

(define move-cost 1)
(define single-move-cost (* move-cost 1))
(define double-move-cost (* move-cost 1))

(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
     #:default-cost move-cost
     #:if-cost (sum move-cost)
     [(bool) (TRUE FALSE)]
     [(bool bool) not]
     [(bool bool bool) (and or)])))

(define non-tunable
  (with-terminal-cost ([binary64 move-cost] [binary32 move-cost])
    (platform-product
     [([real binary64] [bool bool])
      (cost-map #:default-cost move-cost)]
     (operator-set
      [(real) (PI E INFINITY NAN)]
      [(real real bool) (== != > < >= <=)]))))

(define cost-model-single
  (cost-map
   [/ 11]))

(define cost-model-double
  (cost-map
   [/ 14]))

(define cost-model
  (cost-map
   [(* + -) 4]
   [(fmsub fnmadd fnmsub fma) 4]
   [(fmax fmin) 4]
   [fabs 8] ;; cost for _mm256_set1_ps plus _mm256_andnot_ps
   [neg 11] ;; cost for _mm_sub_ps(_mm_set1_ps(0.0), v);
   [sqrt 12]
   [(round ceil floor) 8]))

(define tunable
  (with-terminal-cost ([binary64 move-cost] [binary32 move-cost])
    (platform-product
     #:optional
     [([real binary64]) cost-model]
     (operator-set
      [(real real)
       (neg floor sqrt round ceil fabs
            )]
      [(real real real)
       (+ - * fmax fmin)]
      [(real real real real)
       (fma fmsub fnmadd fnmsub)]))))

(define tunable-single-precision
  (with-terminal-cost ([binary32 move-cost])
    (platform-product
     #:optional
     [([real binary32]) cost-model-single]
     (operator-set
      [(real real real)
       (/)]))))

(define tunable-double-precision
  (with-terminal-cost ([binary64 move-cost])
    (platform-product
     #:optional
     [([real binary64]) cost-model-double]
     (operator-set
      [(real real real)
       (/)]))))

(register-platform! 'avx
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable
                                    tunable-single-precision
                                    tunable-double-precision))

(module test racket/base)
