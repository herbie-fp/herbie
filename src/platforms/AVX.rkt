#lang racket

;;; AVX platform:
;;; Costs based on AVX Skylake instructions

(require "../plugin.rkt")

(define move-cost 1)
(define single-move-cost (* move-cost 1))
(define double-move-cost (* move-cost 1))

(define single-cost-model
  (cost-map
    [/ 11]))

(define double-cost-model
  (cost-map
    [/ 14]))

(define both-cost-model
  (cost-map
    [(fmsub fnmadd fnmsub fma) 4]
    [(* + -) 4]
    [(fmax fmin) 4]
    [fabs 8] ;cost for _mm256_set1_ps plus _mm256_andnot_ps
    [floor 8]
    [neg 11] ;cost for _mm_sub_ps(_mm_set1_ps(0.0), v);
    [sqrt 12]
    [(ceil round) 8]  
    [(== != > < >= <=) 4]
    [(PI E INFINITY NAN) 7])) ;cost for _mm256_broadcast_pd 

; boolean operations
(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost move-cost
      #:if-cost move-cost
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; single-precision
(define single-precision
  (with-terminal-cost ([binary32 single-move-cost])
    (platform-product
    #:optional
      [([real binary32])  single-cost-model]
      (operator-set
        [(real real real)
         (/)]))))

; double-precision
(define double-precision
  (with-terminal-cost ([binary64 double-move-cost])
    (platform-product
       #:optional
      [([real binary64]) double-cost-model]
      (operator-set
        [(real real real)
         (/)]))))

; same costs for both 
(define same-costs
  (with-terminal-cost ([binary64 double-move-cost] [bool move-cost] [binary32 single-move-cost])
    (platform-product
      #:optional
      [([real binary64] [bool bool] [real binary32]) both-cost-model]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real)
         (neg floor sqrt round ceil fabs)]
         [(real real bool) (== != > < >= <=)]
        [(real real real)
         (+ - * fmax fmin)]
        [(real real real real)
         (fma fmsub fnmadd fnmsub)]))))



(register-platform! 'avx
                    (platform-union single-precision
                                    double-precision
                                    boolean-platform
                                    same-costs))

;; Do not run this file during testing
(module test racket/base)