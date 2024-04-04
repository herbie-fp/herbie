#lang racket

(require "../plugin.rkt")

(define move-cost 1)
(define single-move-cost (* move-cost 1))
(define double-move-cost (* move-cost 1))

(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost move-cost
      #:if-cost move-cost
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

(define non-tunable
  (with-terminal-cost ([binary64 move-cost])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost move-cost)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

(define cost-model
  (cost-map
   [* 0.3279999999999999]
   [+ 0.27288]
   [- 0.27643999999999996]
   ;; Hmmm
   [/ 0.32364000000000004]
   [ceil 0.13964000000000001]
   [fabs 0.13316]
   [floor 0.1336]
   [fma 0.4119599999999999]
   [fmax 0.24779999999999994]
   [fmin 0.27904]
   [fmsub 0.4052]
   [fnmadd 0.40891999999999984]
   [fnmsub 0.37879999999999997]
   [neg 0.16715999999999995]
   [round 0.14036]
   [sqrt 0.18604000000000007]))

(define tunable
  (with-terminal-cost ([binary64 move-cost])
    (platform-product
     #:optional
     [([real binary64]) cost-model]
     (operator-set
      [(real real)
       (neg floor sqrt round ceil fabs)]
      [(real real real)
       (+ - * / fmax fmin)]
      [(real real real real)
       (fma fmsub fnmadd fnmsub)]))))

(register-platform! 'avx
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

(module test racket/base)
