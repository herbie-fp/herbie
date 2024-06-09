#lang racket

;;; Arithmetic in platform + FMA

(require "../plugin.rkt")

(define move-cost 0.02333600000000001)
(define fl-move-cost (* move-cost 4))

; universal boolean operations
(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost move-cost
      #:if-cost move-cost
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; non-tunable operations
(define non-tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost fl-move-cost)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

(define cost-model
  (cost-map
    [* 0.20874800000000002]
    [+ 0.164604]
    [- 0.15163999999999997]
    [/ 0.26615199999999994]
    [fabs 0.10161999999999997]
    [fma 0.3366239999999999]
    [fmax 0.3106]
    [fmin 0.289256]
    [neg 0.09659199999999998]
    [sqrt 0.19187200000000001]))

; tunable operations
(define tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real) (neg fabs sqrt)]
        [(real real real) (+ - * / fmax fmin)]
        [(real real real real) (fma)]))))

(register-platform! 'arith-fma
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
