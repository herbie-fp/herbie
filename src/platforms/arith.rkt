#lang racket

;;; Arithmetic in platform

(require "../plugin.rkt")

(define move-cost 0.027403999999999994)
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
    [* 0.19580799999999995]
    [+ 0.15359200000000003]
    [- 0.16655200000000003]
    [/ 0.24216799999999997]
    [fabs 0.09402400000000002]
    [fmax 0.26974000000000004]
    [fmin 0.28530399999999995]
    [neg 0.08628399999999999]
    [sqrt 0.18764400000000003]))

; tunable operations
(define tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real) (neg fabs sqrt)]
        [(real real real) (+ - * / fmax fmin)]))))

(register-platform! 'arith
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)