#lang racket

;;; Arithmetic in platform

(require "../plugin.rkt")

(define move-cost 0.027403999999999994)
(define fl-move-cost (* move-cost 5))

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
    [* 0.21627599999999997]
    [+ 0.158984]
    [- 0.151168]
    [/ 0.278124]
    [fabs 0.107824]
    [fmax 0.2687320000000001]
    [fmin 0.3078199999999999]
    [neg 0.09292]
    [sqrt 0.21313200000000002]))

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
