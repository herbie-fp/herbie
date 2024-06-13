#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require "../plugin.rkt")

(define fl-move-cost 0.008059999999999994)
(define move-cost fl-move-cost)

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

; cost model that is tunable
(define cost-model
  (cost-map
    [* 0.01700999999999999]
    [+ 0.012109999999999987]
    [- 0.013479999999999997]
    [/ 0.02308]
    [acos 0.042480000000000025]
    [acosh 0.05771]
    [asin 0.03957]
    [asinh 0.09461]
    [atan 0.12122000000000002]
    [atan2 0.17298]
    [atanh 0.03768000000000002]
    [cbrt 0.14997]
    [ceil 0.01227999999999999]
    [copysign 0.012379999999999992]
    [cos 0.34913999999999995]
    [cosh 0.11281999999999999]
    [erf 0.10867]
    [erfc 0.07066999999999998]
    [exp 0.12545]
    [exp2 0.25792000000000004]
    [expm1 0.08633999999999999]
    [fabs 0.00847]
    [fdim 0.0694]
    [floor 0.015489999999999976]
    [fma 0.02393999999999999]
    [fmax 0.013140000000000008]
    [fmin 0.015159999999999996]
    [fmod 0.16115000000000002]
    [hypot 0.14554]
    [lgamma 0.30947]
    [log 0.03894999999999999]
    [log10 0.09596]
    [log1p 0.09539]
    [log2 0.0627]
    [logb 0.017309999999999992]
    [neg 0.008109999999999989]
    [pow 0.16465999999999997]
    [remainder 0.06891]
    [rint 0.0129]
    [round 0.07560000000000003]
    [sin 0.3221]
    [sinh 0.14037]
    [sqrt 0.019690000000000006]
    [tan 0.33812]
    [tanh 0.09347999999999998]
    [tgamma 0.21502]
    [trunc 0.0116]))

; tunable operations
(define tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (neg acos acosh asin asinh atan atanh cbrt ceil cos cosh
          erf erfc exp exp2 expm1 fabs floor lgamma log log10 log2
          log1p logb rint round sin sinh sqrt tan tanh tgamma trunc)]
        [(real real real)
         (+ - * / atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
        [(real real real real)
         (fma)]))))

(register-platform! 'c
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
