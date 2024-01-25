#lang racket

;;; The default platform:
;;; Optimized for C/C++ on Linux with a full libm

(require "../plugin.rkt")

; universal boolean operations
(define boolean-platform
  (with-terminal-cost ([bool 1])
    (platform
      #:default-cost 1
      #:if-cost 1
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; non-tunable operations
(define non-tunable
  (with-terminal-cost ([binary64 64] [binary32 32])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost 64)]
      [([real binary32] [bool bool])
       (cost-map #:default-cost 32)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

; cost model that is tunable
(define cost-model
  (cost-map
    [* 1.542883]
[+ 1.212253]
[- 1.618503]
[/ 2.4495229999999997]
[acos 9.003143]
[acosh 7.581492999999999]
[asin 9.448833]
[asinh 8.684543]
[atan 9.240073]
[atan2 15.314733]
[atanh 8.673603]
[cbrt 30]
[ceil 4.564193]
[copysign 1.8491430000000002]
[cos 34.803633000000005]
[cosh 9.183303]
[erf 8.186963]
[erfc 8.312092999999999]
[exp 10.893533]
[exp2 8.634053]
[expm1 8.798613]
[fabs 0.5277990000000001]
[fdim 5.597252999999999]
[floor 4.952242999999999]
[fma 3.6629629999999995]
[fmax 2.7647729999999995]
[fmin 2.2668729999999995]
[fmod 981.483633]
[hypot 12.435133]
[lgamma 15.844033]
[log 9.248303]
[log10 11.814433]
[log1p 8.595272999999999]
[log2 8.445233]
[logb 1.980333]
[neg 0.750858]
[pow 18.469533000000002]
[remainder 172.150633]
[rint 4.269412999999999]
[round 7.093883]
[sin 34.755733]
[sinh 12.249333]
[sqrt 6.844933]
[tan 39.375033]
[tanh 8.553453]
[tgamma 20.001533000000002]
[trunc 4.656702999999999]))

; tunable operations
(define tunable
  (with-terminal-cost ([binary64 64] [binary32 32])
    (platform-product
      #:optional
      [([real binary64]) (cost-map-scale 64 cost-model)]
      [([real binary32]) (cost-map-scale 32 cost-model)]
      (operator-set
        [(real real)
         (neg acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
         exp exp2 expm1 fabs floor lgamma log log10 log2 log1p logb
         rint round sin sinh sqrt tan tanh tgamma trunc)]
        [(real real real)
         (+ - * / atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
        [(real real real real)
         (fma)]))))

; compose platforms

(define default-platform
  (platform-union boolean-platform
                  non-tunable
                  tunable))

; Register all three

; (register-platform! 'boolean boolean-platform)
; (register-platform! 'hardware hardware-platform)
(register-platform! 'default default-platform)

;; Do not run this file during testing
(module test racket/base)
