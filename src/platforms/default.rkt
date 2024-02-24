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
[* 7.9295332136445245]
[+ 5.929084380610413]
[- 7.529622980251346]
[/ 11.13689407540395]
[acos 39.314631956912024]
[acosh 31.473518850987432]
[asin 39.41512567324955]
[asinh 35.4008078994614]
[atan 35.019299820466784]
[atan2 64.51929982046678]
[atanh 36.959605026929985]
[cbrt 66.23294434470377]
[ceil 19.607719928186714]
[copysign 7.829443447037702]
[cos 139.6858168761221]
[cosh 38.666965888689404]
[erf 35.00089766606822]
[erfc 32.758527827648116]
[exp 45.1548473967684]
[exp2 33.66561938958707]
[expm1 36.68222621184919]
[fabs 3.0691157989228004]
[fdim 23.043985637342907]
[floor 19.016157989228006]
[fma 15.605924596050269]
[fmax 12.401705565529621]
[fmin 10.258078994614003]
[fmod 4049.5152603231595]
[hypot 51.25089766606823]
[lgamma 66.13779174147217]
[log 39.617100538599644]
[log10 51.054757630161575]
[log1p 37.690305206463194]
[log2 35.75314183123878]
[logb 8.389587073608617]
[neg 3.1597800718132856]
[pow 76.66472172351885]
[recip 5.122531418312387]
[remainder 688.6445242369838]
[rint 19.32226211849192]
[round 25.35771992818671]
[rsqrt 4.911579892280072]
[sin 146.14856373429083]
[sinh 51.0318671454219]
[sqrt 28.348294434470375]
[tan 169.52917414721722]
[tanh 33.688061041292634]
[tgamma 77.87567324955117]
[trunc 19.265260323159783]))

; tunable operations
(define tunable
  (with-terminal-cost ([binary64 64] [binary32 32])
    (platform-product
      #:optional
      [([real binary64]) (cost-map-scale 64 cost-model)]
      [([real binary32]) (cost-map-scale 32 cost-model)]
      (operator-set
        [(real real)
         (neg acos acosh asin asinh atan atanh cbrt ceil cos cosh
          erf erfc exp exp2 expm1 fabs floor lgamma log log10 log2
          log1p logb rint round sin sinh sqrt tan tanh tgamma trunc
          recip rsqrt)]
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
