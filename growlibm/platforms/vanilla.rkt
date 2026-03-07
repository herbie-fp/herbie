#lang s-exp "../../src/syntax/platform-language.rkt"

;; Herbie 2.0 platform. Based on the C Windows platform, but with
;; every operation having heuristic costs from Herbie 2.0.

(require math/flonum)

(define 64bit-move-cost 0.125)
(define 32bit-move-cost 0.125)
(define boolean-move-cost 0.100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost boolean-move-cost)

(define-operations ()
  <bool>
  [TRUE #:spec (TRUE) #:impl (const true) #:fpcore TRUE #:cost boolean-move-cost]
  [FALSE #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost boolean-move-cost])

(define-operations ([x <bool>] [y <bool>])
  <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost boolean-move-cost]
  [or #:spec (or x y) #:impl (lambda v (ormap values v)) #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool> #:spec (not x) #:impl not #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary32> #:cost 32bit-move-cost)

(define-operation (if.f32 [c <bool>] [t <binary32>] [f <binary32>])
                  <binary32>
                  #:spec (if c t f)
                  #:impl if-impl
                  #:cost (if-cost boolean-move-cost))

(define-operations ([x <binary32>] [y <binary32>])
  <bool>
  [==.f32 #:spec (== x y) #:impl = #:cost 32bit-move-cost]
  [!=.f32 #:spec (!= x y) #:impl (negate =) #:cost 32bit-move-cost]
  [<.f32 #:spec (< x y) #:impl < #:cost 32bit-move-cost]
  [>.f32 #:spec (> x y) #:impl > #:cost 32bit-move-cost]
  [<=.f32 #:spec (<= x y) #:impl <= #:cost 32bit-move-cost]
  [>=.f32 #:spec (>= x y) #:impl >= #:cost 32bit-move-cost])

(define-operations ()
  <binary32>
  #:fpcore (! :precision binary32 _)
  [PI.f32 #:spec (PI) #:impl (const (flsingle pi)) #:fpcore PI #:cost 32bit-move-cost]
  [E.f32 #:spec (E) #:impl (const (flsingle (exp 1))) #:fpcore E #:cost 32bit-move-cost]
  [INFINITY.f32 #:spec (INFINITY) #:impl (const +inf.0) #:fpcore INFINITY #:cost 32bit-move-cost]
  [NAN.f32 #:spec (NAN) #:impl (const +nan.0) #:fpcore NAN #:cost 32bit-move-cost])

(define-operation (neg.f32 [x <binary32>])
                  <binary32>
                  #:spec (neg x)
                  #:impl (compose flsingle -)
                  #:fpcore (! :precision binary32 (- x))
                  #:cost 0.18983050847457694)

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.2]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 0.2024653312788908]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 0.22157164869029342]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.1972265023112485])

(define-operations ([x <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [fabs.f32 #:spec (fabs x) #:impl (from-libm 'fabsf) #:cost 0.19568567026194217]
  [sin.f32 #:spec (sin x) #:impl (from-libm 'sinf) #:cost 2.284437596302006]
  [cos.f32 #:spec (cos x) #:impl (from-libm 'cosf) #:cost 2.2640986132511567]
  [tan.f32 #:spec (tan x) #:impl (from-libm 'tanf) #:cost 2.403697996918337]
  [sinh.f32 #:spec (sinh x) #:impl (from-libm 'sinhf) #:cost 2.2995377503852086]
  [cosh.f32 #:spec (cosh x) #:impl (from-libm 'coshf) #:cost 1.8616332819722698]
  [acos.f32 #:spec (acos x) #:impl (from-libm 'acosf) #:cost 1.503235747303546]
  [acosh.f32 #:spec (acosh x) #:impl (from-libm 'acoshf) #:cost 2.0366718027735007]
  [asin.f32 #:spec (asin x) #:impl (from-libm 'asinf) #:cost 3.3636363636363678]
  [asinh.f32 #:spec (asinh x) #:impl (from-libm 'asinhf) #:cost 2.178428351309709]
  [atan.f32 #:spec (atan x) #:impl (from-libm 'atanf) #:cost 2.2983050847457647]
  [atanh.f32 #:spec (atanh x) #:impl (from-libm 'atanhf) #:cost 2.13959938366718]
  [cbrt.f32 #:spec (cbrt x) #:impl (from-libm 'cbrtf) #:cost 2.13929121725732]
  [ceil.f32 #:spec (ceil x) #:impl (from-libm 'ceilf) #:cost 0.19106317411402185]
  [erf.f32 #:spec (erf x) #:impl (from-libm 'erff) #:cost 2.0942989214175673]
  [exp.f32 #:spec (exp x) #:impl (from-libm 'expf) #:cost 1.572881355932205]
  [exp2.f32 #:spec (exp2 x) #:impl (from-libm 'exp2f) #:cost 1.5950693374422205]
  [floor.f32 #:spec (floor x) #:impl (from-libm 'floorf) #:cost 0.18952234206471533]
  [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf) #:cost 4.1707241910631785]
  [log.f32 #:spec (log x) #:impl (from-libm 'logf) #:cost 1.4197226502311258]
  [log10.f32 #:spec (log10 x) #:impl (from-libm 'log10f) #:cost 1.5716486902927598]
  [log2.f32 #:spec (log2 x) #:impl (from-libm 'log2f) #:cost 1.428967642526966]
  [logb.f32 #:spec (logb x) #:impl (from-libm 'logbf) #:cost 0.7278890600924502]
  [rint.f32 #:spec (rint x) #:impl (from-libm 'rintf) #:cost 0.1870570107858246]
  [round.f32 #:spec (round x) #:impl (from-libm 'roundf) #:cost 0.18736517719568596]
  [sqrt.f32 #:spec (sqrt x) #:impl (from-libm 'sqrtf) #:cost 0.35932203389830625]
  [tanh.f32 #:spec (tanh x) #:impl (from-libm 'tanhf) #:cost 1.6425269645608633]
  [tgamma.f32 #:spec (tgamma x) #:impl (from-libm 'tgammaf) #:cost 2.202773497688754]
  [trunc.f32 #:spec (trunc x) #:impl (from-libm 'truncf) #:cost 0.18983050847457622])

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [pow.f32 #:spec (pow x y) #:impl (from-libm 'powf) #:cost 2.984591679506936]
  [atan2.f32 #:spec (atan2 x y) #:impl (from-libm 'atan2f) #:cost 4.067488443759635]
  [copysign.f32 #:spec (copysign x y) #:impl (from-libm 'copysignf) #:cost 0.19969183359013912]
  [fdim.f32 #:spec (fdim x y) #:impl (from-libm 'fdimf) #:cost 1.3648690292758106]
  [fmax.f32 #:spec (fmax x y) #:impl (from-libm 'fmaxf) #:cost 0.19876733436055496]
  [fmin.f32 #:spec (fmin x y) #:impl (from-libm 'fminf) #:cost 0.20030816640986143]
  [fmod.f32 #:spec (fmod x y) #:impl (from-libm 'fmodf) #:cost 2.853929121725733]
  [remainder.f32 #:spec (remainder x y) #:impl (from-libm 'remainderf) #:cost 3.83451463790447])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 64bit-move-cost)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>])
                  <binary64>
                  #:spec (if c t f)
                  #:impl if-impl
                  #:cost (if-cost boolean-move-cost))

(define-operations ([x <binary64>] [y <binary64>])
  <bool>
  [==.f64 #:spec (== x y) #:impl = #:cost 64bit-move-cost]
  [!=.f64 #:spec (!= x y) #:impl (negate =) #:cost 64bit-move-cost]
  [<.f64 #:spec (< x y) #:impl < #:cost 64bit-move-cost]
  [>.f64 #:spec (> x y) #:impl > #:cost 64bit-move-cost]
  [<=.f64 #:spec (<= x y) #:impl <= #:cost 64bit-move-cost]
  [>=.f64 #:spec (>= x y) #:impl >= #:cost 64bit-move-cost])

(define-operations ()
  <binary64>
  #:fpcore (! :precision binary64 _)
  [PI.f64 #:spec (PI) #:impl (const pi) #:fpcore PI #:cost 64bit-move-cost]
  [E.f64 #:spec (E) #:impl (const (exp 1)) #:fpcore E #:cost 64bit-move-cost]
  [INFINITY #:spec (INFINITY) #:impl (const +inf.0) #:fpcore INFINITY #:cost 64bit-move-cost]
  [NAN.f64 #:spec (NAN) #:impl (const +nan.0) #:fpcore NAN #:cost 64bit-move-cost])

(define-operation (neg.f64 [x <binary64>])
                  <binary64>
                  #:spec (neg x)
                  #:impl -
                  #:fpcore (! :precision binary64 (- x))
                  #:cost 0.18983050847457694)

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [+.f64 #:spec (+ x y) #:impl + #:cost 0.2]
  [-.f64 #:spec (- x y) #:impl - #:cost 0.2024653312788908]
  [*.f64 #:spec (* x y) #:impl * #:cost 0.22157164869029342]
  [/.f64 #:spec (/ x y) #:impl / #:cost 0.1972265023112485])

(define-operations ([x <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [fabs.f64 #:spec (fabs x) #:impl (from-libm 'fabs) #:cost 0.19568567026194217]
  [sin.f64 #:spec (sin x) #:impl (from-libm 'sin) #:cost 2.284437596302006]
  [cos.f64 #:spec (cos x) #:impl (from-libm 'cos) #:cost 2.2640986132511567]
  [tan.f64 #:spec (tan x) #:impl (from-libm 'tan) #:cost 2.403697996918337]
  [sinh.f64 #:spec (sinh x) #:impl (from-libm 'sinh) #:cost 2.2995377503852086]
  [cosh.f64 #:spec (cosh x) #:impl (from-libm 'cosh) #:cost 1.8616332819722698]
  [acos.f64 #:spec (acos x) #:impl (from-libm 'acos) #:cost 1.503235747303546]
  [acosh.f64 #:spec (acosh x) #:impl (from-libm 'acosh) #:cost 2.0366718027735007]
  [asin.f64 #:spec (asin x) #:impl (from-libm 'asin) #:cost 3.3636363636363678]
  [asinh.f64 #:spec (asinh x) #:impl (from-libm 'asinh) #:cost 2.178428351309709]
  [atan.f64 #:spec (atan x) #:impl (from-libm 'atan) #:cost 2.2983050847457647]
  [atanh.f64 #:spec (atanh x) #:impl (from-libm 'atanh) #:cost 2.13959938366718]
  [cbrt.f64 #:spec (cbrt x) #:impl (from-libm 'cbrt) #:cost 2.13929121725732]
  [ceil.f64 #:spec (ceil x) #:impl (from-libm 'ceil) #:cost 0.19106317411402185]
  [erf.f64 #:spec (erf x) #:impl (from-libm 'erf) #:cost 2.0942989214175673]
  [exp.f64 #:spec (exp x) #:impl (from-libm 'exp) #:cost 1.572881355932205]
  [exp2.f64 #:spec (exp2 x) #:impl (from-libm 'exp2) #:cost 1.5950693374422205]
  [floor.f64 #:spec (floor x) #:impl (from-libm 'floor) #:cost 0.18952234206471533]
  [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma) #:cost 4.1707241910631785]
  [log.f64 #:spec (log x) #:impl (from-libm 'log) #:cost 1.4197226502311258]
  [log10.f64 #:spec (log10 x) #:impl (from-libm 'log10) #:cost 1.5716486902927598]
  [log2.f64 #:spec (log2 x) #:impl (from-libm 'log2) #:cost 1.428967642526966]
  [logb.f64 #:spec (logb x) #:impl (from-libm 'logb) #:cost 0.7278890600924502]
  [rint.f64 #:spec (rint x) #:impl (from-libm 'rint) #:cost 0.1870570107858246]
  [round.f64 #:spec (round x) #:impl (from-libm 'round) #:cost 0.18736517719568596]
  [sqrt.f64 #:spec (sqrt x) #:impl (from-libm 'sqrt) #:cost 0.35932203389830625]
  [tanh.f64 #:spec (tanh x) #:impl (from-libm 'tanh) #:cost 1.6425269645608633]
  [tgamma.f64 #:spec (tgamma x) #:impl (from-libm 'tgamma) #:cost 2.202773497688754]
  [trunc.f64 #:spec (trunc x) #:impl (from-libm 'trunc) #:cost 0.18983050847457622])

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [pow.f64 #:spec (pow x y) #:impl (from-libm 'pow) #:cost 2.984591679506936]
  [atan2.f64 #:spec (atan2 x y) #:impl (from-libm 'atan2) #:cost 4.067488443759635]
  [copysign.f64 #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost 0.19969183359013912]
  [fdim.f64 #:spec (fdim x y) #:impl (from-libm 'fdim) #:cost 1.3648690292758106]
  [fmax.f64 #:spec (fmax x y) #:impl (from-libm 'fmax) #:cost 0.19876733436055496]
  [fmin.f64 #:spec (fmin x y) #:impl (from-libm 'fmin) #:cost 0.20030816640986143]
  [fmod.f64 #:spec (fmod x y) #:impl (from-libm 'fmod) #:cost 2.853929121725733]
  [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 3.83451463790447])