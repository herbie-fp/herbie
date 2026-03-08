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
                  #:cost 0.1604774535809016)

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.2]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 0.15729442970822285]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 0.16233421750663113]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.1623342175066312])

(define-operations ([x <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [fabs.f32 #:spec (fabs x) #:impl (from-libm 'fabsf) #:cost 0.16339522546419077]
  [sin.f32 #:spec (sin x) #:impl (from-libm 'sinf) #:cost 1.849602122015914]
  [cos.f32 #:spec (cos x) #:impl (from-libm 'cosf) #:cost 2.1132625994694925]
  [tan.f32 #:spec (tan x) #:impl (from-libm 'tanf) #:cost 2.0647214854111398]
  [sinh.f32 #:spec (sinh x) #:impl (from-libm 'sinhf) #:cost 1.8466843501326242]
  [cosh.f32 #:spec (cosh x) #:impl (from-libm 'coshf) #:cost 1.5644562334217484]
  [acos.f32 #:spec (acos x) #:impl (from-libm 'acosf) #:cost 1.2037135278514572]
  [acosh.f32 #:spec (acosh x) #:impl (from-libm 'acoshf) #:cost 1.740053050397878]
  [asin.f32 #:spec (asin x) #:impl (from-libm 'asinf) #:cost 2.8885941644562316]
  [asinh.f32 #:spec (asinh x) #:impl (from-libm 'asinhf) #:cost 1.8161803713527835]
  [atan.f32 #:spec (atan x) #:impl (from-libm 'atanf) #:cost 1.7543766578249333]
  [atanh.f32 #:spec (atanh x) #:impl (from-libm 'atanhf) #:cost 1.7429708222811646]
  [cbrt.f32 #:spec (cbrt x) #:impl (from-libm 'cbrtf) #:cost 1.7570291777188307]
  [ceil.f32 #:spec (ceil x) #:impl (from-libm 'ceilf) #:cost 0.16233421750663143]
  [erf.f32 #:spec (erf x) #:impl (from-libm 'erff) #:cost 1.7509283819628623]
  [exp.f32 #:spec (exp x) #:impl (from-libm 'expf) #:cost 1.317771883289124]
  [exp2.f32 #:spec (exp2 x) #:impl (from-libm 'exp2f) #:cost 1.3342175066312973]
  [floor.f32 #:spec (floor x) #:impl (from-libm 'floorf) #:cost 0.15305039787798388]
  [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf) #:cost 3.319893899204241]
  [log.f32 #:spec (log x) #:impl (from-libm 'logf) #:cost 1.2010610079575588]
  [log10.f32 #:spec (log10 x) #:impl (from-libm 'log10f) #:cost 1.3061007957559676]
  [log2.f32 #:spec (log2 x) #:impl (from-libm 'log2f) #:cost 1.2506631299734736]
  [logb.f32 #:spec (logb x) #:impl (from-libm 'logbf) #:cost 0.6071618037135276]
  [rint.f32 #:spec (rint x) #:impl (from-libm 'rintf) #:cost 0.16896551724137895]
  [round.f32 #:spec (round x) #:impl (from-libm 'roundf) #:cost 0.1623342175066311]
  [sqrt.f32 #:spec (sqrt x) #:impl (from-libm 'sqrtf) #:cost 0.30848806366047715]
  [tanh.f32 #:spec (tanh x) #:impl (from-libm 'tanhf) #:cost 1.370291777188328]
  [tgamma.f32 #:spec (tgamma x) #:impl (from-libm 'tgammaf) #:cost 1.890185676392571]
  [trunc.f32 #:spec (trunc x) #:impl (from-libm 'truncf) #:cost 0.16419098143236055])

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [pow.f32 #:spec (pow x y) #:impl (from-libm 'powf) #:cost 2.4204244031830218]
  [atan2.f32 #:spec (atan2 x y) #:impl (from-libm 'atan2f) #:cost 3.4103448275862043]
  [copysign.f32 #:spec (copysign x y) #:impl (from-libm 'copysignf) #:cost 0.16074270557029158]
  [fdim.f32 #:spec (fdim x y) #:impl (from-libm 'fdimf) #:cost 1.1522546419098134]
  [fmax.f32 #:spec (fmax x y) #:impl (from-libm 'fmaxf) #:cost 0.16870026525198942]
  [fmin.f32 #:spec (fmin x y) #:impl (from-libm 'fminf) #:cost 0.15968169761273193]
  [fmod.f32 #:spec (fmod x y) #:impl (from-libm 'fmodf) #:cost 2.5676392572944255]
  [remainder.f32 #:spec (remainder x y) #:impl (from-libm 'remainderf) #:cost 3.292307692307691])

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
                  #:cost 0.1604774535809016)

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [+.f64 #:spec (+ x y) #:impl + #:cost 0.2]
  [-.f64 #:spec (- x y) #:impl - #:cost 0.15729442970822285]
  [*.f64 #:spec (* x y) #:impl * #:cost 0.16233421750663113]
  [/.f64 #:spec (/ x y) #:impl / #:cost 0.1623342175066312])

(define-operations ([x <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [fabs.f64 #:spec (fabs x) #:impl (from-libm 'fabs) #:cost 0.16339522546419077]
  [sin.f64 #:spec (sin x) #:impl (from-libm 'sin) #:cost 1.849602122015914]
  [cos.f64 #:spec (cos x) #:impl (from-libm 'cos) #:cost 2.1132625994694925]
  [tan.f64 #:spec (tan x) #:impl (from-libm 'tan) #:cost 2.0647214854111398]
  [sinh.f64 #:spec (sinh x) #:impl (from-libm 'sinh) #:cost 1.8466843501326242]
  [cosh.f64 #:spec (cosh x) #:impl (from-libm 'cosh) #:cost 1.5644562334217484]
  [acos.f64 #:spec (acos x) #:impl (from-libm 'acos) #:cost 1.2037135278514572]
  [acosh.f64 #:spec (acosh x) #:impl (from-libm 'acosh) #:cost 1.740053050397878]
  [asin.f64 #:spec (asin x) #:impl (from-libm 'asin) #:cost 2.8885941644562316]
  [asinh.f64 #:spec (asinh x) #:impl (from-libm 'asinh) #:cost 1.8161803713527835]
  [atan.f64 #:spec (atan x) #:impl (from-libm 'atan) #:cost 1.7543766578249333]
  [atanh.f64 #:spec (atanh x) #:impl (from-libm 'atanh) #:cost 1.7429708222811646]
  [cbrt.f64 #:spec (cbrt x) #:impl (from-libm 'cbrt) #:cost 1.7570291777188307]
  [ceil.f64 #:spec (ceil x) #:impl (from-libm 'ceil) #:cost 0.16233421750663143]
  [erf.f64 #:spec (erf x) #:impl (from-libm 'erf) #:cost 1.7509283819628623]
  [exp.f64 #:spec (exp x) #:impl (from-libm 'exp) #:cost 1.317771883289124]
  [exp2.f64 #:spec (exp2 x) #:impl (from-libm 'exp2) #:cost 1.3342175066312973]
  [floor.f64 #:spec (floor x) #:impl (from-libm 'floor) #:cost 0.15305039787798388]
  [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma) #:cost 3.319893899204241]
  [log.f64 #:spec (log x) #:impl (from-libm 'log) #:cost 1.2010610079575588]
  [log10.f64 #:spec (log10 x) #:impl (from-libm 'log10) #:cost 1.3061007957559676]
  [log2.f64 #:spec (log2 x) #:impl (from-libm 'log2) #:cost 1.2506631299734736]
  [logb.f64 #:spec (logb x) #:impl (from-libm 'logb) #:cost 0.6071618037135276]
  [rint.f64 #:spec (rint x) #:impl (from-libm 'rint) #:cost 0.16896551724137895]
  [round.f64 #:spec (round x) #:impl (from-libm 'round) #:cost 0.1623342175066311]
  [sqrt.f64 #:spec (sqrt x) #:impl (from-libm 'sqrt) #:cost 0.30848806366047715]
  [tanh.f64 #:spec (tanh x) #:impl (from-libm 'tanh) #:cost 1.370291777188328]
  [tgamma.f64 #:spec (tgamma x) #:impl (from-libm 'tgamma) #:cost 1.890185676392571]
  [trunc.f64 #:spec (trunc x) #:impl (from-libm 'trunc) #:cost 0.16419098143236055])

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [pow.f64 #:spec (pow x y) #:impl (from-libm 'pow) #:cost 2.4204244031830218]
  [atan2.f64 #:spec (atan2 x y) #:impl (from-libm 'atan2) #:cost 3.4103448275862043]
  [copysign.f64 #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost 0.16074270557029158]
  [fdim.f64 #:spec (fdim x y) #:impl (from-libm 'fdim) #:cost 1.1522546419098134]
  [fmax.f64 #:spec (fmax x y) #:impl (from-libm 'fmax) #:cost 0.16870026525198942]
  [fmin.f64 #:spec (fmin x y) #:impl (from-libm 'fmin) #:cost 0.15968169761273193]
  [fmod.f64 #:spec (fmod x y) #:impl (from-libm 'fmod) #:cost 2.5676392572944255]
  [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 3.292307692307691])

(define-operation (powcos.f64 [x <binary64>] [y <binary64>])
  <binary64>
  #:spec (pow (cos x) y)
  #:impl (from-accelerators 'powcos)
  #:fpcore (! :precision binary64 (powcos x y))
  #:cost 13.704509283819617)

(define-operation (powcos2.f64 [x <binary64>])
  <binary64>
  #:spec (pow (cos x) 2)
  #:impl (from-accelerators 'powcos2)
  #:fpcore (! :precision binary64 (powcos2 x))
  #:cost 6.671883289124661)

(define-operation (powcos4.f64 [x <binary64>])
  <binary64>
  #:spec (pow (cos x) 4)
  #:impl (from-accelerators 'powcos4)
  #:fpcore (! :precision binary64 (powcos4 x))
  #:cost 7.368169761273204)

(define-operation (powcos6.f64 [x <binary64>])
  <binary64>
  #:spec (pow (cos x) 6)
  #:impl (from-accelerators 'powcos6)
  #:fpcore (! :precision binary64 (powcos6 x))
  #:cost 6.894164456233416)

;;; (define-operation (ncos1p.f64 [x <binary64>])
;;;   <binary64>
;;;   #:spec (- 1 (cos x))
;;;   #:impl (from-accelerators 'ncos1p)
;;;   #:fpcore (! :precision binary64 (ncos1p x))
;;;   #:cost 12800)
