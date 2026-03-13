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
                  #:cost 0.168)

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.2]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 0.177]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 0.167]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.253])

(define-operations ([x <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [fabs.f32 #:spec (fabs x) #:impl (from-libm 'fabsf) #:cost 0.169]
  [sin.f32 #:spec (sin x) #:impl (from-libm 'sinf) #:cost 1.728]
  [cos.f32 #:spec (cos x) #:impl (from-libm 'cosf) #:cost 1.689]
  [tan.f32 #:spec (tan x) #:impl (from-libm 'tanf) #:cost 1.812]
  [sinh.f32 #:spec (sinh x) #:impl (from-libm 'sinhf) #:cost 1.718]
  [cosh.f32 #:spec (cosh x) #:impl (from-libm 'coshf) #:cost 1.500]
  [acos.f32 #:spec (acos x) #:impl (from-libm 'acosf) #:cost 1.213]
  [acosh.f32 #:spec (acosh x) #:impl (from-libm 'acoshf) #:cost 1.528]
  [asin.f32 #:spec (asin x) #:impl (from-libm 'asinf) #:cost 2.575]
  [asinh.f32 #:spec (asinh x) #:impl (from-libm 'asinhf) #:cost 1.442]
  [atan.f32 #:spec (atan x) #:impl (from-libm 'atanf) #:cost 1.506]
  [atanh.f32 #:spec (atanh x) #:impl (from-libm 'atanhf) #:cost 1.674]
  [cbrt.f32 #:spec (cbrt x) #:impl (from-libm 'cbrtf) #:cost 1.510]
  [ceil.f32 #:spec (ceil x) #:impl (from-libm 'ceilf) #:cost 0.198]
  [erf.f32 #:spec (erf x) #:impl (from-libm 'erff) #:cost 1.610]
  [exp.f32 #:spec (exp x) #:impl (from-libm 'expf) #:cost 1.244]
  [exp2.f32 #:spec (exp2 x) #:impl (from-libm 'exp2f) #:cost 1.214]
  [floor.f32 #:spec (floor x) #:impl (from-libm 'floorf) #:cost 0.160]
  [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf) #:cost 2.710]
  [log.f32 #:spec (log x) #:impl (from-libm 'logf) #:cost 1.307]
  [log10.f32 #:spec (log10 x) #:impl (from-libm 'log10f) #:cost 1.926]
  [log2.f32 #:spec (log2 x) #:impl (from-libm 'log2f) #:cost 1.512]
  [logb.f32 #:spec (logb x) #:impl (from-libm 'logbf) #:cost 0.564]
  [rint.f32 #:spec (rint x) #:impl (from-libm 'rintf) #:cost 0.068]
  [round.f32 #:spec (round x) #:impl (from-libm 'roundf) #:cost 0.170]
  [sqrt.f32 #:spec (sqrt x) #:impl (from-libm 'sqrtf) #:cost 0.167]
  [tanh.f32 #:spec (tanh x) #:impl (from-libm 'tanhf) #:cost 1.347]
  [tgamma.f32 #:spec (tgamma x) #:impl (from-libm 'tgammaf) #:cost 2.489]
  [trunc.f32 #:spec (trunc x) #:impl (from-libm 'truncf) #:cost 0.159])

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [pow.f32 #:spec (pow x y) #:impl (from-libm 'powf) #:cost 2.165]
  [atan2.f32 #:spec (atan2 x y) #:impl (from-libm 'atan2f) #:cost 2.649]
  [copysign.f32 #:spec (copysign x y) #:impl (from-libm 'copysignf) #:cost 0.069]
  [fdim.f32 #:spec (fdim x y) #:impl (from-libm 'fdimf) #:cost 1.199]
  [fmax.f32 #:spec (fmax x y) #:impl (from-libm 'fmaxf) #:cost 0.177]
  [fmin.f32 #:spec (fmin x y) #:impl (from-libm 'fminf) #:cost 0.086]
  [fmod.f32 #:spec (fmod x y) #:impl (from-libm 'fmodf) #:cost 2.190]
  [remainder.f32 #:spec (remainder x y) #:impl (from-libm 'remainderf) #:cost 2.557])

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
                  #:cost 0.168)

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [+.f64 #:spec (+ x y) #:impl + #:cost 0.2]
  [-.f64 #:spec (- x y) #:impl - #:cost 0.177]
  [*.f64 #:spec (* x y) #:impl * #:cost 0.167]
  [/.f64 #:spec (/ x y) #:impl / #:cost 0.253])

(define-operations ([x <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [fabs.f64 #:spec (fabs x) #:impl (from-libm 'fabs) #:cost 0.169]
  [sin.f64 #:spec (sin x) #:impl (from-libm 'sin) #:cost 1.728]
  [cos.f64 #:spec (cos x) #:impl (from-libm 'cos) #:cost 1.689]
  [tan.f64 #:spec (tan x) #:impl (from-libm 'tan) #:cost 1.812]
  [sinh.f64 #:spec (sinh x) #:impl (from-libm 'sinh) #:cost 1.718]
  [cosh.f64 #:spec (cosh x) #:impl (from-libm 'cosh) #:cost 1.500]
  [acos.f64 #:spec (acos x) #:impl (from-libm 'acos) #:cost 1.213]
  [acosh.f64 #:spec (acosh x) #:impl (from-libm 'acosh) #:cost 1.528]
  [asin.f64 #:spec (asin x) #:impl (from-libm 'asin) #:cost 2.575]
  [asinh.f64 #:spec (asinh x) #:impl (from-libm 'asinh) #:cost 1.442]
  [atan.f64 #:spec (atan x) #:impl (from-libm 'atan) #:cost 1.506]
  [atanh.f64 #:spec (atanh x) #:impl (from-libm 'atanh) #:cost 1.674]
  [cbrt.f64 #:spec (cbrt x) #:impl (from-libm 'cbrt) #:cost 1.510]
  [ceil.f64 #:spec (ceil x) #:impl (from-libm 'ceil) #:cost 0.198]
  [erf.f64 #:spec (erf x) #:impl (from-libm 'erf) #:cost 1.610]
  [exp.f64 #:spec (exp x) #:impl (from-libm 'exp) #:cost 1.244]
  [exp2.f64 #:spec (exp2 x) #:impl (from-libm 'exp2) #:cost 1.214]
  [floor.f64 #:spec (floor x) #:impl (from-libm 'floor) #:cost 0.160]
  [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma) #:cost 2.710]
  [log.f64 #:spec (log x) #:impl (from-libm 'log) #:cost 1.307]
  [log10.f64 #:spec (log10 x) #:impl (from-libm 'log10) #:cost 1.926]
  [log2.f64 #:spec (log2 x) #:impl (from-libm 'log2) #:cost 1.512]
  [logb.f64 #:spec (logb x) #:impl (from-libm 'logb) #:cost 0.564]
  [rint.f64 #:spec (rint x) #:impl (from-libm 'rint) #:cost 0.068]
  [round.f64 #:spec (round x) #:impl (from-libm 'round) #:cost 0.170]
  [sqrt.f64 #:spec (sqrt x) #:impl (from-libm 'sqrt) #:cost 0.167]
  [tanh.f64 #:spec (tanh x) #:impl (from-libm 'tanh) #:cost 1.347]
  [tgamma.f64 #:spec (tgamma x) #:impl (from-libm 'tgamma) #:cost 2.489]
  [trunc.f64 #:spec (trunc x) #:impl (from-libm 'trunc) #:cost 0.159])

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [pow.f64 #:spec (pow x y) #:impl (from-libm 'pow) #:cost 2.165]
  [atan2.f64 #:spec (atan2 x y) #:impl (from-libm 'atan2) #:cost 2.649]
  [copysign.f64 #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost 0.069]
  [fdim.f64 #:spec (fdim x y) #:impl (from-libm 'fdim) #:cost 1.199]
  [fmax.f64 #:spec (fmax x y) #:impl (from-libm 'fmax) #:cost 0.177]
  [fmin.f64 #:spec (fmin x y) #:impl (from-libm 'fmin) #:cost 0.086]
  [fmod.f64 #:spec (fmod x y) #:impl (from-libm 'fmod) #:cost 2.190]
  [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 2.557])


;;; (define-operation (cosquot.f64 [x <binary64>] [y <binary64>])
;;;   <binary64>
;;;   #:spec (cos (/ x y))
;;;   #:impl (from-accelerators 'cosquot)
;;;   #:fpcore (! :precision binary64 (cosquot x y))
;;;   #:cost 1918618)

;;; (define-operation (sinquot.f64 [x <binary64>] [y <binary64>])
;;;   <binary64>
;;;   #:spec (sin (/ x y))
;;;   #:impl (from-accelerators 'sinquot)
;;;   #:fpcore (! :precision binary64 (sinquot x y))
;;;   #:cost 1926771)

;;; (define-operation (sinprod.f64 [x <binary64>] [y <binary64>])
;;;   <binary64>
;;;   #:spec (sin (* x y))
;;;   #:impl (from-accelerators 'sinprod)
;;;   #:fpcore (! :precision binary64 (sinprod x y))
;;;   #:cost 4.086)

;;; (define-operation (cosprod.f64 [x <binary64>] [y <binary64>])
;;;   <binary64>
;;;   #:spec (cos (* x y))
;;;   #:impl (from-accelerators 'cosprod)
;;;   #:fpcore (! :precision binary64 (cosprod x y))
;;;   #:cost 4.086)

;;; (define-operation (log1pmd.f64 [x <binary64>])
;;;   <binary64>
;;;   #:spec (log (/ (+ 1 x) (- 1 x)))
;;;   #:impl (from-accelerators 'log1pmd)
;;;   #:fpcore (! :precision binary64 (log1pmd x))
;;;   #:cost 1.093)

;;; (define-operation (invgud.f64 [x <binary64>])
;;;   <binary64>
;;;   #:spec (log (tan (* (+ (+ x x) (PI)) 1/4)))
;;;   #:impl (from-accelerators 'invgud)
;;;   #:fpcore (! :precision binary64 (invgud x))
;;;   #:cost 9.955)

;;; (define-operation (hypot.f64 [x <binary64>] [y <binary64>])
;;;   <binary64>
;;;   #:spec (sqrt (+ (* x x) (* y y)))
;;;   #:impl (from-accelerators 'hypot)
;;;   #:fpcore (! :precision binary64 (hypot x y))
;;;   #:cost 0.864)

;;; (define-operation (verdcos.f64 [x <binary64>])
;;;   <binary64>
;;;   #:spec (- (cos (+ x x)) 1)
;;;   #:impl (from-accelerators 'verdcos)
;;;   #:fpcore (! :precision binary64 (verdcos x))
;;;   #:cost 2.562)

(define-operation (powcos.f64 [x <binary64>] [y <binary64>])
  <binary64>
  #:spec (pow (cos x) y)
  #:impl (from-accelerators 'powcos)
  #:fpcore (! :precision binary64 (powcos x y))
  #:cost 8.155)

(define-operation (powcos2.f64 [x <binary64>])
  <binary64>
  #:spec (pow (cos x) 2)
  #:impl (from-accelerators 'powcos2)
  #:fpcore (! :precision binary64 (powcos2 x))
  #:cost 4.815)

(define-operation (powcos4.f64 [x <binary64>])
  <binary64>
  #:spec (pow (cos x) 4)
  #:impl (from-accelerators 'powcos4)
  #:fpcore (! :precision binary64 (powcos4 x))
  #:cost 4.930)

(define-operation (powcos6.f64 [x <binary64>])
  <binary64>
  #:spec (pow (cos x) 6)
  #:impl (from-accelerators 'powcos6)
  #:fpcore (! :precision binary64 (powcos6 x))
  #:cost 4.798)

;;; (define-operation (pow1ms.f64 [x <binary64>] [y <binary64>])
;;;   <binary64>
;;;   #:spec (pow (* (- 1 x) (- 1 x)) y) 
;;;   #:impl (from-accelerators 'pow1ms)
;;;   #:fpcore (! :precision binary64 (pow1ms x y))
;;;   #:cost 4.020)
