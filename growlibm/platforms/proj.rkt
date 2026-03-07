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
                  #:cost 0.1486933257700667)

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.2]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 0.21013950416861515]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 0.4092162494523331]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.7111355694356547])

(define-operations ([x <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [fabs.f32 #:spec (fabs x) #:impl (from-libm 'fabsf) #:cost 0.14881439447655762]
  [sin.f32 #:spec (sin x) #:impl (from-libm 'sinf) #:cost 13.702441270036127]
  [cos.f32 #:spec (cos x) #:impl (from-libm 'cosf) #:cost 13.244872888441108]
  [tan.f32 #:spec (tan x) #:impl (from-libm 'tanf) #:cost 14.940014689105281]
  [sinh.f32 #:spec (sinh x) #:impl (from-libm 'sinhf) #:cost 5.262005380361041]
  [cosh.f32 #:spec (cosh x) #:impl (from-libm 'coshf) #:cost 3.759272021487575]
  [acos.f32 #:spec (acos x) #:impl (from-libm 'acosf) #:cost 1.3383364482420674]
  [acosh.f32 #:spec (acosh x) #:impl (from-libm 'acoshf) #:cost 2.6487856089099777]
  [asin.f32 #:spec (asin x) #:impl (from-libm 'asinf) #:cost 1.2906108254896183]
  [asinh.f32 #:spec (asinh x) #:impl (from-libm 'asinhf) #:cost 3.4949364918437307]
  [atan.f32 #:spec (atan x) #:impl (from-libm 'atanf) #:cost 4.493566849186905]
  [atanh.f32 #:spec (atanh x) #:impl (from-libm 'atanhf) #:cost 1.2625783401310988]
  [cbrt.f32 #:spec (cbrt x) #:impl (from-libm 'cbrtf) #:cost 6.742743391490051]
  [ceil.f32 #:spec (ceil x) #:impl (from-libm 'ceilf) #:cost 0.6330310143314017]
  [erf.f32 #:spec (erf x) #:impl (from-libm 'erff) #:cost 3.003521575417126]
  [exp.f32 #:spec (exp x) #:impl (from-libm 'expf) #:cost 4.273762591039643]
  [exp2.f32 #:spec (exp2 x) #:impl (from-libm 'exp2f) #:cost 3.1662114596187134]
  [floor.f32 #:spec (floor x) #:impl (from-libm 'floorf) #:cost 0.6553639575074235]
  [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf) #:cost 6.502350891823409]
  [log.f32 #:spec (log x) #:impl (from-libm 'logf) #:cost 2.1245898585909013]
  [log10.f32 #:spec (log10 x) #:impl (from-libm 'log10f) #:cost 3.7700196207501566]
  [log2.f32 #:spec (log2 x) #:impl (from-libm 'log2f) #:cost 1.933940934557282]
  [logb.f32 #:spec (logb x) #:impl (from-libm 'logbf) #:cost 0.7167811386808173]
  [rint.f32 #:spec (rint x) #:impl (from-libm 'rintf) #:cost 0.5962149096535363]
  [round.f32 #:spec (round x) #:impl (from-libm 'roundf) #:cost 2.4054800522373365]
  [sqrt.f32 #:spec (sqrt x) #:impl (from-libm 'sqrtf) #:cost 0.6165176237514782]
  [tanh.f32 #:spec (tanh x) #:impl (from-libm 'tanhf) #:cost 3.0095866519634473]
  [tgamma.f32 #:spec (tgamma x) #:impl (from-libm 'tgammaf) #:cost 7.892269593763681]
  [trunc.f32 #:spec (trunc x) #:impl (from-libm 'truncf) #:cost 0.7143656909911761])

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [pow.f32 #:spec (pow x y) #:impl (from-libm 'powf) #:cost 5.825077308295526]
  [atan2.f32 #:spec (atan2 x y) #:impl (from-libm 'atan2f) #:cost 7.6975462421077765]
  [copysign.f32 #:spec (copysign x y) #:impl (from-libm 'copysignf) #:cost 0.22880122931301985]
  [fdim.f32 #:spec (fdim x y) #:impl (from-libm 'fdimf) #:cost 2.16844339932142]
  [fmax.f32 #:spec (fmax x y) #:impl (from-libm 'fmaxf) #:cost 0.32273255457087296]
  [fmin.f32 #:spec (fmin x y) #:impl (from-libm 'fminf) #:cost 0.31357810942325126]
  [fmod.f32 #:spec (fmod x y) #:impl (from-libm 'fmodf) #:cost 1.5430684990655252]
  [remainder.f32 #:spec (remainder x y) #:impl (from-libm 'remainderf) #:cost 2.55404024476198])

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
                  #:cost 0.1486933257700667)

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [+.f64 #:spec (+ x y) #:impl + #:cost 0.2]
  [-.f64 #:spec (- x y) #:impl - #:cost 0.21013950416861515]
  [*.f64 #:spec (* x y) #:impl * #:cost 0.4092162494523331]
  [/.f64 #:spec (/ x y) #:impl / #:cost 0.7111355694356547])

(define-operations ([x <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [fabs.f64 #:spec (fabs x) #:impl (from-libm 'fabs) #:cost 0.14881439447655762]
  [sin.f64 #:spec (sin x) #:impl (from-libm 'sin) #:cost 13.702441270036127]
  [cos.f64 #:spec (cos x) #:impl (from-libm 'cos) #:cost 13.244872888441108]
  [tan.f64 #:spec (tan x) #:impl (from-libm 'tan) #:cost 14.940014689105281]
  [sinh.f64 #:spec (sinh x) #:impl (from-libm 'sinh) #:cost 5.262005380361041]
  [cosh.f64 #:spec (cosh x) #:impl (from-libm 'cosh) #:cost 3.759272021487575]
  [acos.f64 #:spec (acos x) #:impl (from-libm 'acos) #:cost 1.3383364482420674]
  [acosh.f64 #:spec (acosh x) #:impl (from-libm 'acosh) #:cost 2.6487856089099777]
  [asin.f64 #:spec (asin x) #:impl (from-libm 'asin) #:cost 1.2906108254896183]
  [asinh.f64 #:spec (asinh x) #:impl (from-libm 'asinh) #:cost 3.4949364918437307]
  [atan.f64 #:spec (atan x) #:impl (from-libm 'atan) #:cost 4.493566849186905]
  [atanh.f64 #:spec (atanh x) #:impl (from-libm 'atanh) #:cost 1.2625783401310988]
  [cbrt.f64 #:spec (cbrt x) #:impl (from-libm 'cbrt) #:cost 6.742743391490051]
  [ceil.f64 #:spec (ceil x) #:impl (from-libm 'ceil) #:cost 0.6330310143314017]
  [erf.f64 #:spec (erf x) #:impl (from-libm 'erf) #:cost 3.003521575417126]
  [exp.f64 #:spec (exp x) #:impl (from-libm 'exp) #:cost 4.273762591039643]
  [exp2.f64 #:spec (exp2 x) #:impl (from-libm 'exp2) #:cost 3.1662114596187134]
  [floor.f64 #:spec (floor x) #:impl (from-libm 'floor) #:cost 0.6553639575074235]
  [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma) #:cost 6.502350891823409]
  [log.f64 #:spec (log x) #:impl (from-libm 'log) #:cost 2.1245898585909013]
  [log10.f64 #:spec (log10 x) #:impl (from-libm 'log10) #:cost 3.7700196207501566]
  [log2.f64 #:spec (log2 x) #:impl (from-libm 'log2) #:cost 1.933940934557282]
  [logb.f64 #:spec (logb x) #:impl (from-libm 'logb) #:cost 0.7167811386808173]
  [rint.f64 #:spec (rint x) #:impl (from-libm 'rint) #:cost 0.5962149096535363]
  [round.f64 #:spec (round x) #:impl (from-libm 'round) #:cost 2.4054800522373365]
  [sqrt.f64 #:spec (sqrt x) #:impl (from-libm 'sqrt) #:cost 0.6165176237514782]
  [tanh.f64 #:spec (tanh x) #:impl (from-libm 'tanh) #:cost 3.0095866519634473]
  [tgamma.f64 #:spec (tgamma x) #:impl (from-libm 'tgamma) #:cost 7.892269593763681]
  [trunc.f64 #:spec (trunc x) #:impl (from-libm 'trunc) #:cost 0.7143656909911761])

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [pow.f64 #:spec (pow x y) #:impl (from-libm 'pow) #:cost 5.825077308295526]
  [atan2.f64 #:spec (atan2 x y) #:impl (from-libm 'atan2) #:cost 7.6975462421077765]
  [copysign.f64 #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost 0.22880122931301985]
  [fdim.f64 #:spec (fdim x y) #:impl (from-libm 'fdim) #:cost 2.16844339932142]
  [fmax.f64 #:spec (fmax x y) #:impl (from-libm 'fmax) #:cost 0.32273255457087296]
  [fmin.f64 #:spec (fmin x y) #:impl (from-libm 'fmin) #:cost 0.31357810942325126]
  [fmod.f64 #:spec (fmod x y) #:impl (from-libm 'fmod) #:cost 1.5430684990655252]
  [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 2.55404024476198])

(define-operation (sinprod.f64 [x <binary64>] [y <binary64>])
  <binary64>
  #:spec (sin (* x y))
  #:impl (from-accelerators 'sinprod)
  #:fpcore (! :precision binary64 (sinprod x y))
  #:cost 25.368719190448275)

(define-operation (cosprod.f64 [x <binary64>] [y <binary64>])
  <binary64>
  #:spec (cos (* x y))
  #:impl (from-accelerators 'cosprod)
  #:fpcore (! :precision binary64 (cosprod x y))
  #:cost 25.458416285857595)

(define-operation (log1pmd.f64 [x <binary64>])
  <binary64>
  #:spec (log (/ (+ 1 x) (- 1 x)))
  #:impl (from-accelerators 'log1pmd)
  #:fpcore (! :precision binary64 (log1pmd x))
  #:cost 7.914834514740955)

;;; (define-operation (invgudf.f32 [x <binary32>])
;;;   <binary32>
;;;   #:spec (log (tan (* (+ (+ x x) (PI)) 1/4)))
;;;   #:impl (from-accelerators 'invgudf)
;;;   #:fpcore (! :precision binary32 (invgudf x))
;;;   #:cost 0)

(define-operation (invgud.f64 [x <binary64>])
  <binary64>
  #:spec (log (tan (* (+ (+ x x) (PI)) 1/4)))
  #:impl (from-accelerators 'invgud)
  #:fpcore (! :precision binary64 (invgud x))
  #:cost 29.704927623349334)

;;; (define-operation (invgud.f64 [x <binary64>])
;;;   <binary64>
;;;   #:spec (log (tan (+ (* (PI) 1/4) (* x 1/2))))
;;;   #:impl (from-accelerators 'invgud)
;;;   #:fpcore (! :precision binary64 (invgud x))
;;;   #:cost 25000)

(define-operation (hypot.f64 [x <binary64>] [y <binary64>])
  <binary64>
  #:spec (sqrt (+ (* x x) (* y y)))
  #:impl (from-accelerators 'hypot)
  #:fpcore (! :precision binary64 (hypot x y))
  #:cost 1.8750448187038438)

(define-operation (verdcos.f64 [x <binary64>])
  <binary64>
  #:spec (- (cos (+ x x)) 1)
  #:impl (from-accelerators 'verdcos)
  #:fpcore (! :precision binary64 (verdcos x))
  #:cost 13.933354640296807)

;;; (define-operation (ncos1p.f64 [x <binary64>])
;;;   <binary64>
;;;   #:spec (- 1 (cos x))
;;;   #:impl (from-accelerators 'ncos1p)
;;;   #:fpcore (! :precision binary64 (ncos1p x))
;;;   #:cost 12800)

;;; (define-operation (sindivpz.f64 [x <binary64>] [y <binary64>] [z <binary64>])
;;;   <binary64>
;;;   #:spec (sin (+ (/ x y) z))
;;;   #:impl (from-accelerators 'sindivpz)
;;;   #:fpcore (! :precision binary64 (sindivpz x y z))
;;;   #:cost 0)

;;; (define-operation (cosdivpz.f64 [x <binary64>] [y <binary64>] [z <binary64>])
;;;   <binary64>
;;;   #:spec (cos (+ (/ x y) z))
;;;   #:impl (from-accelerators 'cosdivpz)
;;;   #:fpcore (! :precision binary64 (cosdivpz x y z))
;;;   #:cost 0)

;;; (define-operation (sinquot.f64 [x <binary64>] [y <binary64>])
;;;   <binary64>
;;;   #:spec (sin (/ x y))
;;;   #:impl (from-accelerators 'sinquot)
;;;   #:fpcore (! :precision binary64 (sinquot x y))
;;;   #:cost 0)

;;; (define-operation (cosquot.f64 [x <binary64>] [y <binary64>])
;;;   <binary64>
;;;   #:spec (cos (/ x y))
;;;   #:impl (from-accelerators 'cosquot)
;;;   #:fpcore (! :precision binary64 (cosquot x y))
;;;   #:cost 10000)
