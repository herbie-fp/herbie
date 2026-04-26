#lang s-exp "../syntax/platform-language.rkt"

;; C/C++ platform with a full libm

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
                  #:cost 0.125)

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.200]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 0.200]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 0.250]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.350])

(define-operations ([x <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [fabs.f32 #:spec (fabs x) #:impl (from-libm 'fabsf) #:cost 0.125]
  [sqrt.f32 #:spec (sqrt x) #:impl (from-libm 'sqrtf) #:cost 0.250]
  [exp2.f32 #:spec (exp2 x) #:impl (from-libm 'exp2f) #:cost 1.175]
  [log.f32 #:spec (log x) #:impl (from-libm 'logf) #:cost 0.250]
  [rint.f32 #:spec (rint x) #:impl (from-libm 'rintf) #:cost 0.250])

(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [fmax.f32 #:spec (fmax x y) #:impl (from-libm 'fmaxf) #:cost 0.250]
  [fmin.f32 #:spec (fmin x y) #:impl (from-libm 'fminf) #:cost 0.250]
  [copysign.f32 #:spec (copysign x y) #:impl (from-libm 'copysignf) #:cost 0.200]
  [remainder.f32 #:spec (remainder x y) #:impl (from-libm 'remainderf) #:cost 1.000])

(define-operation (fma.f32 [x <binary32>] [y <binary32>] [z <binary32>])
                  <binary32>
                  #:spec (+ (* x y) z)
                  #:impl (from-libm 'fmaf)
                  #:fpcore (! :precision binary32 (fma x y z))
                  #:cost 0.375)

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
                  #:cost 0.125)

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [+.f64 #:spec (+ x y) #:impl + #:cost 0.200]
  [-.f64 #:spec (- x y) #:impl - #:cost 0.200]
  [*.f64 #:spec (* x y) #:impl * #:cost 0.250]
  [/.f64 #:spec (/ x y) #:impl / #:cost 0.350])

(define-operations ([x <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [fabs.f64 #:spec (fabs x) #:impl (from-libm 'fabs) #:cost 0.125]
  [sqrt.f64 #:spec (sqrt x) #:impl (from-libm 'sqrt) #:cost 0.250]
  [exp2.f64 #:spec (exp2 x) #:impl (from-libm 'exp2) #:cost 1.175]
  [log.f64 #:spec (log x) #:impl (from-libm 'log) #:cost 0.250]
  [rint.f64 #:spec (rint x) #:impl (from-libm 'rint) #:cost 0.250])
(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [fmax.f64 #:spec (fmax x y) #:impl (from-libm 'fmax) #:cost 0.250]
  [fmin.f64 #:spec (fmin x y) #:impl (from-libm 'fmin) #:cost 0.250]
  [copysign.f64 #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost 0.200]
  [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 1.000])

(define-operation (fma.f64 [x <binary64>] [y <binary64>] [z <binary64>])
                  <binary64>
                  #:spec (+ (* x y) z)
                  #:impl (from-libm 'fma)
                  #:fpcore (! :precision binary64 (fma x y z))
                  #:cost 0.375)

(define-operation (rem2pi.f64 [x <binary64>])
                  <binary64>
                  #:spec (remainder x (* 2 (PI)))
                  #:impl (from-accelerators 'rem2pi)
                  #:fpcore (! :precision binary64 (rem2pi x))
                  #:cost 1.000)

(define-operation (sinpoly17.f64 [x <binary64>])
                  <binary64>
                  #:spec (sin x)
                  #:impl (from-accelerators 'sinpoly17)
                  #:fpcore (! :precision binary64 (sinpoly17 x))
                  #:cost (* 0.375 8))

(define-operation (sinpoly19.f64 [x <binary64>])
                  <binary64>
                  #:spec (sin x)
                  #:impl (from-accelerators 'sinpoly19)
                  #:fpcore (! :precision binary64 (sinpoly19 x))
                  #:cost (* 0.375 9))

(define-operation (sinpoly21.f64 [x <binary64>])
                  <binary64>
                  #:spec (sin x)
                  #:impl (from-accelerators 'sinpoly21)
                  #:fpcore (! :precision binary64 (sinpoly21 x))
                  #:cost (* 0.375 10))

(define-operation (exppoly7.f64 [x <binary64>])
                  <binary64>
                  #:spec (exp x)
                  #:impl (from-accelerators 'exppoly7)
                  #:fpcore (! :precision binary64 (exppoly7 x))
                  #:cost (* 0.375 12))
