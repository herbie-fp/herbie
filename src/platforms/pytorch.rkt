#lang s-exp "../syntax/platform-language.rkt"

;; PyTorch platform

(require math/flonum)

(define 64bit-move-cost   1.0)
(define boolean-move-cost 1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost boolean-move-cost)

(define-operations () <bool>
    [TRUE  #:spec (TRUE)  #:impl (const true)   #:cost boolean-move-cost #:fpcore TRUE]
    [FALSE #:spec (FALSE) #:impl (const false)  #:cost boolean-move-cost #:fpcore FALSE])

(define-operations ([x <bool>] [y <bool>]) <bool>
    [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost boolean-move-cost]
    [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool>
    #:spec (not x) #:impl not #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 64bit-move-cost)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
    #:spec (if c t f)
    #:impl if-impl
    #:cost (if-cost boolean-move-cost))

(define-operations ([x <binary64>] [y <binary64>]) <bool>
    [eq.f64 #:spec (== x y) #:impl =           #:cost 26]
    [ne.f64 #:spec (!= x y) #:impl (negate =)  #:cost 26]
    [lt.f64 #:spec (< x y)  #:impl <           #:cost 26]
    [gt.f64 #:spec (> x y)  #:impl >           #:cost 26]
    [le.f64 #:spec (<= x y) #:impl <=          #:cost 26]
    [ge.f64 #:spec (>= x y) #:impl >=          #:cost 26])

(define-operations () <binary64> #:fpcore (! :precision binary64 _)
    [PI.f64 #:spec (PI)      #:impl (const pi)      #:fpcore PI      #:cost 64bit-move-cost]
    [E.f64  #:spec (E)       #:impl (const (exp 1)) #:fpcore E       #:cost 64bit-move-cost])

(define-operation (neg.f64 [x <binary64>]) <binary64>
    #:spec (neg x)
    #:impl -
    #:cost 14
    #:fpcore (! :precision binary64 (- x)))

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [add.f64          #:spec (+ x y)                        #:impl +                                  #:cost 23]
    [sub.f64          #:spec (- x y)                        #:impl -                                  #:cost 24]
    [mul.f64          #:spec (* x y)                        #:impl *                                  #:cost 19]
    [div.f64          #:spec (/ x y)                        #:impl /                                  #:cost 19]
    [true_divide.f64  #:spec (/ x y)                        #:impl /                                  #:cost 20]
    [pow.f64          #:spec (pow x y)                      #:impl (from-libm 'pow)                   #:cost 588]
    [floor_divide.f64 #:spec (floor (/ x y))                #:impl (lambda (x y) (floor (/ x y)))     #:cost 567  #:fpcore (floor_divide x y)]
    [fmax.f64         #:spec (fmax x y)                     #:impl (from-libm 'fmax)                  #:cost 25]
    [fmin.f64         #:spec (fmin x y)                     #:impl (from-libm 'fmin)                  #:cost 25]
    [hypot.f64        #:spec (sqrt (+ (* x x) (* y y)))     #:impl (from-libm 'hypot)                 #:cost 102  #:fpcore (hypot x y)]
    [logaddexp.f64    #:spec (log (+ (exp  x) (exp y)))     #:impl (from-rival)                       #:cost 549  #:fpcore (logaddexp x y)]
    [logaddexp2.f64   #:spec (log2 (+ (exp2 x) (exp2 y)))   #:impl (from-rival)                       #:cost 552  #:fpcore (logaddexp2 x y)]
    [atan2.f64        #:spec (atan (/ x y))                 #:impl (from-libm 'atan2)                 #:cost 1131 #:fpcore (atan2 x y)]
    [xlogy.f64        #:spec (if (== x 0) 0 (* x (log y)))  #:impl (from-rival)                       #:cost 178  #:fpcore (xlogy x y)]
    [fmod.f64         #:spec (fmod x y)                     #:impl (from-libm 'fmod)                  #:cost 357]
    [remainder.f64    #:spec (remainder x y)                #:impl (from-libm 'remainder)             #:cost 373])

(define-operations ([a <binary64>] [b <binary64>] [c <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [addcmul.f64 #:spec (+ a (* b c))        #:impl (lambda (a b c) (+ a (* b c)))        #:cost 30 #:fpcore (addcmul a b c)]
    [addcdiv.f64 #:spec (+ a (/ b c))        #:impl (lambda (a b c) (+ a (/ b c)))        #:cost 31 #:fpcore (addcdiv a b c)]
    [lerp.f64    #:spec (+ a (* c (- b a)))  #:impl (lambda (a b w) (+ a (* w (- b a))))  #:cost 61 #:fpcore (lerp a b c)])

(define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [positive.f64    #:spec x                                      #:impl (lambda (x) x)                            #:cost 2]
    [square.f64      #:spec (* x x)                                #:impl (lambda (x) (* x x))                      #:cost 15]
    [round.f64       #:spec (round x)                              #:impl (from-libm 'round)                        #:cost 16]
    [ceil.f64        #:spec (ceil x)                               #:impl (from-libm 'ceil)                         #:cost 16]
    [floor.f64       #:spec (floor x)                              #:impl (from-libm 'floor)                        #:cost 16]
    [trunc.f64       #:spec (trunc x)                              #:impl (from-libm 'trunc)                        #:cost 16  #:fpcore (trunc x)]
    [abs.f64         #:spec (fabs x)                               #:impl (from-libm 'fabs)                         #:cost 18]
    [frac.f64        #:spec (- x (trunc x))                        #:impl (lambda (x) (- x (fltruncate x)))         #:cost 19  #:fpcore (frac x)]
    [reciprocal.f64  #:spec (/ 1 x)                                #:impl (lambda (x) (/ 1.0 x))                    #:cost 20]
    [rad2deg.f64     #:spec (* x (/ 180 (PI)))                     #:impl (lambda (x) (* x (/ 180.0 pi)))           #:cost 21  #:fpcore (rad2deg x)]
    [deg2rad.f64     #:spec (* x (/ (PI) 180))                     #:impl (lambda (x) (* x (/ pi 180.0)))           #:cost 21  #:fpcore (deg2rad x)]
    [sqrt.f64        #:spec (sqrt x)                               #:impl (from-libm 'sqrt)                         #:cost 22]
    [rsqrt.f64       #:spec (/ 1 (sqrt x))                         #:impl (lambda (x) (/ 1.0 (sqrt x)))             #:cost 32  #:fpcore (rsqrt x)]
    [sign.f64
        #:spec (if (> x 0) 1 (if (< x 0) -1 0))
        #:impl (lambda (x)
                 (cond
                   [(fl> x 0.0) 1.0]
                   [(fl< x 0.0) -1.0]
                   [else 0.0]))
        #:cost 54
        #:fpcore (sign x)]
    [exp.f64         #:spec (exp x)                                #:impl (from-libm 'exp)                          #:cost 123]
    [expm1.f64       #:spec (- (exp x) 1)                          #:impl (from-libm 'expm1)                        #:cost 136  #:fpcore (expm1 x)]
    [sigmoid.f64     #:spec (/ 1 (+ 1 (exp (neg x))))              #:impl (lambda (x) (/ 1.0 (+ 1.0 (exp (- x)))))  #:cost 137  #:fpcore (sigmoid x)]
    [exp2.f64        #:spec (exp2 x)                               #:impl (from-libm 'exp2)                         #:cost 140]
    [log.f64         #:spec (log x)                                #:impl (from-libm 'log)                          #:cost 179]
    [cosh.f64        #:spec (cosh x)                               #:impl (from-libm 'cosh)                         #:cost 189]
    [log10.f64       #:spec (log10 x)                              #:impl (from-libm 'log10)                        #:cost 190]
    [log2.f64        #:spec (log2 x)                               #:impl (from-libm 'log2)                         #:cost 194]
    [sinh.f64        #:spec (sinh x)                               #:impl (from-libm 'sinh)                         #:cost 197]
    [acosh.f64       #:spec (acosh x)                              #:impl (from-libm 'acosh)                        #:cost 214]
    [tanh.f64        #:spec (tanh x)                               #:impl (from-libm 'tanh)                         #:cost 279]
    [log1p.f64       #:spec (log (+ 1 x))                          #:impl (from-libm 'log1p)                        #:cost 282  #:fpcore (log1p x)]
    [tan.f64         #:spec (tan x)                                #:impl (from-libm 'tan)                          #:cost 299]
    [atan.f64        #:spec (atan x)                               #:impl (from-libm 'atan)                         #:cost 314]
    [asinh.f64       #:spec (asinh x)                              #:impl (from-libm 'asinh)                        #:cost 341]
    [atanh.f64       #:spec (atanh x)                              #:impl (from-libm 'atanh)                        #:cost 381]
    [sin.f64         #:spec (sin x)                                #:impl (from-libm 'sin)                          #:cost 394]
    [cos.f64         #:spec (cos x)                                #:impl (from-libm 'cos)                          #:cost 394]
    [sinc.f64        #:spec (if (== x 0) 1 (/ (sin (* (PI) x)) (* (PI) x)))
                                     #:impl (lambda (x) (if (zero? x) 1.0 (/ (sin (* pi x)) (* pi x))))
                                     #:cost 408
                                     #:fpcore (sinc x)]
    [asin.f64        #:spec (asin x)                               #:impl (from-libm 'asin)                         #:cost 475]
    [lgamma.f64      #:spec (lgamma x)                             #:impl (from-libm 'lgamma)                       #:cost 541]
    [acos.f64        #:spec (acos x)                               #:impl (from-libm 'acos)                         #:cost 556])
