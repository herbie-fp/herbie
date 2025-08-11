#lang s-exp "../syntax/platform-language.rkt"

;; Julia platform

(require math/flonum)

(define 64bit-move-cost   1.000)
(define boolean-move-cost 0.100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost boolean-move-cost)

(define-operations () <bool>
    [TRUE   #:spec (TRUE)   #:impl (const true)   #:fpcore TRUE   #:cost boolean-move-cost]
    [FALSE  #:spec (FALSE)  #:impl (const false)  #:fpcore FALSE  #:cost boolean-move-cost])

(define-operations ([x <bool>] [y <bool>]) <bool>
    [and    #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost boolean-move-cost]
    [or     #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool>
    #:spec (not x) #:impl not #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 64bit-move-cost)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
    #:spec (if c t f) #:impl if-impl
    #:cost (if-cost boolean-move-cost))

(define-operations ([x <binary64>] [y <binary64>]) <bool>
    [==.f64  #:spec (== x y)  #:impl =          #:cost boolean-move-cost]
    [!=.f64  #:spec (!= x y)  #:impl (negate =) #:cost boolean-move-cost]
    [<.f64   #:spec (< x y)   #:impl <          #:cost boolean-move-cost]
    [>.f64   #:spec (> x y)   #:impl >          #:cost boolean-move-cost]
    [<=.f64  #:spec (<= x y)  #:impl <=         #:cost boolean-move-cost]
    [>=.f64  #:spec (>= x y)  #:impl >=         #:cost boolean-move-cost])

(define-operations () <binary64> #:fpcore (! :precision binary64 _)
    [PI.f64    #:spec (PI)        #:impl (const pi)        #:fpcore PI        #:cost 64bit-move-cost]
    [E.f64     #:spec (E)         #:impl (const (exp 1))   #:fpcore E         #:cost 64bit-move-cost]
    [INFINITY  #:spec (INFINITY)  #:impl (const +inf.0)    #:fpcore INFINITY  #:cost 64bit-move-cost]
    [NAN.f64   #:spec (NAN)       #:impl (const +nan.0)    #:fpcore NAN       #:cost 64bit-move-cost])

(define-operation (neg.f64 [x <binary64>]) <binary64>
    #:spec (neg x) #:impl - #:fpcore (! :precision binary64 (- x)) #:cost 1.0)

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [+.f64       #:spec (+ x y)        #:impl +                     #:cost 1.0]
    [-.f64       #:spec (- x y)        #:impl -                     #:cost 1.0]
    [*.f64       #:spec (* x y)        #:impl *                     #:cost 1.0]
    [/.f64       #:spec (/ x y)        #:impl /                     #:cost 1.0]
    [fmax.f64    #:spec (fmax x y)     #:impl (from-libm 'fmax)     #:cost 1.0]
    [fmin.f64    #:spec (fmin x y)     #:impl (from-libm 'fmin)     #:cost 1.0])

;; Unary operations
;; Align #:spec, #:impl, #:fpcore, #:cost columns; only lines with #:fpcore have that column.
(define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [fabs.f64    #:spec (fabs x)                           #:impl (from-libm 'fabs)   #:cost 1.0]
    [sqrt.f64    #:spec (sqrt x)                           #:impl (from-libm 'sqrt)   #:cost 1.0]
    [exp.f64     #:spec (exp x)                            #:impl (from-libm 'exp)    #:cost 3.5]
    [exp2.f64    #:spec (exp2 x)                           #:impl (from-libm 'exp2)   #:cost 3.5]
    [exp10.f64   #:spec (pow 10 x)                         #:impl (from-rival)        #:cost 3.5]
    [log.f64     #:spec (log x)                            #:impl (from-libm 'log)    #:cost 4.5]
    [log10.f64   #:spec (log10 x)                          #:impl (from-libm 'log10)  #:cost 5.0]
    [log2.f64    #:spec (log2 x)                           #:impl (from-libm 'log2)   #:cost 5.5]
    [sin.f64     #:spec (sin x)                            #:impl (from-libm 'sin)    #:cost 3.5]
    [cos.f64     #:spec (cos x)                            #:impl (from-libm 'cos)    #:cost 4.0]
    [tan.f64     #:spec (tan x)                            #:impl (from-libm 'tan)    #:cost 6.0]
    [sinh.f64    #:spec (sinh x)                           #:impl (from-libm 'sinh)   #:cost 4.5]
    [cosh.f64    #:spec (cosh x)                           #:impl (from-libm 'cosh)   #:cost 4.5]
    [tanh.f64    #:spec (tanh x)                           #:impl (from-libm 'tanh)   #:cost 5.0]
    [asin.f64    #:spec (asin x)                           #:impl (from-libm 'asin)   #:cost 4.0]
    [acos.f64    #:spec (acos x)                           #:impl (from-libm 'acos)   #:cost 4.0]
    [atan.f64    #:spec (atan x)                           #:impl (from-libm 'atan)   #:cost 6.0]
    [atanh.f64   #:spec (atanh x)                          #:impl (from-libm 'atanh)  #:cost 6.5]
    [asinh.f64   #:spec (asinh x)                          #:impl (from-libm 'asinh)  #:cost 9.5]
    [acosh.f64   #:spec (acosh x)                          #:impl (from-libm 'acosh)  #:cost 7.5]
    [cbrt.f64    #:spec (cbrt x)                           #:impl (from-libm 'cbrt)   #:cost 4.5]
    [log1p.f64   #:spec (log (+ 1 x))                      #:impl (from-libm 'log1p)  #:cost 5.5    #:fpcore (log1p x)]
    [expm1.f64   #:spec (- (exp x) 1)                      #:impl (from-libm 'expm1)  #:cost 5.5    #:fpcore (expm1 x)]
    [deg2rad.f64 #:spec (* x (/ (PI) 180))                 #:impl (from-rival)        #:cost 1.0    #:fpcore (deg2rad x)]
    [rad2deg.f64 #:spec (* x (/ 180 (PI)))                 #:impl (from-rival)        #:cost 1.0    #:fpcore (rad2deg x)]
    [abs2.f64    #:spec (* (fabs x) (fabs x))              #:impl (from-rival)        #:cost 1.0    #:fpcore (abs2 x)]
    [sec.f64     #:spec (/ 1 (cos x))                      #:impl (from-rival)        #:cost 4.0    #:fpcore (sec x)]
    [csc.f64     #:spec (/ 1 (sin x))                      #:impl (from-rival)        #:cost 3.5    #:fpcore (csc x)]
    [cot.f64     #:spec (/ 1 (tan x))                      #:impl (from-rival)        #:cost 7.5    #:fpcore (cot x)]
    [sech.f64    #:spec (/ 1 (cosh x))                     #:impl (from-rival)        #:cost 8.0    #:fpcore (sech x)]
    [csch.f64    #:spec (/ 1 (sinh x))                     #:impl (from-rival)        #:cost 5.0    #:fpcore (csch x)]
    [coth.f64    #:spec (/ (cosh x) (sinh x))              #:impl (from-rival)        #:cost 8.0    #:fpcore (coth x)]
    [asec.f64    #:spec (acos (/ 1 x))                     #:impl (from-rival)        #:cost 4.5    #:fpcore (asec x)]
    [acsc.f64    #:spec (asin (/ 1 x))                     #:impl (from-rival)        #:cost 6.5    #:fpcore (acsc x)]
    [acot.f64    #:spec (atan (/ 1 x))                     #:impl (from-rival)        #:cost 7.5    #:fpcore (acot x)]
    [asech.f64   #:spec (acosh (/ 1 x))                    #:impl (from-rival)        #:cost 8.5    #:fpcore (asech x)]
    [acsch.f64   #:spec (asinh (/ 1 x))                    #:impl (from-rival)        #:cost 10.0   #:fpcore (acsch x)]
    [acoth.f64   #:spec (atanh (/ 1 x))                    #:impl (from-rival)        #:cost 7.0    #:fpcore (acoth x)]
    [sind.f64    #:spec (sin (* x (/ (PI) 180)))           #:impl (from-rival)        #:cost 6.0    #:fpcore (sind x)]
    [cosd.f64    #:spec (cos (* x (/ (PI) 180)))           #:impl (from-rival)        #:cost 6.5    #:fpcore (cosd x)]
    [tand.f64    #:spec (tan (* x (/ (PI) 180)))           #:impl (from-rival)        #:cost 13.0   #:fpcore (tand x)]
    [cotd.f64    #:spec (/ 1 (tan (* x (/ (PI) 180))))     #:impl (from-rival)        #:cost 14.0   #:fpcore (cotd x)]
    [asind.f64   #:spec (* (asin x) (/ 180 (PI)))          #:impl (from-rival)        #:cost 4.5    #:fpcore (asind x)]
    [acosd.f64   #:spec (* (acos x) (/ 180 (PI)))          #:impl (from-rival)        #:cost 4.0    #:fpcore (acosd x)]
    [atand.f64   #:spec (* (atan x) (/ 180 (PI)))          #:impl (from-rival)        #:cost 6.0    #:fpcore (atand x)]
    [acotd.f64   #:spec (* (atan (/ 1 x)) (/ 180 (PI)))    #:impl (from-rival)        #:cost 8.0    #:fpcore (acotd x)]
    [asecd.f64   #:spec (* (acos (/ 1 x)) (/ 180 (PI)))    #:impl (from-rival)        #:cost 5.0    #:fpcore (asecd x)]
    [acscd.f64   #:spec (* (asin (/ 1 x)) (/ 180 (PI)))    #:impl (from-rival)        #:cost 6.5    #:fpcore (acscd x)]
    [secd.f64    #:spec (/ 1 (cos (* x (/ (PI) 180))))     #:impl (from-rival)        #:cost 8.0    #:fpcore (secd x)]
    [cscd.f64    #:spec (/ 1 (sin (* x (/ (PI) 180))))     #:impl (from-rival)        #:cost 6.5    #:fpcore (cscd x)]
    [sinpi.f64   #:spec (sin (* (* x (PI)) (/ (PI) 180)))  #:impl (from-rival)        #:cost 5.0    #:fpcore (sinpi x)]
    [cospi.f64   #:spec (cos (* (* x (PI)) (/ (PI) 180)))  #:impl (from-rival)        #:cost 5.0    #:fpcore (cospi x)])

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [hypot.f64     #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypot)    #:fpcore (hypot x y) #:cost 4.0]
    [pow.f64       #:spec (pow x y)                  #:impl (from-libm 'pow)                           #:cost 15.5]
    [copysign.f64  #:spec (copysign x y)             #:impl (from-libm 'copysign)                      #:cost 1.0])

(define-operation (fma.f64 [x <binary64>] [y <binary64>] [z <binary64>]) <binary64>
    #:spec (+ (* x y) z) #:impl (from-libm 'fma)
    #:fpcore (! :precision binary64 (fma x y z)) #:cost 1.0)
