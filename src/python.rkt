#lang herbie/platform

(require math/flonum)

(define 64bit-move-cost   0.125)
(define boolean-move-cost 0.100)

(define-representation <bool> #:cost boolean-move-cost)

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost boolean-move-cost]
  [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:impl not #:cost boolean-move-cost)

(define-representation <binary64> #:cost 64bit-move-cost)

(define-operations ([x <binary64>] [y <binary64>]) <bool>
  [== #:spec (== x y) #:impl =          #:cost 64bit-move-cost]
  [!= #:spec (!= x y) #:impl (negate =) #:cost 64bit-move-cost]
  [<  #:spec (< x y)  #:impl <          #:cost 64bit-move-cost]
  [>  #:spec (> x y)  #:impl >          #:cost 64bit-move-cost]
  [<= #:spec (<= x y) #:impl <=         #:cost 64bit-move-cost]
  [>= #:spec (>= x y) #:impl >=         #:cost 64bit-move-cost])

(define-operations () <binary64> #:fpcore (:precision binary64)
  [pi    #:spec (PI)   #:impl (const (flsingle pi))       #:fpcore PI       #:cost 64bit-move-cost]
  [e     #:spec (E)    #:impl (const (flsingle (exp 1)))  #:fpcore E        #:cost 64bit-move-cost]
  //[inf   #:spec (+inf.0)  #:impl (const +inf.0)           #:fpcore INFINITY #:cost 64bit-move-cost]
  //[nan   #:spec (+nan.0)  #:impl (const +nan.0)           #:fpcore NAN      #:cost 64bit-move-cost]
  [tau   #:spec (* 2 PI)  #:impl (const(* 2 pi))          #:fpcore (* 2 PI) #:cost 64bit-move-cost]
)

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (:precision binary64)
  [+ #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.200]
  [- #:spec (- x y) #:impl (compose flsingle -) #:cost 0.200]
  [* #:spec (* x y) #:impl (compose flsingle *) #:cost 0.250]
  [/ #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.350])

(define-operations ([x <binary64>]) <binary64> #:fpcore (:precision binary64)
    [degrees  #:spec (degrees x) #:impl (lambda (x)(* x (/ pi 180.0))) #:cost 1]
    [radians  #:spec (radians x) #:impl (lambda (x)(* x (/ 180.0 pi))) #:cost 1]
    [neg      #:spec (- x)      #:impl -                      #:cost 0.125]
    [fabs     #:spec (fabs x)   #:impl (from-libm 'fabs)      #:cost 1]
    [sin      #:spec (sin x)    #:impl (from-libm 'sin)       #:cost 1]
    [sinh     #:spec (sinh x)   #:impl (from-libm 'sinh)      #:cost 1]
    [asin     #:spec (asin x)   #:impl (from-libm 'asin)      #:cost 1]
    [asinh    #:spec (asinh x)  #:impl (from-libm 'asinh)     #:cost 1]
    [cos      #:spec (cos x)    #:impl (from-libm 'cos)       #:cost 1]
    [cosh     #:spec (cosh x)   #:impl (from-libm 'cosh)      #:cost 1]
    [acos     #:spec (acos x)   #:impl (from-libm 'acos)      #:cost 1]
    [acosh    #:spec (acosh x)  #:impl (from-libm 'acosh)     #:cost 1]
    [tan      #:spec (tan x)    #:impl (from-libm 'tan)       #:cost 1]
    [atan     #:spec (atan x)   #:impl (from-libm 'atan)      #:cost 1]
    [atanh    #:spec (atanh x)  #:impl (from-libm 'atanh)     #:cost 1]
    [tanh     #:spec (tanh x)   #:impl (from-libm 'tanh)      #:cost 1]
    [sqrt     #:spec (sqrt x)   #:impl (from-libm 'sqrt)      #:cost 1]
    [cbrt     #:spec (cbrt x)   #:impl (from-libm 'cbrt)      #:cost 1]
    [ceil     #:spec (ceil x)   #:impl (from-libm 'ceil)      #:cost 1]
    [floor    #:spec (floor x)  #:impl (from-libm 'floor)     #:cost 1]
    [erf      #:spec (erf x)    #:impl (from-libm 'erf)       #:cost 1]
    [exp      #:spec (exp x)    #:impl (from-libm 'exp)       #:cost 1]
    [exp2     #:spec (exp2 x)   #:impl (from-libm 'exp2)      #:cost 1]
    [gamma    #:spec (gamma x)  #:impl (from-libm 'tgamma)    #:cost 1]
    [lgamma   #:spec (lgamma x) #:impl (from-libm 'lgamma)    #:cost 1]
    [log.1var #:spec (log x)    #:impl (from-libm 'log)       #:cost 1]
    [log10    #:spec (log10 x)  #:impl (from-libm 'log10)     #:cost 1]
    [log2     #:spec (log2 x)   #:impl (from-libm 'log2)      #:cost 1]
    [rint     #:spec (rint x)   #:impl (from-libm 'rint)      #:cost 1]
    [round    #:spec (round x)  #:impl (from-libm 'round)     #:cost 1]
    [trunc    #:spec (trunc x)  #:impl (from-libm 'trunc)     #:cost 1]
    [modf     #:spec (modf x)   #:impl (from-libm 'modf)      #:cost 1]
    [frexp    #:spec (frexp x)  #:impl (from-libm 'frexp)     #:cost 1]
)

(define-operations ([x <binary64>]) <binary64> #:fpcore (:precision binary64)
  [erfc.f64  #:spec (- 1 (erf x)) #:impl (from-libm 'erfc)  #:fpcore (erfc x) #:cost 0.900]
  [expm1.f64 #:spec (- (exp x) 1) #:impl (from-libm 'expm1) #:fpcore (expm1 x) #:cost 0.900]
  [log1p.f64 #:spec (log (+ 1 x)) #:impl (from-libm 'log1p) #:fpcore (log1p x) #:cost 1.300])

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (:precision binary64)
    [log.2var  #:spec (log x y)       #:impl (from-libm 'log)       #:cost 1]
    [pow       #:spec (pow x y)       #:impl (from-libm 'pow)       #:cost 1]
    [atan2     #:spec (atan2 x y)     #:impl (from-libm 'atan2)     #:cost 1]
    [copysign  #:spec (copysign x y)  #:impl (from-libm 'copysign)  #:cost 1]
    [fdim      #:spec (fdim x y)      #:impl (from-libm 'fdim)      #:cost 1]
    [fmax      #:spec (fmax x y)      #:impl (from-libm 'fmax)      #:cost 1]
    [fmin      #:spec (fmin x y)      #:impl (from-libm 'fmin)      #:cost 1]
    [fmod      #:spec (fmod x y)      #:impl (from-libm 'fmod)      #:cost 1]
    [remainder #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 1]
    [ldexp     #:spec (ldexp x y)     #:impl (from-libm 'ldexp)     #:cost 1]
)

(define-operation (hypot [x <binary64>] [y <binary64>]) <binary64>
  #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypot)
  #:fpcore (! :precision binary64 (hypot x y)) #:cost 1)

(define-operation (fma [x <binary64>] [y <binary64>] [z <binary64>]) <binary64>
  #:spec (+ (* x y) z) #:impl (from-libm 'fma)
  #:fpcore (! :precision binary64 (fma x y z)) #:cost 1)

(define-operations ([x <binary64>]) <bool> #:fpcore (:precision binary64)
    [isfinite #:spec (isfinite x) #:impl (from-libm 'isfinite) #:cost 1]
    [isinf    #:spec (isinf x)    #:impl (from-libm 'isinf)    #:cost 1]
    [isnan    #:spec (isnan x)    #:impl (from-libm 'isnan)    #:cost 1]
)