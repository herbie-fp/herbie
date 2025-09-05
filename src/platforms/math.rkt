#lang s-exp "../syntax/platform-language.rkt"

;;; C/C++ on Linux with reduced libm, meaning no special numeric
;;; functions. It is also 64-bit only.

(define move-cost    0.02333600000000001)
(define fl-move-cost (* move-cost 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost move-cost)

(define-operations () <bool>
  [TRUE  #:spec (TRUE)  #:impl (const true)  #:fpcore TRUE  #:cost move-cost]
  [FALSE #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost move-cost])

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost move-cost]
  [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost move-cost])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:impl not #:cost move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost fl-move-cost)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
  #:spec (if c t f) #:impl if-impl
  #:cost (if-cost move-cost))

(define-operations ([x <binary64>] [y <binary64>]) <bool>
  [==.f64 #:spec (== x y) #:impl =          #:cost fl-move-cost]
  [!=.f64 #:spec (!= x y) #:impl (negate =) #:cost fl-move-cost]
  [<.f64  #:spec (< x y)  #:impl <          #:cost fl-move-cost]
  [>.f64  #:spec (> x y)  #:impl >          #:cost fl-move-cost]
  [<=.f64 #:spec (<= x y) #:impl <=         #:cost fl-move-cost]
  [>=.f64 #:spec (>= x y) #:impl >=         #:cost fl-move-cost])

(define-operations () <binary64> #:fpcore (! :precision binary64 _)
  [PI.f64   #:spec (PI)       #:impl (const pi)      #:fpcore PI       #:cost fl-move-cost]
  [E.f64    #:spec (E)        #:impl (const (exp 1)) #:fpcore E        #:cost fl-move-cost]
  [INFINITY #:spec (INFINITY) #:impl (const +inf.0)  #:fpcore INFINITY #:cost fl-move-cost]
  [NAN.f64  #:spec (NAN)      #:impl (const +nan.0)  #:fpcore NAN      #:cost fl-move-cost])

(define-operation (neg.f64 [x <binary64>]) <binary64>
  #:spec (neg x) #:impl -
  #:fpcore (! :precision binary64 (- x)) #:cost 0.096592)

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
  [+.f64 #:spec (+ x y) #:impl + #:cost 0.164604]
  [-.f64 #:spec (- x y) #:impl - #:cost 0.15163999999999997]
  [*.f64 #:spec (* x y) #:impl * #:cost 0.20874800000000002]
  [/.f64 #:spec (/ x y) #:impl / #:cost 0.26615199999999994])

(define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
  [fabs.f64   #:spec (fabs x)   #:impl (from-libm 'fabs)      #:cost 0.10162]
  [sin.f64    #:spec (sin x)    #:impl (from-libm 'sin)       #:cost 3.318128]
  [cos.f64    #:spec (cos x)    #:impl (from-libm 'cos)       #:cost 3.32288]
  [tan.f64    #:spec (tan x)    #:impl (from-libm 'tan)       #:cost 3.710904]
  [sinh.f64   #:spec (sinh x)   #:impl (from-libm 'sinh)      #:cost 1.20954]
  [cosh.f64   #:spec (cosh x)   #:impl (from-libm 'cosh)      #:cost 0.953896]
  [acos.f64   #:spec (acos x)   #:impl (from-libm 'acos)      #:cost 0.357748]
  [acosh.f64  #:spec (acosh x)  #:impl (from-libm 'acosh)     #:cost 0.659472]
  [asin.f64   #:spec (asin x)   #:impl (from-libm 'asin)      #:cost 0.389788]
  [asinh.f64  #:spec (asinh x)  #:impl (from-libm 'asinh)     #:cost 0.835028]
  [atan.f64   #:spec (atan x)   #:impl (from-libm 'atan)      #:cost 0.83752]
  [atanh.f64  #:spec (atanh x)  #:impl (from-libm 'atanh)     #:cost 0.36238]
  [cbrt.f64   #:spec (cbrt x)   #:impl (from-libm 'cbrt)      #:cost 1.565176]
  [ceil.f64   #:spec (ceil x)   #:impl (from-libm 'ceil)      #:cost 0.47299]
  [erf.f64    #:spec (erf x)    #:impl (from-libm 'erf)       #:cost 0.806436]
  [exp.f64    #:spec (exp x)    #:impl (from-libm 'exp)       #:cost 1.0806]
  [exp2.f64   #:spec (exp2 x)   #:impl (from-libm 'exp2)      #:cost 0.825484]
  [floor.f64  #:spec (floor x)  #:impl (from-libm 'floor)     #:cost 0.468568]
  [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma)    #:cost 1.568012]
  [log.f64    #:spec (log x)    #:impl (from-libm 'log)       #:cost 0.505724]
  [log10.f64  #:spec (log10 x)  #:impl (from-libm 'log10)     #:cost 0.868856]
  [log2.f64   #:spec (log2 x)   #:impl (from-libm 'log2)      #:cost 0.681276]
  [logb.f64   #:spec (logb x)   #:impl (from-libm 'logb)      #:cost 0.220656]
  [rint.f64   #:spec (rint x)   #:impl (from-libm 'rint)      #:cost 0.121864]
  [round.f64  #:spec (round x)  #:impl (from-libm 'round)     #:cost 0.658564]
  [sqrt.f64   #:spec (sqrt x)   #:impl (from-libm 'sqrt)      #:cost 0.191872]
  [tanh.f64   #:spec (tanh x)   #:impl (from-libm 'tanh)      #:cost 0.824016]
  [tgamma.f64 #:spec (tgamma x) #:impl (from-libm 'tgamma)    #:cost 1.882576]
  [trunc.f64  #:spec (trunc x)  #:impl (from-libm 'trunc)     #:cost 0.463644])

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
  [pow.f64       #:spec (pow x y)       #:impl (from-libm 'pow)       #:cost 1.52482]
  [atan2.f64     #:spec (atan2 x y)     #:impl (from-libm 'atan2)     #:cost 1.492804]
  [copysign.f64  #:spec (copysign x y)  #:impl (from-libm 'copysign)  #:cost 0.200452]
  [fdim.f64      #:spec (fdim x y)      #:impl (from-libm 'fdim)      #:cost 0.592576]
  [fmax.f64      #:spec (fmax x y)      #:impl (from-libm 'fmax)      #:cost 0.3106]
  [fmin.f64      #:spec (fmin x y)      #:impl (from-libm 'fmin)      #:cost 0.289256]
  [fmod.f64      #:spec (fmod x y)      #:impl (from-libm 'fmod)      #:cost 94.277144]
  [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 16.165012])

(define-operation (erfc.f64 [x <binary64>]) <binary64>
  #:spec (- 1 (erf x)) #:impl (from-libm 'erfc)
  #:fpcore (! :precision binary64 (erfc x)) #:cost 0.816512)
