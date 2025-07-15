#lang s-exp "../platform.rkt"

;;; Racket platform, focusing on racket/base and math/base.
;;; Therefore only one data type, <binary64>, is supported.

(require math/flonum
         math/base)

(define-if #:cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost 1)

(define-operations () <bool>
  [true  #:spec (TRUE)  #:impl (const true)  #:fpcore TRUE  #:cost 1]
  [false #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost 1])

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost 1]
  [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost 1])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:impl not #:cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 1)

(define-operations () <binary64>
  [PI.rkt       #:spec (PI)       #:impl (const pi)       #:fpcore PI       #:cost 1]
  [INFINITY.rkt #:spec (INFINITY) #:impl (const +inf.0)   #:fpcore INFINITY #:cost 1]
  [NAN.rkt      #:spec (NAN)      #:impl (const +nan.0)   #:fpcore NAN      #:cost 1])

(define-operations ([x <binary64>] [y <binary64>]) <bool>
  [=  #:spec (== x y) #:impl =  #:cost 1]
  [<  #:spec (< x y)  #:impl <  #:cost 1]
  [>  #:spec (> x y)  #:impl >  #:cost 1]
  [<= #:spec (<= x y) #:impl <= #:cost 1]
  [>= #:spec (>= x y) #:impl >= #:cost 1])

(define ((no-complex fun) . xs)
  (define res (apply fun xs))
  (if (real? res) res +nan.0))

(define-operation (-/1 [x <binary64>]) <binary64>
  #:spec (neg x) #:impl - #:fpcore (- x) #:cost 1)

(define-operations ([x <binary64>]) <binary64>
 [acos      #:spec (acos  x) #:impl (no-complex acos)  #:cost 1]
 [acosh     #:spec (acosh x) #:impl (no-complex acosh) #:cost 1]
 [asin      #:spec (asin  x) #:impl (no-complex asin)  #:cost 1]
 [asinh     #:spec (asinh x) #:impl (no-complex asinh) #:cost 1]
 [atan      #:spec (atan  x) #:impl (no-complex atan)  #:cost 1]
 [atanh     #:spec (atanh x) #:impl (no-complex atanh) #:cost 1]
 [ceiling   #:spec (ceil  x) #:impl ceiling            #:cost 1]
 [cos       #:spec (cos   x) #:impl cos                #:cost 1]
 [cosh      #:spec (cosh  x) #:impl cosh               #:cost 1]
 [exp       #:spec (exp   x) #:impl exp                #:cost 1]
 [abs       #:spec (fabs  x) #:impl abs                #:cost 1]
 [floor     #:spec (floor x) #:impl floor              #:cost 1]
 [log       #:spec (log   x) #:impl (no-complex log)   #:cost 1]
 [round     #:spec (round x) #:impl round              #:cost 1]
 [sin       #:spec (sin   x) #:impl sin                #:cost 1]
 [sinh      #:spec (sinh  x) #:impl sinh               #:cost 1]
 [sqrt      #:spec (sqrt  x) #:impl (no-complex sqrt)  #:cost 1]
 [tan       #:spec (tan   x) #:impl tan                #:cost 1]
 [tanh      #:spec (tanh  x) #:impl tanh               #:cost 1]
 [truncate  #:spec (trunc x) #:impl truncate           #:cost 1])

(define-operations ([x <binary64>] [y <binary64>]) <binary64>
 [+         #:spec (+ x y)         #:impl +                 #:cost 1]
 [-         #:spec (- x y)         #:impl -                 #:cost 1]
 [*         #:spec (* x y)         #:impl *                 #:cost 1]
 [/         #:spec (/ x y)         #:impl /                 #:cost 1]
 [atan/2    #:spec (atan2 x y)     #:impl (no-complex atan) #:cost 1]
 [max       #:spec (fmax x y)      #:impl max               #:cost 1]
 [min       #:spec (fmin x y)      #:impl min               #:cost 1]
 [expt      #:spec (pow x y)       #:impl (no-complex expt) #:cost 1]
 [remainder #:spec (remainder x y) #:impl remainder         #:cost 1])

(define-operation (//1 [x <binary64>]) <binary64>
  #:spec (/ 1 x) #:impl / #:cost 1)

(define-operation (log/2 [x <binary64>] [y <binary64>]) <binary64>
  #:spec (/ (log x) (log y)) #:impl log #:cost 1)

(define-operation (flsingle [x <binary64>]) <binary64>
  #:spec x #:impl flsingle #:cost 1)
