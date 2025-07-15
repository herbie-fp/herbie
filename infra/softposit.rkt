#lang s-exp "../src/platform.rkt"

;;; Softposit platform, using David Thien's softposit-rkt package for
;;; bindings. Provides operations like real->posit16 or +.p16.

(require softposit-rkt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define posit8-max  (ordinal->posit8  (- (expt 2 (- 8 1)) 1)))
(define posit16-max (ordinal->posit16 (- (expt 2 (- 16 1)) 1)))
(define posit32-max (ordinal->posit32 (- (expt 2 (- 32 1)) 1)))

;; Max quire is difficult to compute:
;;
;; A quire is a 2s-complement signed fixed-point number with
;;  - `000...000` representing 0,
;;  - `100...000` representing NAR.
;; Unlike traditional fixed-point numbers, quires are actually symmetric.
;; For an `n`-bit posit, the associated quire has bitwidth `16 * n`
;; with a scale of `2^(16 - 8*n)`.
;;
;;   posit   quire size         max value               nearest double w/ epsilon
;;  -------|------------|----------------------|------------------------------------------|
;;   8       128         (2^(127) - 1) * 2^-48   2^79 - 2^-48   (6.044629098073146e+23)
;;   16      256         (2^(255) - 1) * 2^-112  2^143 - 2^-112  (1.1150372599265312e+43)
;;   32      512         (2^(511) - 1) * 2^-240  2^270 - 2^-240  (3.794275180128377e+81)
;;
;; Unfortunately, we don't have a good way to convert doubles to quire.
;; The libsoftposit library only has double to posit; the Racket shim
;; incorrectly composes double-posit, posit-quire conversions.
;;

(define quire8-max   (quire8-fdp-add  (double->quire8 0.0)  posit8-max  posit8-max))
(define quire8-nmax  (quire8-fdp-sub  (double->quire8 0.0)  posit8-max  posit8-max))
(define quire16-max  (quire16-fdp-add (double->quire16 0.0) posit16-max posit16-max))
(define quire16-nmax (quire16-fdp-sub (double->quire16 0.0) posit16-max posit16-max))

; These crash
; (define quire32-max (quire32-fdp-add (double->quire32 0.0) posit32-max posit32-max))
; (define quire32-nmax (quire32-fdp-sub (double->quire32 0.0) posit32-max posit32-max))

(define (bf-inf->nan x) (let ([y (bf x)]) (if (bfinfinite? y) +nan.bf y)))

(define (double->posit8* x)
  (let ([y (double->posit8 x)])
    (if (posit8= y (posit8-nar))
        (if (> x 0) posit8-max (posit8-neg posit8-max))
        y)))
(define (double->posit16* x)
  (let ([y (double->posit16 x)])
    (if (posit16= y (posit16-nar))
        (if (> x 0) posit16-max (posit16-neg posit16-max))
        y)))
(define (double->posit32* x)
  (let ([y (double->posit32 x)])
    (if (posit32= y (posit32-nar))
        (if (> x 0) posit32-max (posit32-neg posit32-max))
        y)))
(define (double->quire8* x)
  (let ([y (double->quire8 x)])
    (if (infinite? (quire8->double y))
        (if (> x 0) quire8-max quire8-nmax)
        y)))
(define (double->quire16* x)
  (let ([y (double->quire16 x)])
    (if (infinite? (quire16->double y))
        (if (> x 0) quire16-max quire16-nmax)
        y)))
#;(define (double->quire32* x)
  (let ([y (double->quire32 x)])
    (if (infinite? (quire32->double y))
        (if (> x 0) quire32-max quire32-nmax)
        y)))

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define-if #:cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPRESENTATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(define <posit8>
  (make-representation #:name 'posit8
                       #:bf->repr (compose double->posit8* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan posit8->double)
                       #:ordinal->repr (shift 7 ordinal->posit8)
                       #:repr->ordinal (unshift 7 posit8->ordinal)
                       #:total-bits 8
                       #:special-value? (curry posit8= (posit8-nar))))

(define <posit16>
  (make-representation #:name 'posit16
                       #:bf->repr (compose double->posit16* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan posit16->double)
                       #:ordinal->repr (shift 15 ordinal->posit16)
                       #:repr->ordinal (unshift 15 posit16->ordinal)
                       #:total-bits 16
                       #:special-value? (curry posit16= (posit16-nar))))

(define <posit32>
  (make-representation #:name 'posit32
                       #:bf->repr (compose double->posit32* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan posit32->double)
                       #:ordinal->repr (shift 31 ordinal->posit32)
                       #:repr->ordinal (unshift 31 posit32->ordinal)
                       #:total-bits 32
                       #:special-value? (curry posit32= (posit32-nar))))

(define <quire8>
  (make-representation #:name 'quire8
                       #:bf->repr (compose double->quire8* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan quire8->double)
                       #:ordinal->repr (compose double->quire8 ordinal->flonum)
                       #:repr->ordinal (compose flonum->ordinal quire8->double)
                       #:total-bits 64
                       #:special-value? (const #f)))

(define <quire16>
  (make-representation #:name 'quire16
                       #:bf->repr (compose double->quire16* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan quire16->double)
                       #:ordinal->repr (compose double->quire16 ordinal->flonum)
                       #:repr->ordinal (compose flonum->ordinal quire16->double)
                       #:total-bits 64
                       #:special-value? (const #f)))

(define <quire32>
  (make-representation #:name 'quire32
                       #:bf->repr (compose double->quire32 bigfloat->flonum) ; TODO: use double->quire32* when crash fixed
                       #:repr->bf (compose bf-inf->nan quire32->double)
                       #:ordinal->repr (compose double->quire32 ordinal->flonum)
                       #:repr->ordinal (compose flonum->ordinal quire32->double)
                       #:total-bits 64
                       #:special-value? (const #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN IMPLS ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost boolean-move-cost)

(define-operations () <bool>
  [TRUE  #:spec (TRUE)  #:impl (const true)  #:fpcore TRUE  #:cost boolean-move-cost]
  [FALSE #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost boolean-move-cost])

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost boolean-move-cost]
  [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:impl not #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POSIT IMPLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <posit8>  #:cost cost)
(define-representation <posit16> #:cost cost)
(define-representation <posit32> #:cost cost)

(define-operations ([x <posit8>] [y <posit8>]) <bool>
  [==.p8 #:spec (== x y) #:impl posit8=  #:cost 1]
  [<.p8  #:spec (< x y)  #:impl posit8<  #:cost 1]
  [>.p8  #:spec (> x y)  #:impl posit8>  #:cost 1]
  [<=.p8 #:spec (<= x y) #:impl posit8<= #:cost 1]
  [>=.p8 #:spec (>= x y) #:impl posit8>= #:cost 1])

(parameterize ([fpcore-context '(:precision posit8)])
  (define-operations ([x <posit8>]) <posit8>
    [neg.p8  #:spec (neg x)  #:impl posit8-neg  #:cost 1]
    [sqrt.p8 #:spec (sqrt x) #:impl posit8-sqrt #:cost 1])

  (define-operations ([x <posit8>] [y <posit8>]) <posit8>
    [+.p8 #:spec (+ x y) #:impl posit8-add #:cost 1]
    [-.p8 #:spec (- x y) #:impl posit8-sub #:cost 1]
    [*.p8 #:spec (* x y) #:impl posit8-mul #:cost 1]
    [/.p8 #:spec (/ x y) #:impl posit8-div #:cost 1]))

(define-operations ([x <posit16>] [y <posit16>]) <bool>
  [==.p16 #:spec (== x y) #:impl posit16=  #:cost 1]
  [<.p16  #:spec (< x y)  #:impl posit16<  #:cost 1]
  [>.p16  #:spec (> x y)  #:impl posit16>  #:cost 1]
  [<=.p16 #:spec (<= x y) #:impl posit16<= #:cost 1]
  [>=.p16 #:spec (>= x y) #:impl posit16>= #:cost 1])

(parameterize ([fpcore-context '(:precision posit16)])
  (define-operations ([x <posit16>]) <posit16>
    [neg.p16  #:spec (neg x)  #:impl posit16-neg  #:cost 1]
    [sqrt.p16 #:spec (sqrt x) #:impl posit16-sqrt #:cost 1])

  (define-operations ([x <posit16>] [y <posit16>]) <posit16>
    [+.p16 #:spec (+ x y) #:impl posit16-add #:cost 1]
    [-.p16 #:spec (- x y) #:impl posit16-sub #:cost 1]
    [*.p16 #:spec (* x y) #:impl posit16-mul #:cost 1]
    [/.p16 #:spec (/ x y) #:impl posit16-div #:cost 1]))

(define-operations ([x <posit32>] [y <posit32>]) <bool>
  [==.p32 #:spec (== x y) #:impl posit32=  #:cost 1]
  [<.p32  #:spec (< x y)  #:impl posit32<  #:cost 1]
  [>.p32  #:spec (> x y)  #:impl posit32>  #:cost 1]
  [<=.p32 #:spec (<= x y) #:impl posit32<= #:cost 1]
  [>=.p32 #:spec (>= x y) #:impl posit32>= #:cost 1])

(parameterize ([fpcore-context '(:precision posit32)])
  (define-operations ([x <posit32>]) <posit32>
    [neg.p32  #:spec (neg x)  #:impl posit32-neg  #:cost 1]
    [sqrt.p32 #:spec (sqrt x) #:impl posit32-sqrt #:cost 1])

  (define-operations ([x <posit32>] [y <posit32>]) <posit32>
    [+.p32 #:spec (+ x y) #:impl posit32-add #:cost 1]
    [-.p32 #:spec (- x y) #:impl posit32-sub #:cost 1]
    [*.p32 #:spec (* x y) #:impl posit32-mul #:cost 1]
    [/.p32 #:spec (/ x y) #:impl posit32-div #:cost 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QUIRE OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <quire8>  #:cost cost)
(define-representation <quire16> #:cost cost)
(define-representation <quire32> #:cost cost)

(parameterize ([fpcore-context '(:precision quire8)])
  (define-operations ([x <quire8>] [y <posit8>] [z <posit8>]) <quire8>
    [quire8-mul-add #:spec (+ x (* y z)) #:impl quire8-fdp-add #:fpcore (fma y z x)     #:cost 1]
    [quire8-mul-sub #:spec (- x (* y z)) #:impl quire8-fdp-sub #:fpcore (fma (- y) z x) #:cost 1]))

(parameterize ([fpcore-context '(:precision quire16)])
  (define-operations ([x <quire16>] [y <posit16>] [z <posit16>]) <quire16>
    [quire16-mul-add #:spec (+ x (* y z)) #:impl quire16-fdp-add #:fpcore (fma y z x)     #:cost 1]
    [quire16-mul-sub #:spec (- x (* y z)) #:impl quire16-fdp-sub #:fpcore (fma (- y) z x) #:cost 1]))

(parameterize ([fpcore-context '(:precision quire32)])
  (define-operations ([x <quire32>] [y <posit32>] [z <posit32>]) <quire32>
    [quire32-mul-add #:spec (+ x (* y z)) #:impl quire32-fdp-add #:fpcore (fma y z x)     #:cost 1]
    [quire32-mul-sub #:spec (- x (* y z)) #:impl quire32-fdp-sub #:fpcore (fma (- y) z x) #:cost 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONVERTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost cost)

(define-operation (binary64->posit8  [x <binary64>]) <posit8>
  #:spec x #:impl double->posit8   #:fpcore (! :precision posit8 (cast x))   #:cost cost]
(define-operation (binary64->posit16 [x <binary64>]) <posit16>
  #:spec x #:impl double->posit16  #:fpcore (! :precision posit16 (cast x))  #:cost cost]
(define-operation (binary64->posit32 [x <binary64>]) <posit32>
  #:spec x #:impl double->posit32  #:fpcore (! :precision posit32 (cast x))  #:cost cost]
(define-operation (posit8->binary64  [x <posit8>])   <binary64>
  #:spec x #:impl posit8->double   #:fpcore (! :precision binary64 (cast x)) #:cost cost]
(define-operation (posit16->binary64 [x <posit16>])  <binary64>
  #:spec x #:impl posit16->double  #:fpcore (! :precision binary64 (cast x)) #:cost cost]
(define-operation (posit32->binary64 [x <posit32>])  <binary64>
  #:spec x #:impl posit32->double  #:fpcore (! :precision binary64 (cast x)) #:cost cost]

(define-operation (binary64->quire8  [x <binary64>]) <quire8>
  #:spec x #:impl double->quire8   #:fpcore (! :precision quire8 (cast x))   #:cost cost]
(define-operation (binary64->quire16 [x <binary64>]) <quire16>
  #:spec x #:impl double->quire16  #:fpcore (! :precision quire16 (cast x))  #:cost cost]
(define-operation (binary64->quire32 [x <binary64>]) <quire32>
  #:spec x #:impl double->quire32  #:fpcore (! :precision quire32 (cast x))  #:cost cost]
(define-operation (quire8->binary64  [x <quire8>])   <binary64>
  #:spec x #:impl quire8->double   #:fpcore (! :precision binary64 (cast x)) #:cost cost]
(define-operation (quire16->binary64 [x <quire16>])  <binary64>
  #:spec x #:impl quire16->double  #:fpcore (! :precision binary64 (cast x)) #:cost cost]
(define-operation (quire32->binary64 [x <quire32>])  <binary64>
  #:spec x #:impl quire32->double  #:fpcore (! :precision binary64 (cast x)) #:cost cost]

(define-operation (quire8->posit8    [x <quire8>])   <posit8>
  #:spec x #:impl quire8->posit8   #:fpcore (! :precision posit8 (cast x))   #:cost cost]
(define-operation (quire16->posit16  [x <quire16>])  <posit16>
  #:spec x #:impl quire16->posit16 #:fpcore (! :precision posit16 (cast x))  #:cost cost]
(define-operation (quire32->posit32  [x <quire32>])  <posit32>
  #:spec x #:impl quire32->posit32 #:fpcore (! :precision posit32 (cast x))  #:cost cost]
(define-operation (posit8->quire8    [x <posit8>])   <quire8>
  #:spec x #:impl posit8->quire8   #:fpcore (! :precision quire8 (cast x))   #:cost cost]
(define-operation (posit16->quire16  [x <posit16>])  <quire16>
  #:spec x #:impl posit16->quire16 #:fpcore (! :precision quire16 (cast x))  #:cost cost]
(define-operation (posit32->quire32  [x <posit32>])  <quire32>
  #:spec x #:impl posit32->quire32 #:fpcore (! :precision quire32 (cast x))  #:cost cost]))
