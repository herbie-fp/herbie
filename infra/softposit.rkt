#lang racket

;;; Softposit platform:
;;; Enable functions like real->posit16 or +.p16 in Herbie through David Thien's package

(require math/flonum
         math/bigfloat
         softposit-rkt
         "../src/syntax/types.rkt"  ; for shift/unshift
         "../src/syntax/platform.rkt")
(provide platform)

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

(define cost 1)

(define platform (make-empty-platform 'softposit))

(platform-register-if-cost! platform #:cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPRESENTATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(define bool <bool>)

(define posit8
  (make-representation #:name 'posit8
                       #:bf->repr (compose double->posit8* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan posit8->double)
                       #:ordinal->repr (shift 7 ordinal->posit8)
                       #:repr->ordinal (unshift 7 posit8->ordinal)
                       #:total-bits 8
                       #:special-value? (curry posit8= (posit8-nar))))

(define posit16
  (make-representation #:name 'posit16
                       #:bf->repr (compose double->posit16* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan posit16->double)
                       #:ordinal->repr (shift 15 ordinal->posit16)
                       #:repr->ordinal (unshift 15 posit16->ordinal)
                       #:total-bits 16
                       #:special-value? (curry posit16= (posit16-nar))))

(define posit32
  (make-representation #:name 'posit32
                       #:bf->repr (compose double->posit32* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan posit32->double)
                       #:ordinal->repr (shift 31 ordinal->posit32)
                       #:repr->ordinal (unshift 31 posit32->ordinal)
                       #:total-bits 32
                       #:special-value? (curry posit32= (posit32-nar))))

(define quire8
  (make-representation #:name 'quire8
                       #:bf->repr (compose double->quire8* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan quire8->double)
                       #:ordinal->repr (compose double->quire8 ordinal->flonum)
                       #:repr->ordinal (compose flonum->ordinal quire8->double)
                       #:total-bits 64
                       #:special-value? (const #f)))

(define quire16
  (make-representation #:name 'quire16
                       #:bf->repr (compose double->quire16* bigfloat->flonum)
                       #:repr->bf (compose bf-inf->nan quire16->double)
                       #:ordinal->repr (compose double->quire16 ordinal->flonum)
                       #:repr->ordinal (compose flonum->ordinal quire16->double)
                       #:total-bits 64
                       #:special-value? (const #f)))

(define quire32
  (make-representation #:name 'quire32
                       #:bf->repr (compose double->quire32 bigfloat->flonum) ; TODO: use double->quire32* when crash fixed
                       #:repr->bf (compose bf-inf->nan quire32->double)
                       #:ordinal->repr (compose double->quire32 ordinal->flonum)
                       #:repr->ordinal (compose flonum->ordinal quire32->double)
                       #:total-bits 64
                       #:special-value? (const #f)))

; Binary64 is needed for casting from double to softposit formats
(define binary64 <binary64>)

(platform-register-representation! platform #:repr bool     #:cost cost)
(platform-register-representation! platform #:repr posit8   #:cost cost)
(platform-register-representation! platform #:repr posit16  #:cost cost)
(platform-register-representation! platform #:repr posit32  #:cost cost)
(platform-register-representation! platform #:repr quire8   #:cost cost)
(platform-register-representation! platform #:repr quire16  #:cost cost)
(platform-register-representation! platform #:repr quire32  #:cost cost)
(platform-register-representation! platform #:repr binary64 #:cost cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OPERATORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN IMPLS ;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([TRUE  ()                      bool (TRUE)    (const true)  (! TRUE)  cost]
  [FALSE ()                      bool (FALSE)   (const false) (! FALSE) cost]
  [not   ([x : bool])            bool (not x)   not           (not x)   cost]
  [and   ([x : bool] [y : bool]) bool (and x y) and-fn        (and x y) cost]
  [or    ([x : bool] [y : bool]) bool (or x y)  or-fn         (or x y)  cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POSIT IMPLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name           ([var : repr] ...)                          otype   spec          fl                fpcore                           cost])
(platform-register-implementations!
 platform
 (; Posit8
  [neg.p8          ([x : posit8])                              posit8  (neg x)       posit8-neg        (! :precision posit8 (- x))        cost]
  [+.p8            ([x : posit8]  [y : posit8])                posit8  (+ x y)       posit8-add        (! :precision posit8 (+ x y))      cost]
  [-.p8            ([x : posit8]  [y : posit8])                posit8  (- x y)       posit8-sub        (! :precision posit8 (- x y))      cost]
  [*.p8            ([x : posit8]  [y : posit8])                posit8  (* x y)       posit8-mul        (! :precision posit8 (* x y))      cost]
  [/.p8            ([x : posit8]  [y : posit8])                posit8  (/ x y)       posit8-div        (! :precision posit8 (/ x y))      cost]
  [sqrt.p8         ([x : posit8])                              posit8  (sqrt x)      posit8-sqrt       (! :precision posit8 (sqrt x))     cost]
  [==.p8           ([x : posit8]  [y : posit8])                bool    (== x y)      posit8=           (== x y)                           cost]
  [!=.p8           ([x : posit8]  [y : posit8])                bool    (!= x y)      (negate posit8=)  (!= x y)                           cost]
  [<.p8            ([x : posit8]  [y : posit8])                bool    (< x y)       posit8<           (< x y)                            cost]
  [>.p8            ([x : posit8]  [y : posit8])                bool    (> x y)       posit8>           (> x y)                            cost]
  [<=.p8           ([x : posit8]  [y : posit8])                bool    (<= x y)      posit8<=          (<= x y)                           cost]
  [>=.p8           ([x : posit8]  [y : posit8])                bool    (>= x y)      posit8>=          (>= x y)                           cost]
  ; Posit 16
  [neg.p16         ([x : posit16])                             posit16 (neg x)       posit16-neg       (! :precision posit16 (- x))       cost]
  [+.p16           ([x : posit16] [y : posit16])               posit16 (+ x y)       posit16-add       (! :precision posit16 (+ x y))     cost]
  [-.p16           ([x : posit16] [y : posit16])               posit16 (- x y)       posit16-sub       (! :precision posit16 (- x y))     cost]
  [*.p16           ([x : posit16] [y : posit16])               posit16 (* x y)       posit16-mul       (! :precision posit16 (* x y))     cost]
  [/.p16           ([x : posit16] [y : posit16])               posit16 (/ x y)       posit16-div       (! :precision posit16 (/ x y))     cost]
  [sqrt.p16        ([x : posit16])                             posit16 (sqrt x)      posit16-sqrt      (! :precision posit16 (sqrt x))    cost]
  [==.p16          ([x : posit16] [y : posit16])               bool    (== x y)      posit16=          (== x y)                           cost]
  [!=.p16          ([x : posit16] [y : posit16])               bool    (!= x y)      (negate posit16=) (!= x y)                           cost]
  [<.p16           ([x : posit16] [y : posit16])               bool    (< x y)       posit16<          (< x y)                            cost]
  [>.p16           ([x : posit16] [y : posit16])               bool    (> x y)       posit16>          (> x y)                            cost]
  [<=.p16          ([x : posit16] [y : posit16])               bool    (<= x y)      posit16<=         (<= x y)                           cost]
  [>=.p16          ([x : posit16] [y : posit16])               bool    (>= x y)      posit16>=         (>= x y)                           cost]
  ; Posit 32
  [neg.p32         ([x : posit32])                             posit32 (neg x)       posit32-neg       (! :precision posit32 (- x))       cost]
  [+.p32           ([x : posit32] [y : posit32])               posit32 (+ x y)       posit32-add       (! :precision posit32 (+ x y))     cost]
  [-.p32           ([x : posit32] [y : posit32])               posit32 (- x y)       posit32-sub       (! :precision posit32 (- x y))     cost]
  [*.p32           ([x : posit32] [y : posit32])               posit32 (* x y)       posit32-mul       (! :precision posit32 (* x y))     cost]
  [/.p32           ([x : posit32] [y : posit32])               posit32 (/ x y)       posit32-div       (! :precision posit32 (/ x y))     cost]
  [sqrt.p32        ([x : posit32])                             posit32 (sqrt x)      posit32-sqrt      (! :precision posit32 (sqrt x))    cost]
  [==.p32          ([x : posit32] [y : posit32])               bool    (== x y)      posit32=          (== x y)                           cost]
  [!=.p32          ([x : posit32] [y : posit32])               bool    (!= x y)      (negate posit32=) (!= x y)                           cost]
  [<.p32           ([x : posit32] [y : posit32])               bool    (< x y)       posit32<          (< x y)                            cost]
  [>.p32           ([x : posit32] [y : posit32])               bool    (> x y)       posit32>          (> x y)                            cost]
  [<=.p32          ([x : posit32] [y : posit32])               bool    (<= x y)      posit32<=         (<= x y)                           cost]
  [>=.p32          ([x : posit32] [y : posit32])               bool    (>= x y)      posit32>=         (>= x y)                           cost]
  ; Quire/posit fused impl
  [quire8-mul-add  ([x : quire8]  [y : posit8]  [z : posit8])  quire8  (+ x (* y z)) quire8-fdp-add    (! :precision quire8 (fdp x y z))  cost]
  [quire16-mul-add ([x : quire16] [y : posit16] [z : posit16]) quire16 (+ x (* y z)) quire16-fdp-add   (! :precision quire16 (fdp x y z)) cost]
  [quire32-mul-add ([x : quire32] [y : posit32] [z : posit32]) quire32 (+ x (* y z)) quire32-fdp-add   (! :precision quire32 (fdp x y z)) cost]
  [quire8-mul-sub  ([x : quire8]  [y : posit8]  [z : posit8])  quire8  (- x (* y z)) quire8-fdp-sub    (! :precision quire8 (fdm x y z))  cost]
  [quire16-mul-sub ([x : quire16] [y : posit16] [z : posit16]) quire16 (- x (* y z)) quire16-fdp-sub   (! :precision quire16 (fdm x y z)) cost]
  [quire32-mul-sub ([x : quire32] [y : posit32] [z : posit32]) quire32 (- x (* y z)) quire32-fdp-sub   (! :precision quire32 (fdm x y z)) cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONVERTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name             ([var : repr] ...) otype  spec fl             fpcore                        cost])
(platform-register-implementations!
 platform
 (; Posit/float implementations
  [binary64->posit8  ([x : binary64]) posit8   x double->posit8   (! :precision posit8 (cast x))   cost]
  [binary64->posit16 ([x : binary64]) posit16  x double->posit16  (! :precision posit16 (cast x))  cost]
  [binary64->posit32 ([x : binary64]) posit32  x double->posit32  (! :precision posit32 (cast x))  cost]
  [posit8->binary64  ([x : posit8])   binary64 x posit8->double   (! :precision binary64 (cast x)) cost]
  [posit16->binary64 ([x : posit16])  binary64 x posit16->double  (! :precision binary64 (cast x)) cost]
  [posit32->binary64 ([x : posit32])  binary64 x posit32->double  (! :precision binary64 (cast x)) cost]
  ; Quire/float implementations
  [binary64->quire8  ([x : binary64]) quire8   x double->quire8   (! :precision quire8 (cast x))   cost]
  [binary64->quire16 ([x : binary64]) quire16  x double->quire16  (! :precision quire16 (cast x))  cost]
  [binary64->quire32 ([x : binary64]) quire32  x double->quire32  (! :precision quire32 (cast x))  cost]
  [quire8->binary64  ([x : quire8])   binary64 x quire8->double   (! :precision binary64 (cast x)) cost]
  [quire16->binary64 ([x : quire16])  binary64 x quire16->double  (! :precision binary64 (cast x)) cost]
  [quire32->binary64 ([x : quire32])  binary64 x quire32->double  (! :precision binary64 (cast x)) cost]
  ; Quire/posit implementations
  [quire8->posit8    ([x : quire8])   posit8   x quire8->posit8   (! :precision posit8 (cast x))   cost]
  [quire16->posit16  ([x : quire16])  posit16  x quire16->posit16 (! :precision posit16 (cast x))  cost]
  [quire32->posit32  ([x : quire32])  posit32  x quire32->posit32 (! :precision posit32 (cast x))  cost]
  [posit8->quire8    ([x : posit8])   quire8   x posit8->quire8   (! :precision quire8 (cast x))   cost]
  [posit16->quire16  ([x : posit16])  quire16  x posit16->quire16 (! :precision quire16 (cast x))  cost]
  [posit32->quire32  ([x : posit32])  quire32  x posit32->quire32 (! :precision quire32 (cast x))  cost]))

(register-platform! platform)

(module+ main
  (display-platform platform))

;; Do not run this file during testing
(module test racket/base
  )
  
