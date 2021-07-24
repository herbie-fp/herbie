#lang racket

;; binary32 builtin plugin

(require math/base math/bigfloat math/flonum math/special-functions)
(require (submod "syntax/syntax.rkt" internals)
         (submod "interface.rkt" internals)
         "syntax/syntax.rkt" "common.rkt"
         "errors.rkt")

; needed by src/syntax/test-rules.rkt
; cannot be exported with contracts since ffi/unsafe is required
(provide ->float32) 

(module+ test
  (require rackunit))

; Racket CS made single-flonums a little confusing
; All single-precision code is here to make things easier

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; float32 library ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Racket 8.0
(define at-least-racket-8?
  (>= (string->number (substring (version) 0 1)) 8))

; Need a placeholder for < 7.3
(define single-flonum-available?
  (let ([single-flonum-available? (const (equal? (system-type 'vm) 'racket))])
    (local-require racket/flonum)
    single-flonum-available?))

; Need a placeholder for < 8.0
(define cast-single
  (let ([flsingle identity])
    (local-require racket/flonum)
    flsingle))

; Returns true if single flonum is available directly (BC)
; or through emulation (CS, >= 8.0)
(define (single-flonum-supported?)
  (or (single-flonum-available?) at-least-racket-8?))

; Contracts are problematic (BC, < 8.0): values not
; necessarily single flonums. Bug in Herbie?
(define float32?
  (if at-least-racket-8?
      flonum?
      single-flonum?))

(define (->float32 x)
  (if at-least-racket-8?
      (cast-single (exact->inexact x))
      (real->single-flonum x)))

(define (float32->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f #f))

(define (float32->ordinal x)
  (if (negative? x)
      (- (float32->bit-field (- x)))
      (float32->bit-field (abs x))))

(define (bit-field->float32 x)
  (->float32 (floating-point-bytes->real (integer->integer-bytes x 4 #f #f) #f)))

(define (ordinal->float32 x)
  (if (negative? x)
      (- (bit-field->float32 (- x)))
      (bit-field->float32 x)))

(define (float32-step x n)
  (ordinal->float32 (+ (float32->ordinal x) n)))

(define (bigfloat->float32 x)
  (define loprec (parameterize ([bf-precision 24]) (bf+ 0.bf x)))
  (define y (->float32 (bigfloat->flonum loprec)))
  (define x2 (bf y))
  (match (bf-rounding-mode)
   ['nearest y]
   ['up     (if (bf< x2 x) (float32-step y 1) y)]
   ['down   (if (bf> x2 x) (float32-step y -1) y)]
   ['zero   (if (bf< x 0.bf)
                (if (bf< x2 x) (float32-step y 1) y)
                (if (bf> x2 x) (float32-step y -1) y))]))

(define-syntax-rule (float32-fun name op)
  (define name (compose ->float32 op)))

(define-syntax-rule (float32-funs [name op] ...)
  (begin (float32-fun name op) ...))

(float32-funs
  [fl32+  +]
  [fl32-  -]
  [fl32*  *]
  [fl32/  /])
    
(module+ test
  (check-equal? (fl32+ 1.0 2.0) (->float32 3.0))
  (check-equal? (fl32- 1.0 2.0) (->float32 -1.0))
  (check-equal? (fl32* 1.0 2.0) (->float32 2.0))
  (check-equal? (fl32/ 1.0 2.0) (->float32 0.5)))

; for define-libm-operator (must be top-level)
(require ffi/unsafe)

;; Only load everything below if single flonum supported?
;; BC or CS (>= 8.0)
(when (single-flonum-supported?)

; (eprintf "Loading binary32 support...\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-representation (binary32 real float32?)
  bigfloat->float32
  bf
  (shift 31 ordinal->float32)
  (unshift 31 float32->ordinal)
  32
  (disjoin nan? infinite?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant-impl (PI PI.f32) binary32
  [fl (const (->float32 pi))])

(define-constant-impl (E E.f32) binary32
  [fl (const (->float32 (exp 1.0)))])

(define-constant-impl (INFINITY INFINITY.f32) binary32
  [fl (const (->float32 +inf.0))])

(define-constant-impl (NAN NAN.f32) binary32
  [fl (const (->float32 +nan.0))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-operator stx)
  (syntax-case stx (real libm)
    [(_ (op real ...) [libm id] [nonffi fn] [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.f32)])
       #`(begin
           (define fl-proc (get-ffi-obj 'id #f (_fun #,@(build-list num-args (λ (_) #'_float)) -> _float)
                                        (lambda ()
                                          (*unknown-ops* (cons '#,name (*unknown-ops*)))
                                          (warn 'fallback #:url "faq.html#native-ops"
                                                "native `~a` not supported on your system, using fallback; ~a"
                                                'op
                                                "use --disable precision:fallback to disable fallbacks")
                                          fn)))
           (define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'binary32))) binary32
             [fl fl-proc])))]))

(define-syntax-rule (define-1ary-libm-operator op name fallback)
  (define-libm-operator (op real) [libm name] [nonffi fallback]))

(define-syntax-rule (define-2ary-libm-operator op name fallback)
  (define-libm-operator (op real real) [libm name] [nonffi fallback]))

(define-syntax-rule (define-1ary-libm-operators [op name fallback] ...)
  (begin (define-1ary-libm-operator op name fallback) ...))

(define-syntax-rule (define-2ary-libm-operators [op name fallback] ...)
  (begin (define-2ary-libm-operator op name fallback) ...))

(define (no-complex fun)
  (λ xs
     (define res (apply fun xs))
     (if (real? res) res +nan.0)))

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define (bffma x y z)
  (bf+ (bf* x y) z))


(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

(define-1ary-libm-operators
 [acos acosf (no-complex acos)]
 [acosh acoshf (no-complex acosh)]
 [asin asinf (no-complex asin)]
 [asinh asinhf (no-complex asinh)]
 [atan atanf (no-complex atan)]
 [atanh atanhf (no-complex atanh)]
 [cbrt cbrtf (no-complex (λ (x) (expt x 1/3)))]
 [ceil ceilf ceiling]
 [cos cosf cos]
 [cosh coshf cosh]
 [erf erff (no-complex erf)]
 [erfc erfcf erfc]
 [exp expf exp]
 [exp2 exp2f (no-complex (λ (x) (expt 2 x)))]
 [expm1 expm1f (from-bigfloat bfexpm1)]
 [fabs fabsf abs]
 [floor floorf floor]
 [j0 j0f (from-bigfloat bfbesj0)]
 [j1 j1f (from-bigfloat bfbesj1)]
 [lgamma lgammaf log-gamma]
 [log logf (no-complex log)]
 [log10 log10f (no-complex (λ (x) (log x 10)))]
 [log1p log1pf (from-bigfloat bflog1p)]
 [log2 log2f (from-bigfloat bflog2)]
 [logb logbf (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))]
 [rint rintf round]
 [round roundf round]
 [sin sinf sin]
 [sinh sinhf sinh]
 [sqrt sqrtf (no-complex sqrt)]
 [tan tanf tan]
 [tanh tanhf tanh]
 [tgamma tgammaf gamma]
 [trunc truncf truncate]
 [y0 y0f (from-bigfloat bfbesy0)]
 [y1 y1f (from-bigfloat bfbesy1)])

(define-2ary-libm-operators
 [atan2 atan2f (no-complex atan)]
 [copysign copysignf (λ (x y) (if (>= y 0) (abs x) (- (abs x))))]
 [fdim fdimf (λ (x y) (max (- x y) 0))]
 [fmax fmaxf (λ (x y) (cond [(nan? x) y] [(nan? y) x] [else (max x y)]))]
 [fmin fminf (λ (x y) (cond [(nan? x) y] [(nan? y) x] [else (min x y)]))]
 [fmod fmodf (from-bigfloat bffmod)]
 [hypot hypotf (from-bigfloat bfhypot)]
 [pow powf (no-complex expt)]
 [remainder remainderf remainder])

(define-libm-operator (fma real real real)
 [libm fmaf] [nonffi (from-bigfloat bffma)])

(define-operator-impl (== ==.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator =)])

(define-operator-impl (!= !=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (negate (comparator =))])

(define-operator-impl (< <.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator <)])

(define-operator-impl (> >.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator >)])

(define-operator-impl (<= <=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)])

(define-operator-impl (>= >=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator >=)])

(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])

)
