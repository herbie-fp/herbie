#lang racket

(require math/flonum math/base math/bigfloat math/special-functions)
(require "../common.rkt" "../float.rkt" "../errors.rkt" "types.rkt")
(require "../bigcomplex.rkt" "../biginterval.rkt" "softposit.rkt")

(provide constant? variable? operator? operator-info constant-info parametric-operators
         variary-operators parametric-operators-reverse
         *unknown-d-ops* *unknown-f-ops* *loaded-ops*)

(module+ internals (provide operators constants))

(module+ test (require rackunit))

(define *unknown-d-ops* (make-parameter '()))
(define *unknown-f-ops* (make-parameter '()))

(define *loaded-ops* (make-parameter '()))

(define value? (apply or/c (map value-of types)))
(define bigvalue? (apply or/c (map bigvalue-of types)))

;; Constants's values are defined as functions to allow them to
;; depend on (bf-precision) and (flag 'precision 'double).

(define-table constants
  [type type?]
  [bf (->* () bigvalue?)]
  [fl (->* () value?)]
  [ival (or/c (->* () ival?) #f)]
  [nonffi (->* () value?)]
  [->c/double string?]
  [->c/mpfr (->* (string?) string?)]
  [->tex string?])

(define (constant-info constant field) (table-ref constants constant field))

(define-syntax-rule (define-constant constant ctype [key value] ...)
  (table-set! constants 'constant
              (make-hash (list (cons 'type 'ctype) (cons 'key value) ...))))

(define-constant PI real
  [bf (λ () pi.bf)]
  [fl (λ () pi)]
  [ival ival-pi]
  [nonffi (λ () pi)]
  [->c/double "atan2(1.0, 0.0)"]
  [->c/mpfr (curry format "mpfr_const_pi(~a, MPFR_RNDN)")]
  [->tex "\\pi"])

(define-constant E real
  [bf (λ () (bfexp 1.bf))]
  [fl (λ () (exp 1.0))]
  [ival ival-e]
  [nonffi (λ () (exp 1.0))]
  [->c/double "exp(1.0)"]
  [->c/mpfr (λ (x) (format "mpfr_set_si(~a, 1, MPFR_RNDN), mpfr_const_exp(~a, ~a, MPFR_RNDN)" x x x))]
  [->tex "e"])

(define-constant TRUE bool
  [bf (const true)]
  [fl (const true)]
  [nonffi (const true)]
  [ival (λ () (ival-bool true))]
  [->c/double "1"]
  [->c/mpfr (curry format "mpfr_set_si(~a, 1, MPFR_RNDN)")]
  [->tex "\\top"])

(define-constant FALSE bool
  [bf (const false)]
  [fl (const false)]
  [nonffi (const false)]
  [ival (λ () (ival-bool false))]
  [->c/double "0"]
  [->c/mpfr (curry format "mpfr_set_si(~a, 0, MPFR_RNDN)")]
  [->tex "\\perp"])

(define-constant I complex
  [bf (λ () (bigcomplex 0.bf 1.bf))]
  [fl (const 0+1i)]
  [nonffi (const 0+1i)]
  [ival #f]
  [->c/double "/* Complex numbers not supported in C */"]
  [->c/mpfr "/* Complex numbers not supported in C */"]
  [->tex "i"])

;; TODO: The contracts for operators are tricky because the number of arguments is unknown
;; There's no easy way to write such a contract in Racket, so I only constrain the output type.
(define (unconstrained-argument-number-> from/c to/c)
  (unconstrained-domain-> to/c))

;; TODO: the costs below seem likely to be incorrect, and also do we still need them?
(define-table operators
  [args  (listof (or/c '* natural-number/c))]
  [bf    (unconstrained-argument-number-> bigvalue? bigvalue?)]
  [fl    (unconstrained-argument-number-> value? value?)]
  [nonffi (unconstrained-argument-number-> value? value?)]
  [ival (or/c #f (unconstrained-argument-number-> ival? ival?))]
  [cost  natural-number/c]
  [type  (hash/c (or/c '* natural-number/c) (listof (list/c (or/c (listof type?) (list/c '* type?)) type?)))]
  [->c/double (unconstrained-argument-number-> string? string?)]
  [->c/mpfr   (unconstrained-argument-number-> string? string?)]
  [->tex      (unconstrained-argument-number-> string? string?)])

(define (operator-info operator field) (table-ref operators operator field))

(define (operator-remove! operator)
  (table-remove! operators operator)
  (*loaded-ops* (set-remove (*loaded-ops*) operator)))

(register-reset
 (λ ()
   (unless (flag-set? 'precision 'fallback)
     (for ([op (if (flag-set? 'precision 'double) (*unknown-d-ops*) (*unknown-f-ops*))])
       (operator-remove! op)))))

(define-syntax-rule (define-operator (operator atypes ...) rtype [key value] ...)
  (let ([type (hash (length '(atypes ...)) (list (list '(atypes ...) 'rtype)))]
        [args (list (length '(atypes ...)))])
    (*loaded-ops* (cons 'operator (*loaded-ops*)))
    (table-set! operators 'operator
                (make-hash (list (cons 'type type) (cons 'args args) (cons 'key value) ...)))))

(define (default-nonffi . args)
  (raise
   (make-exn:fail:unsupported
    (format "couldn't find ~a and no default implementation defined" 'operator)
    (current-continuation-marks))))

(define-operator (+ real real) real 
  [fl +] [bf bf+] [ival ival-add] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (curry format "mpfr_add(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a + ~a")]
  [nonffi +])

(define-operator (+.c complex complex) complex
  [fl +] [bf bf-complex-add] [ival #f] [cost 80]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a + ~a")]
  [nonffi +])

#|
(define-operator (+.p8 posit8 posit8) posit8
  [fl posit8-add] [bf big-posit8-add] [ival #f] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi +])

(define-operator (+.p16 posit16 posit16) posit16
  [fl posit16-add] [bf big-posit16-add] [ival #f] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi +])

(define-operator (+.p32 posit32 posit32) posit32
  [fl posit32-add] [bf big-posit32-add] [ival #f] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi +])
|#

(define-operator (- real [real]) real
  ;; Override the normal argument handling because - can be unary
  [args '(1 2)] [type (hash 1 '(((real) real)) 2 '(((real real) real)))]
  [fl -] [bf bf-] [cost 40] [ival (λ args (if (= (length args) 2) (apply ival-sub args) (apply ival-neg args)))]
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (λ (out x [y #f]) (if y (format "mpfr_sub(~a, ~a, ~a, MPFR_RNDN)" out x y) (format "mpfr_neg(~a, ~a, MPFR_RNDN)" out x)))]
  [->tex (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [nonffi -])

(define-operator (neg.c complex) complex
  [fl -] [bf bf-complex-neg] [ival #f] [cost 80]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "-~a")]
  [nonffi -])

(define-operator (-.c complex complex) complex
  [fl -] [bf bf-complex-sub] [ival #f] [cost 80]
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [nonffi -])

#|
(define-operator (neg.p8 posit8) posit8
  [fl posit8-neg] [bf big-posit8-neg] [ival #f] [cost 80]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "-~a")]
  [nonffi -])

(define-operator (neg.p16 posit16) posit16
  [fl posit16-neg] [bf big-posit16-neg] [ival #f] [cost 80]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "-~a")]
  [nonffi -])

(define-operator (neg.p32 posit32) posit32
  [fl posit32-neg] [bf big-posit32-neg] [ival #f] [cost 80]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "-~a")]
  [nonffi -])

(define-operator (-.p8 posit8 posit8) posit8
  [fl posit8-sub] [bf big-posit8-sub] [ival #f] [cost 80]
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a - ~a")]
  [nonffi -])

(define-operator (-.p16 posit16 posit16) posit16
  [fl posit16-sub] [bf big-posit16-sub] [ival #f] [cost 80]
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a - ~a")]
  [nonffi -])

(define-operator (-.p32 posit32 posit32) posit32
  [fl posit32-sub] [bf big-posit32-sub] [ival #f] [cost 80]
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a - ~a")]
  [nonffi -])
|#

(define-operator (* real real) real
  [fl *] [bf bf*] [ival ival-mult] [cost 40]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (curry format "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (*.c complex complex) complex
  [fl *] [bf bf-complex-mult] [ival #f] [cost 320]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

#|
(define-operator (*.p8 posit8 posit8) posit8
  [fl posit8-mul] [bf big-posit8-mul] [ival #f] [cost 320]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (*.p16 posit16 posit16) posit16
  [fl posit16-mul] [bf big-posit16-mul] [ival #f] [cost 320]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (*.p32 posit32 posit32) posit32
  [fl posit32-mul] [bf big-posit32-mul] [ival #f] [cost 320]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])
|#

(define-operator (/ real real) real
  [fl /] [bf bf/] [ival ival-div] [cost 40]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (curry format "mpfr_div(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

(define-operator (/.c complex complex) complex
  [fl /] [bf bf-complex-div] [ival #f] [cost 440]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

#|
(define-operator (/.p8 posit8 posit8) posit8
  [fl posit8-div] [bf big-posit8-div] [ival #f] [cost 440]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

(define-operator (/.p16 posit16 posit16) posit16
  [fl posit16-div] [bf big-posit16-div] [ival #f] [cost 440]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

(define-operator (/.p32 posit32 posit32) posit32
  [fl posit32-div] [bf big-posit32-div] [ival #f] [cost 440]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])
|#

(require ffi/unsafe)
(define-syntax (define-operator/libm stx)
  (syntax-case stx (real libm)
    [(_ (operator real ...) real [libm id_d id_f] [key value] ...)
     (let ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))])
       #`(begin
           (define (fallback . args)
             (warn 'fallback #:url "faq.html#native-ops"
                   "native `~a` not supported on your system, using fallback; ~a"
                   'operator
                   "use --disable precision:fallback to disable fallbacks")
             (apply (operator-info 'operator 'nonffi) args))
           (define double-proc (get-ffi-obj 'id_d #f (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double)
                                            (lambda () (*unknown-d-ops* (cons 'operator (*unknown-d-ops*))) fallback)))
           (define float-proc (get-ffi-obj 'id_f #f (_fun #,@(build-list num-args (λ (_) #'_float)) -> _float)
                                           (lambda () (*unknown-f-ops* (cons 'operator (*unknown-f-ops*))) fallback)))
           (define-operator (operator #,@(build-list num-args (λ (_) #'real))) real
             [fl (λ args (apply (if (flag-set? 'precision 'double) double-proc float-proc) args))]
             [key value] ...)))]))

(define-operator/libm (acos real) real
  [libm acos acosf] [bf bfacos] [ival ival-acos] [cost 90]
  [->c/double (curry format "acos(~a)")]
  [->c/mpfr (curry format "mpfr_acos(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cos^{-1} ~a")]
  [nonffi acos])

(define-operator/libm (acosh real) real
  [libm acosh acoshf] [bf bfacosh] [ival ival-acosh] [cost 55]
  [->c/double (curry format "acosh(~a)")]
  [->c/mpfr (curry format "mpfr_acosh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cosh^{-1} ~a")]
  [nonffi acosh])

(define-operator/libm (asin real) real
  [libm asin asinf] [bf bfasin] [ival ival-asin] [cost 105]
  [->c/double (curry format "asin(~a)")]
  [->c/mpfr (curry format "mpfr_asin(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sin^{-1} ~a")]
  [nonffi asin])

(define-operator/libm (asinh real) real
  [libm asinh asinhf] [bf bfasinh] [ival ival-asinh] [cost 55]
  [->c/double (curry format "asinh(~a)")]
  [->c/mpfr (curry format "mpfr_asinh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sinh^{-1} ~a")]
  [nonffi asinh])

(define-operator/libm (atan real) real
  [libm atan atanf] [bf bfatan] [ival ival-atan] [cost 105]
  [->c/double (curry format "atan(~a)")]
  [->c/mpfr (curry format "mpfr_atan(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan^{-1} ~a")]
  [nonffi atan])

(define-operator/libm (atan2 real real) real
  [libm atan2 atan2f] [bf bfatan2] [ival ival-atan2] [cost 140]
  [->c/double (curry format "atan2(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_atan2(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan^{-1}_* \\frac{~a}{~a}")]
  [nonffi atan])

(define-operator/libm (atanh real) real
  [libm atanh atanhf] [bf bfatanh] [ival ival-atanh] [cost 55]
  [->c/double (curry format "atanh(~a)")]
  [->c/mpfr (curry format "mpfr_atanh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tanh^{-1} ~a")]
  [nonffi atanh])

(define-operator/libm (cbrt real) real
  [libm cbrt cbrtf] [bf bfcbrt] [ival ival-cbrt] [cost 80]
  [->c/double (curry format "cbrt(~a)")]
  [->c/mpfr (curry format "mpfr_cbrt(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt[3]{~a}")]
  [nonffi (λ (x) (expt x (/ 1 3)))])

(define-operator/libm (ceil real) real
  [libm ceil ceilf] [bf bfceiling] [ival #f] [cost 80]
  [->c/double (curry format "ceil(~a)")]
  [->c/mpfr (curry format "mpfr_ceil(~a, ~a)")]
  [->tex (curry format "\\left\\lceil~a\\right\\rceil")]
  [nonffi ceiling])

(define (bfcopysign x y)
  (bf* (bfabs x) (bf (expt -1 (bigfloat-signbit y)))))

(define-operator/libm (copysign real real) real
  [libm copysign copysignf] [bf bfcopysign] [ival #f] [cost 80]
  [->c/double (curry format "copysign(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_copysign(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{copysign}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (if (>= y 0) (abs x) (- (abs x))))])

(define-operator/libm (cos real) real
  [libm cos cosf] [bf bfcos] [ival ival-cos] [cost 60]
  [->c/double (curry format "cos(~a)")]
  [->c/mpfr (curry format "mpfr_cos(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cos ~a")]
  [nonffi cos])

(define-operator/libm (cosh real) real
  [libm cosh coshf] [bf bfcosh] [ival ival-cosh] [cost 55]
  [->c/double (curry format "cosh(~a)")]
  [->c/mpfr (curry format "mpfr_cosh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cosh ~a")]
  [nonffi cosh])

(define-operator/libm (erf real) real
  [libm erf erff] [bf bferf] [ival ival-erf] [cost 70]
  [->c/double (curry format "erf(~a)")]
  [->c/mpfr (curry format "mpfr_erf(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{erf} ~a")]
  [nonffi erf])

(define-operator/libm (erfc real) real
  [libm erfc erfcf] [bf bferfc] [ival ival-erfc] [cost 70]
  [->c/double (curry format "erfc(~a)")]
  [->c/mpfr (curry format "mpfr_erfc(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{erfc} ~a")]
  [nonffi erfc])

(define-operator/libm (exp real) real
  [libm exp expf] [bf bfexp] [ival ival-exp] [cost 70]
  [->c/double (curry format "exp(~a)")]
  [->c/mpfr (curry format "mpfr_exp(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "e^{~a}")]
  [nonffi exp])

(define-operator (exp.c complex) complex
  [fl exp] [bf bf-complex-exp] [ival #f] [cost 70]
  [->c/double (curry format "exp(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "e^{~a}")]
  [nonffi exp])

(define-operator/libm (exp2 real) real
  [libm exp2 exp2f] [bf bfexp2] [ival #f] [cost 70]
  [->c/double (curry format "exp2(~a)")]
  [->c/mpfr (curry format "mpfr_exp2(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "2^{~a}")]
  [nonffi (λ (x) (expt 2 x))])

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define-operator/libm (expm1 real) real
  [libm expm1 expm1f] [bf bfexpm1] [ival ival-expm1] [cost 70]
  [->c/double (curry format "expm1(~a)")]
  [->c/mpfr (curry format "mpfr_expm1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{expm1}\\left(~a\\right)")]
  [nonffi (from-bigfloat bfexpm1)])

(define-operator/libm (fabs real) real
  [libm fabs fabsf] [bf bfabs] [ival ival-fabs] [cost 40]
  [->c/double (curry format "fabs(~a)")]
  [->c/mpfr (curry format "mpfr_abs(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\left|~a\\right|")]
  [nonffi abs])

(define (bffdim x y)
  (if (bf> x y)
    (bf- x y)
    0.bf))

(define-operator/libm (fdim real real) real
  [libm fdim fdimf] [bf bffdim] [ival #f] [cost 55]
  [->c/double (curry format "fdim(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_dim(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fdim}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (max (- x y) 0))])

(define-operator/libm (floor real) real
  [libm floor floorf] [bf bffloor] [ival #f] [cost 55]
  [->c/double (curry format "floor(~a)")]
  [->c/mpfr (curry format "mpfr_floor(~a, ~a)")]
  [->tex (curry format "\\left\\lfloor~a\\right\\rfloor")]
  [nonffi (λ (x) (floor x))])

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define-operator/libm (fma real real real) real
  [libm fma fmaf] [bf bffma] [ival ival-fma] [cost 55]
  [->c/double (curry format "fma(~a, ~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fma(~a, ~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fma}\\left(~a, ~a, ~a\\right)")]
  [nonffi (λ (x y z) (bigfloat->flonum (bf+ (bf* (bf x) (bf y)) (bf z))))])

(define-operator/libm (fmax real real) real
  [libm fmax fmaxf] [bf bfmax] [ival #f] [cost 55]
  [->c/double (curry format "fmax(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmax(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{max}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (max x y)]))])

(define-operator/libm (fmin real real) real
  [libm fmin fminf] [bf bfmin] [ival #f] [cost 55]
  [->c/double (curry format "fmin(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmin(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{min}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (min x y)]))])

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define-operator/libm (fmod real real) real
  [libm fmod fmodf] [bf bffmod] [ival ival-fmod] [cost 70]
  [->c/double (curry format "fmod(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmod(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\bmod ~a")]
  [nonffi (from-bigfloat bffmod)])

(define-operator/libm (hypot real real) real
  [libm hypot hypotf] [bf bfhypot] [ival ival-hypot] [cost 55]
  [->c/double (curry format "hypot(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_hypot(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{hypot}\\left(~a, ~a\\right)")]
  [nonffi (from-bigfloat bfhypot)])

(define-operator/libm (j0 real) real
  [libm j0 j0f] [bf bfbesj0] [ival #f] [cost 55]
  [->c/double (curry format "j0(~a)")]
  [->c/mpfr (curry format "mpfr_j0(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "j_0\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesj0)])

(define-operator/libm (j1 real) real
  [libm j1 j1f] [bf bfbesj1] [ival #f] [cost 55]
  [->c/double (curry format "j1(~a)")]
  [->c/mpfr (curry format "mpfr_j1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "j_1\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesj1)])

(define-operator/libm (lgamma real) real
  [libm lgamma lgammaf] [bf bflog-gamma] [ival #f] [cost 55]
  [->c/double (curry format "lgamma(~a)")]
  [->c/mpfr (curry format "mpfr_lngamma(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{lgamma} \\left( ~a \\right)")]
  [nonffi log-gamma])

(define-operator/libm (log real) real
  [libm log logf] [bf bflog] [ival ival-log] [cost 70]
  [->c/double (curry format "log(~a)")]
  [->c/mpfr (curry format "mpfr_log(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log ~a")]
  [nonffi log])

(define-operator (log.c complex) complex
  [fl log] [bf bf-complex-log] [ival #f] [cost 265]
  [->c/double (curry format "log(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\log ~a")]
  [nonffi log])

(define-operator/libm (log10 real) real
  [libm log10 log10f] [bf bflog10] [ival #f] [cost 70]
  [->c/double (curry format "log10(~a)")]
  [->c/mpfr (curry format "mpfr_log10(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_{10} ~a")]
  [nonffi (λ (x) (log x 10))])

(define-operator/libm (log1p real) real
  [libm log1p log1pf] [bf bflog1p] [ival ival-log1p] [cost 90]
  [->c/double (curry format "log1p(~a)")]
  [->c/mpfr (curry format "mpfr_log1p(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{log1p}\\left(~a\\right)")]
  [nonffi (from-bigfloat bflog1p)])

(define-operator/libm (log2 real) real
  [libm log2 log2f] [bf bflog2] [ival #f] [cost 70]
  [->c/double (curry format "log2(~a)")]
  [->c/mpfr (curry format "mpfr_log2(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_{2} ~a")]
  [nonffi (from-bigfloat bflog2)])

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define-operator/libm (logb real) real
  [libm logb logbf] [bf bflogb] [ival #f] [cost 70]
  [->c/double (curry format "logb(~a)")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_exp(~a), MPFR_RNDN)")]
  [->tex (curry format "\\log_{b} ~a")]
  [nonffi (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))])

(define-operator/libm (pow real real) real
  [libm pow powf] [bf bfexpt] [ival ival-pow] [cost 210]
  [->c/double (curry format "pow(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_pow(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "{~a}^{~a}")]
  [nonffi expt])

(define-operator (pow.c complex complex) complex
  [fl expt] [bf bf-complex-pow] [ival #f] [cost 210]
  [->c/double (curry format "pow(~a, ~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "{~a}^{~a}")]
  [nonffi expt])

(define (bfremainder x mod)
  (bf- x (bf* (bfround (bf/ x mod)) mod)))

(define-operator/libm (remainder real real) real
  [libm remainder remainderf] [bf bfremainder] [ival ival-remainder] [cost 70]
  [->c/double (curry format "remainder(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_remainder(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\mathsf{rem} ~a")]
  [nonffi remainder])

(define-operator/libm (rint real) real
  [libm rint rintf] [bf bfrint] [ival #f] [cost 70]
  [->c/double (curry format "rint(~a)")]
  [->c/mpfr (curry format "mpfr_rint(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{rint} ~a")]
  [nonffi round])

(define-operator/libm (round real) real
  [libm round roundf] [bf bfround] [ival #f] [cost 70]
  [->c/double (curry format "round(~a)")]
  [->c/mpfr (curry format "mpfr_round(~a, ~a)")]
  [->tex (curry format "\\mathsf{round} ~a")]
  [nonffi round])

(define-operator/libm (sin real) real
  [libm sin sinf] [bf bfsin] [ival ival-sin] [cost 60]
  [->c/double (curry format "sin(~a)")]
  [->c/mpfr (curry format "mpfr_sin(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sin ~a")]
  [nonffi sin])

(define-operator/libm (sinh real) real
  [libm sinh sinhf] [bf bfsinh] [ival ival-sinh] [cost 55]
  [->c/double (curry format "sinh(~a)")]
  [->c/mpfr (curry format "mpfr_sinh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sinh ~a")]
  [nonffi sinh])

(define-operator/libm (sqrt real) real
  [libm sqrt sqrtf] [bf bfsqrt] [ival ival-sqrt] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (curry format "mpfr_sqrt(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])

(define-operator (sqrt.c complex) complex
  [fl sqrt] [bf bf-complex-sqrt] [ival #f] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])

#|
(define-operator (sqrt.p8 posit8) posit8
  [fl posit8-sqrt] [bf big-posit8-sqrt] [ival #f] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])

(define-operator (sqrt.p16 posit16) posit16
  [fl posit16-sqrt] [bf big-posit16-sqrt] [ival #f] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])

(define-operator (sqrt.p32 posit32) posit32
  [fl posit32-sqrt] [bf big-posit32-sqrt] [ival #f] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])
|#

(define-operator/libm (tan real) real
  [libm tan tanf] [bf bftan] [ival ival-tan] [cost 95]
  [->c/double (curry format "tan(~a)")]
  [->c/mpfr (curry format "mpfr_tan(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan ~a")]
  [nonffi tan])

(define-operator/libm (tanh real) real
  [libm tanh tanhf] [bf bftanh] [ival ival-tanh] [cost 55]
  [->c/double (curry format "tanh(~a)")]
  [->c/mpfr (curry format "mpfr_tanh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tanh ~a")]
  [nonffi tanh])

(define-operator/libm (tgamma real) real
  [libm tgamma tgammaf] [bf bfgamma] [ival #f] [cost 55]
  [->c/double (curry format "tgamma(~a)")]
  [->c/mpfr (curry format "mpfr_gamma(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\Gamma\\left(~a\\right)")]
  [nonffi gamma])

(define-operator/libm (trunc real) real
  [libm trunc truncf] [bf bftruncate] [ival #f] [cost 55]
  [->c/double (curry format "trunc(~a)")]
  [->c/mpfr (curry format "mpfr_trunc(~a, ~a)")]
  [->tex (curry format "\\mathsf{trunc}\\left(~a\\right)")]
  [nonffi truncate])

(define-operator/libm (y0 real) real
  [libm y0 y0f] [bf bfbesy0] [ival #f] [cost 55]
  [->c/double (curry format "y0(~a)")]
  [->c/mpfr (curry format "mpfr_y0(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "y_0\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesy0)])

(define-operator/libm (y1 real) real
  [libm y1 y1f] [bf bfbesy1] [ival #f] [cost 55]
  [->c/double (curry format "y1(~a)")]
  [->c/mpfr (curry format "mpfr_y1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "y_1\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesy1)])

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define-operator (if bool real real) real ; types not used, special cased in type checker
  [fl if-fn] [bf if-fn] [cost 65] [ival ival-if]
  [->c/double (curry format "~a ? ~a : ~a")]
  [->c/mpfr
   (λ (out c a b)
     (format "if (mpfr_get_si(~a, MPFR_RNDN)) { mpfr_set(~a, ~a, MPFR_RNDN); } else { mpfr_set(~a, ~a, MPFR_RNDN); }" c out a out b))]
  [->tex (curry format "~a ? ~a : ~a")]
  [nonffi if-fn])

(define ((infix-joiner str) . args)
  (string-join args str))

(define-operator (== real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator =)] [bf (comparator bf=)] [cost 65] [ival ival-==]
  [->c/double (curry format "~a == ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) == 0, MPFR_RNDN)")] ; TODO: cannot handle variary =
  [->tex (infix-joiner " = ")]
  [nonffi (comparator =)])

#|
(define-operator (real->posit8 real) posit8
  ; Override number of arguments
  [fl double->posit8] [bf bf-double->posit8] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit8])

(define-operator (real->posit16 real) posit16
  ; Override number of arguments
  [fl double->posit16] [bf bf-double->posit16] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit16])

(define-operator (real->posit32 real) posit32
  ; Override number of arguments
  [fl double->posit32] [bf bf-double->posit32] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit32])

(define-operator (posit8->real posit8) real
  ; Override number of arguments
  [fl posit8->double] [bf big-posit8->double] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit8])

(define-operator (posit16->real posit16) real
  ; Override number of arguments
  [fl posit16->double] [bf big-posit16->double] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit16])

(define-operator (posit32->real posit32) real
  ; Override number of arguments
  [fl posit32->double] [bf big-posit32->double] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit32])

(define-operator (real->quire8 real) quire8
  ; Override number of arguments
  [fl double->quire8] [bf bf-double->quire8] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire8])

(define-operator (real->quire16 real) quire16
  ; Override number of arguments
  [fl double->quire16] [bf bf-double->quire16] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire16])

(define-operator (real->quire32 real) quire32
  ; Override number of arguments
  [fl double->quire32] [bf bf-double->quire32] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire32])

(define-operator (quire8->real quire8) real
  ; Override number of arguments
  [fl quire8->double] [bf bf-quire8->double] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire8])

(define-operator (quire16->real quire16) real
  ; Override number of arguments
  [fl quire16->double] [bf bf-quire16->double] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire16])

(define-operator (quire16->real quire16) real
  ; Override number of arguments
  [fl quire16->double] [bf bf-quire16->double] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire32])

(define-operator (quire8-mul-add quire8 posit8 posit8) quire8
  ; Override number of arguments
  [fl quire8-fdp-add] [bf bf-quire8-fdp-add] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qma}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire32])

(define-operator (quire16-mul-add quire16 posit16 posit16) quire16
  ; Override number of arguments
  [fl quire16-fdp-add] [bf bf-quire16-fdp-add] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qma}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire32])

(define-operator (quire32-mul-add quire32 posit32 posit32) quire32
  ; Override number of arguments
  [fl quire32-fdp-add] [bf bf-quire32-fdp-add] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qma}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire32])

(define-operator (quire8-mul-sub quire8 posit8 posit8) quire8
  ; Override number of arguments
  [fl quire8-fdp-sub] [bf bf-quire8-fdp-sub] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qms}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire8])

(define-operator (quire16-mul-sub quire16 posit16 posit16) quire16
  ; Override number of arguments
  [fl quire16-fdp-sub] [bf bf-quire16-fdp-sub] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qms}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire16])

(define-operator (quire32-mul-sub quire32 posit32 posit32) quire32
  ; Override number of arguments
  [fl quire32-fdp-sub] [bf bf-quire32-fdp-sub] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qms}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire32])

(define-operator (quire8->posit8 quire8) posit8
  ; Override number of arguments
  [fl quire8->posit8] [bf bf-quire8->posit8] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi quire8->posit8])

(define-operator (quire16->posit16 quire16) posit16
  ; Override number of arguments
  [fl quire16->posit16] [bf bf-quire16->posit16] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi quire16->posit16])

(define-operator (quire32->posit32 quire32) posit32
  ; Override number of arguments
  [fl quire32->posit32] [bf bf-quire32->posit32] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi quire32->posit32])

(define-operator (posit8->quire8 posit8) quire8
  ; Override number of arguments
  [fl posit8->quire8] [bf big-posit8->quire8] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi posit8->quire8])

(define-operator (posit16->quire16 posit16) quire16
  ; Override number of arguments
  [fl posit16->quire16] [bf big-posit16->quire16] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi posit16->quire16])

(define-operator (posit32->quire32 posit32) quire32
  ; Override number of arguments
  [fl posit32->quire32] [bf big-posit32->quire32] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi posit32->quire32])
|#

(define-operator (complex real real) complex
  ; Override number of arguments
  [fl make-rectangular] [bf bigcomplex] [cost 0] [ival #f]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a + ~a i")]
  [nonffi make-rectangular])

(define-operator (re complex) real
  ; Override number of arguments
  [fl real-part] [bf bigcomplex-re] [cost 0] [ival #f]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\Re(~a)")]
  [nonffi real-part])

(define-operator (im complex) real
  ; Override number of arguments
  [fl imag-part] [bf bigcomplex-im] [cost 0] [ival #f]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\Im(~a)")]
  [nonffi imag-part])

(define-operator (conj complex) complex
  ; Override number of arguments
  [fl conjugate] [bf bf-complex-conjugate] [cost 0] [ival #f]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\overline{~a}")]
  [nonffi conjugate])

(define-operator (!= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl !=-fn] [bf bf!=-fn] [cost 65] [ival ival-!=]
  [->c/double (curry format "~a != ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) != 0, MPFR_RNDN)")] ; TODO: cannot handle variary !=
  [->tex (infix-joiner " \\ne ")]
  [nonffi !=-fn])

(define-operator (< real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator <)] [bf (comparator bf<)] [cost 65] [ival ival-<]
  [->c/double (curry format "~a < ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) < 0, MPFR_RNDN)")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator <)])

#|
(define-operator (<.p8 posit8 posit8) bool
  ; Override number of arguments
  [type #hash((* . (((* posit8) bool))))] [args '(*)]
  [fl (comparator posit8<)] [bf (comparator big-posit8<)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator posit8<)])

(define-operator (<.p16 posit16 posit16) bool
  ; Override number of arguments
  [type #hash((* . (((* posit16) bool))))] [args '(*)]
  [fl (comparator posit16<)] [bf (comparator big-posit16<)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator posit16<)])

(define-operator (<.p32 posit32 posit32) bool
  ; Override number of arguments
  [type #hash((* . (((* posit32) bool))))] [args '(*)]
  [fl (comparator posit32<)] [bf (comparator big-posit32<)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator posit32<)])
|#

(define-operator (> real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator >)] [bf (comparator bf>)] [cost 65] [ival ival->]
  [->c/double (curry format "~a > ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) > 0, MPFR_RNDN)")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator >)])

#|
(define-operator (>.p8 posit8 posit8) bool
  ; Override number of arguments
  [type #hash((* . (((* posit8) bool))))] [args '(*)]
  [fl (comparator posit8>)] [bf (comparator big-posit8>)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator posit8>)])

(define-operator (>.p16 posit16 posit16) bool
  ; Override number of arguments
  [type #hash((* . (((* posit16) bool))))] [args '(*)]
  [fl (comparator posit16>)] [bf (comparator big-posit16>)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator posit16>)])

(define-operator (>.p32 posit32 posit32) bool
  ; Override number of arguments
  [type #hash((* . (((* posit32) bool))))] [args '(*)]
  [fl (comparator posit32>)] [bf (comparator big-posit32>)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator posit32>)])
|#

(define-operator (<= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator <=)] [bf (comparator bf<=)] [cost 65] [ival ival-<=]
  [->c/double (curry format "~a <= ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) <= 0, MPFR_RNDN)")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator <=)])

#|
(define-operator (<=.p8 posit8 posit8) bool
  ; Override number of arguments
  [type #hash((* . (((* posit8) bool))))] [args '(*)]
  [fl (comparator posit8<=)] [bf (comparator big-posit8<=)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator posit8<=)])

(define-operator (<=.p16 posit16 posit16) bool
  ; Override number of arguments
  [type #hash((* . (((* posit16) bool))))] [args '(*)]
  [fl (comparator posit16<=)] [bf (comparator big-posit16<=)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator posit16<=)])

(define-operator (<=.p32 posit32 posit32) bool
  ; Override number of arguments
  [type #hash((* . (((* posit32) bool))))] [args '(*)]
  [fl (comparator posit32<=)] [bf (comparator big-posit32<=)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator posit32<=)])
|#

(define-operator (>= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator >=)] [bf (comparator bf>=)] [cost 65] [ival ival->=]
  [->c/double (curry format "~a >= ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) >= 0, MPFR_RNDN)")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator >=)])

#|
(define-operator (>=.p8 posit8 posit8) bool
  ; Override number of arguments
  [type #hash((* . (((* posit8) bool))))] [args '(*)]
  [fl (comparator posit8>=)] [bf (comparator big-posit8>=)] [ival #f] [cost 65]
  [->c/double (curry format "/* Error: no posit support in C */")]
  [->c/mpfr (curry format "/* Error: no posit support in C */")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator posit8>=)])

(define-operator (>=.p16 posit16 posit16) bool
  ; Override number of arguments
  [type #hash((* . (((* posit16) bool))))] [args '(*)]
  [fl (comparator posit16>=)] [bf (comparator big-posit16>=)] [ival #f] [cost 65]
  [->c/double (curry format "/* Error: no posit support in C */")]
  [->c/mpfr (curry format "/* Error: no posit support in C */")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator posit16>=)])

(define-operator (>=.p32 posit32 posit32) bool
  ; Override number of arguments
  [type #hash((* . (((* posit32) bool))))] [args '(*)]
  [fl (comparator posit32>=)] [bf (comparator big-posit32>=)] [ival #f] [cost 65]
  [->c/double (curry format "/* Error: no posit support in C */")]
  [->c/mpfr (curry format "/* Error: no posit support in C */")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator posit32>=)])
|#

(define-operator (not bool) bool
  [fl not] [bf not] [cost 65] [ival ival-not]
  [->c/double (curry format "!~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, !mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (curry format "\\neg ~a")]
  [nonffi not])

(define-operator (and bool bool) bool
  ; Override number of arguments
  [type #hash((* . (((* bool) bool))))] [args '(*)]
  [fl and-fn] [bf and-fn] [cost 55] [ival ival-and]
  [->c/double (curry format "~a && ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) && mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (infix-joiner " \\land ")]
  [nonffi and-fn])

(define-operator (or bool bool) bool
  ; Override number of arguments
  [type #hash((* . (((* bool) bool))))] [args '(*)]
  [fl or-fn] [bf or-fn] [cost 55] [ival ival-or]
  [->c/double (curry format "~a || ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) || mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (infix-joiner " \\lor ")]
  [nonffi or-fn])

(define (operator? op)
  (and (symbol? op) (dict-has-key? (cdr operators) op)))

(define (constant? var)
  (or (number? var)
      (and (symbol? var)
           (dict-has-key? (cdr constants) var))))

(define (variable? var)
  (and (symbol? var) (not (constant? var))))

(define parametric-operators
  #hash([+ . ((+ real real real)
              (+.c complex complex complex)
              #;(+.p8 posit8 posit8 posit8)
              #;(+.p16 posit16 posit16 posit16)
              #;(+.p32 posit32 posit32 posit32))]
        [- . ((- real real real) (- real real)
              (-.c complex complex complex) (neg.c complex complex)
              #;(-.p8 posit8 posit8 posit8) (neg.p8 posit8 posit8)
              #;(-.p16 posit16 posit16 posit16) (neg.p16 posit16 posit16)
              #;(-.p32 posit32 posit32 posit32) (neg.p32 posit32 posit32)) ]
        [* . ((* real real real)
              (*.c complex complex complex)
              #;(*.p8 posit8 posit8 posit8)
              #;(*.p16 posit16 posit16 posit16)
              #;(*.p32 posit32 posit32 posit32))]
        [/ . ((/ real real real)
              (/.c complex complex complex)
              #;(/.p8 posit8 posit8 posit8)
              #;(/.p16 posit16 posit16 posit16)
              #;(/.p32 posit32 posit32 posit32))]
        [pow . ((pow real real real) (pow.c complex complex complex))]
        [exp . ((exp real real) (exp.c complex complex))]
        [log . ((log real real) (log.c complex complex))]
        [sqrt . ((sqrt real real)
                 (sqrt.c complex complex)
                 #;(sqrt.p8 posit8 posit8)
                 #;(sqrt.p16 posit16 posit16)
                 #;(sqrt.p32 posit32 posit32))]
        [< . ((< bool real real)
              #;(<.p8 bool posit8 posit8)
              #;(<.p16 bool posit16 posit16)
              #;(<.p32 bool posit32 posit32))]
        [<= . ((<= bool real real)
              #;(<=.p8 bool posit8 posit8)
              #;(<=.p16 bool posit16 posit16)
              #;(<=.p32 bool posit32 posit32))]
        [> . ((> bool real real)
              #;(>.p8 bool posit8 posit8)
              #;(>.p16 bool posit16 posit16)
              #;(>.p32 bool posit32 posit32))]
        [>= . ((>= bool real real)
              #;(>=.p8 bool posit8 posit8)
              #;(>=.p16 bool posit16 posit16)
              #;(>=.p32 bool posit32 posit32))]
        [== . ((== bool real real)
              #;(=.p8 bool posit8 posit8)
              #;(=.p16 bool posit16 posit16)
              #;(=.p32 bool posit32 posit32))]))

(define variary-operators '(< <= > >= == !=))

(define parametric-operators-reverse
  (make-hash (append* (for/list ([(key-val) (hash->list parametric-operators)])
    (define key (car key-val))
    (define vals (cdr key-val))
    (for/list ([val vals])
      (cons (car val) key))))))

(module+ test
  (for ([(k r) (in-hash (cdr constants))] #:when true
        [(f c) (in-dict (car constants))] [v (in-list r)] #:when (flat-contract? c))
    (check-pred (flat-contract-predicate c) v)))
