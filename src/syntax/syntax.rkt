#lang racket

(require math/flonum)
(require math/base)
(require math/bigfloat)
(require math/special-functions)
(require "../common.rkt")
(require "../float.rkt")
(require "../bigcomplex.rkt")

(provide constant? variable? operator? operator-info constant-info prune-operators!
         parametric-operators *unknown-d-ops* *unknown-f-ops* *loaded-ops*)

(define *unknown-d-ops* (make-parameter '()))
(define *unknown-f-ops* (make-parameter '()))

(define *loaded-ops* (make-parameter '()))

(define (type? x) (or (equal? x 'real) (equal? x 'bool) (equal? x 'complex)))

;; Constants's values are defined as functions to allow them to
;; depend on (bf-precision) and (flag 'precision 'double).

(define-table constants
  [type type?]
  [bf (->* () (or/c bigfloat? boolean?))]
  [fl (->* () (or/c flonum? boolean?))]
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
  [->c/double "atan2(1.0, 0.0)"]
  [->c/mpfr (curry format "mpfr_const_pi(~a, MPFR_RNDN)")]
  [->tex "\\pi"])

(define-constant E real
  [bf (λ () (bfexp 1.bf))]
  [fl (λ () (exp 1.0))]
  [->c/double "exp(1.0)"]
  [->c/mpfr (λ (x) (format "mpfr_set_si(~a, 1, MPFR_RNDN), mpfr_const_exp(~a, ~a, MPFR_RNDN)" x x x))]
  [->tex "e"])

(define-constant TRUE bool
  [bf (const true)]
  [fl (const true)]
  [->c/double "1"]
  [->c/mpfr (curry format "mpfr_set_si(~a, 1, MPFR_RNDN)")]
  [->tex "\\top"])

(define-constant FALSE bool
  [bf (const false)]
  [fl (const false)]
  [->c/double "0"]
  [->c/mpfr (curry format "mpfr_set_si(~a, 0, MPFR_RNDN)")]
  [->tex "\\perp"])

;; TODO: The contracts for operators are tricky because the number of arguments is unknown
;; There's no easy way to write such a contract in Racket, so I only constrain the output type.
(define (unconstrained-argument-number-> from/c to/c)
  (unconstrained-domain-> to/c))

;; TODO: the costs below seem likely to be incorrect, and also do we still need them?
(define-table operators
  [args  (listof (or/c '* natural-number/c))]
  [bf    (unconstrained-argument-number-> (or/c bigfloat? boolean? bigcomplex?) (or/c bigfloat? boolean? bigcomplex?))]
  [fl    (unconstrained-argument-number-> (or/c flonum? boolean? complex?) (or/c flonum? boolean? complex?))]
  [nonffi (unconstrained-argument-number-> (or/c real? boolean? complex?) (or/c real? boolean? complex?))]
  [cost  natural-number/c]
  [type  (hash/c (or/c '* natural-number/c) (listof (list/c (or/c (listof type?) (list/c '* type?)) type?)))]
  [->c/double (unconstrained-argument-number-> string? string?)]
  [->c/mpfr   (unconstrained-argument-number-> string? string?)]
  [->tex      (unconstrained-argument-number-> string? string?)])

(define (operator-info operator field) (table-ref operators operator field))

(define (operator-remove! operator)
  (table-remove! operators operator)
  (*loaded-ops* (set-remove (*loaded-ops*) operator)))

(define (prune-operators!)
  (unless (flag-set? 'precision 'fallback)
    (for ([op (if (flag-set? 'precision 'double) (*unknown-d-ops*) (*unknown-f-ops*))])
      (operator-remove! op)))
  (unless (flag-set? 'fn 'cbrt) (operator-remove! 'cbrt)))

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
  [fl +] [bf bf+] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (curry format "mpfr_add(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a + ~a")]
  [nonffi +])

(define-operator (+.c complex complex) complex
  [fl +] [bf bf-complex-add] [cost 80]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a + ~a")]
  [nonffi +])

(define-operator (- real [real]) real
  ;; Override the normal argument handling because - can be unary
  [args '(1 2)] [type (hash 1 '(((real) real)) 2 '(((real real) real)))]
  [fl -] [bf bf-] [cost 40]
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (λ (out x [y #f]) (if y (format "mpfr_sub(~a, ~a, ~a, MPFR_RNDN)" out x y) (format "mpfr_neg(~a, ~a, MPFR_RNDN)" out x)))]
  [->tex (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [nonffi -])

(define-operator (neg.c complex) complex
  [fl -] [bf bf-complex-neg] [cost 80]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "-~a")]
  [nonffi -])

(define-operator (-.c complex complex) complex
  [fl -] [bf bf-complex-sub] [cost 80]
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [nonffi -])

(define-operator (* real real) real
  [fl *] [bf bf*] [cost 40]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (curry format "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (*.c complex complex) complex
  [fl *] [bf bf-complex-mult] [cost 320]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (/ real real) real
  [fl /] [bf bf/] [cost 40]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (curry format "mpfr_div(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

(define-operator (/.c complex complex) complex
  [fl /] [bf bf-complex-div] [cost 440]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

(require ffi/unsafe)
(define-syntax (define-operator/libm stx)
  (syntax-case stx (real libm)
    [(_ (operator real ...) real [libm id_d id_f] [key value] ...)
     (let ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))])
       #`(begin
           (define double-proc (get-ffi-obj 'id_d #f (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double)
                                            (lambda ()
                                              (*unknown-d-ops* (cons 'operator (*unknown-d-ops*)))
                                              (λ args (apply (operator-info 'operator 'nonffi) args)))))
           (define float-proc (get-ffi-obj 'id_f #f (_fun #,@(build-list num-args (λ (_) #'_float)) -> _float)
                                           (lambda ()
                                             (*unknown-f-ops* (cons 'operator (*unknown-f-ops*)))
                                             (λ args (apply (operator-info 'operator 'nonffi) args)))))
           (define-operator (operator #,@(build-list num-args (λ (_) #'real))) real
             [fl (λ args (apply (if (flag-set? 'precision 'double) double-proc float-proc) args))]
             [key value] ...)))]))

(define-operator/libm (acos real) real
  [libm acos acosf] [bf bfacos] [cost 90]
  [->c/double (curry format "acos(~a)")]
  [->c/mpfr (curry format "mpfr_acos(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cos^{-1} ~a")]
  [nonffi acos])

(define-operator/libm (acosh real) real
  [libm acosh acoshf] [bf bfacosh] [cost 55]
  [->c/double (curry format "acosh(~a)")]
  [->c/mpfr (curry format "mpfr_acosh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cosh^{-1} ~a")]
  [nonffi acosh])

(define-operator/libm (asin real) real
  [libm asin asinf] [bf bfasin] [cost 105]
  [->c/double (curry format "asin(~a)")]
  [->c/mpfr (curry format "mpfr_asin(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sin^{-1} ~a")]
  [nonffi asin])

(define-operator/libm (asinh real) real
  [libm asinh asinhf] [bf bfasinh] [cost 55]
  [->c/double (curry format "asinh(~a)")]
  [->c/mpfr (curry format "mpfr_asinh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sinh^{-1} ~a")]
  [nonffi asinh])

(define-operator/libm (atan real) real
  [libm atan atanf] [bf bfatan] [cost 105]
  [->c/double (curry format "atan(~a)")]
  [->c/mpfr (curry format "mpfr_atan(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan^{-1} ~a")]
  [nonffi atan])

(define-operator/libm (atan2 real real) real
  [libm atan2 atan2f] [bf bfatan2] [cost 140]
  [->c/double (curry format "atan2(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_atan2(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan^{-1}_* \\frac{~a}{~a}")]
  [nonffi atan])

(define-operator/libm (atanh real) real
  [libm atanh atanhf] [bf bfatanh] [cost 55]
  [->c/double (curry format "atanh(~a)")]
  [->c/mpfr (curry format "mpfr_atanh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tanh^{-1} ~a")]
  [nonffi atanh])

(define-operator/libm (cbrt real) real
  [libm cbrt cbrtf] [bf bfcbrt] [cost 80]
  [->c/double (curry format "cbrt(~a)")]
  [->c/mpfr (curry format "mpfr_cbrt(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt[3]{~a}")]
  [nonffi (λ (x) (expt x (/ 1 3)))])

(define-operator/libm (ceil real) real
  [libm ceil ceilf] [bf bfceiling] [cost 80]
  [->c/double (curry format "ceil(~a)")]
  [->c/mpfr (curry format "mpfr_ceil(~a, ~a)")]
  [->tex (curry format "\\left\\lceil~a\\right\\rceil")]
  [nonffi ceiling])

(define (bfcopysign x y)
  (bf* (bfabs x) (bf (expt -1 (bigfloat-signbit y)))))

(define-operator/libm (copysign real real) real
  [libm copysign copysignf] [bf bfcopysign] [cost 80]
  [->c/double (curry format "copysign(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_copysign(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{copysign}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (if (>= y 0) (abs x) (- (abs x))))])

(define-operator/libm (cos real) real
  [libm cos cosf] [bf bfcos] [cost 60]
  [->c/double (curry format "cos(~a)")]
  [->c/mpfr (curry format "mpfr_cos(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cos ~a")]
  [nonffi cos])

(define-operator/libm (cosh real) real
  [libm cosh coshf] [bf bfcosh] [cost 55]
  [->c/double (curry format "cosh(~a)")]
  [->c/mpfr (curry format "mpfr_cosh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cosh ~a")]
  [nonffi cosh])

(define-operator/libm (erf real) real
  [libm erf erff] [bf bferf] [cost 70]
  [->c/double (curry format "erf(~a)")]
  [->c/mpfr (curry format "mpfr_erf(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{erf} ~a")]
  [nonffi erf])

(define-operator/libm (erfc real) real
  [libm erfc erfcf] [bf bferfc] [cost 70]
  [->c/double (curry format "erfc(~a)")]
  [->c/mpfr (curry format "mpfr_erfc(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{erfc} ~a")]
  [nonffi erfc])

(define-operator/libm (exp real) real
  [libm exp expf] [bf bfexp] [cost 70]
  [->c/double (curry format "exp(~a)")]
  [->c/mpfr (curry format "mpfr_exp(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "e^{~a}")]
  [nonffi exp])

(define-operator (exp.c complex) complex
  [fl exp] [bf bf-complex-exp] [cost 70]
  [->c/double (curry format "exp(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "e^{~a}")]
  [nonffi exp])

(define-operator/libm (exp2 real) real
  [libm exp2 exp2f] [bf bfexp2] [cost 70]
  [->c/double (curry format "exp2(~a)")]
  [->c/mpfr (curry format "mpfr_exp2(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "2^{~a}")]
  [nonffi (λ (x) (expt 2 x))])

(define-operator/libm (expm1 real) real
  [libm expm1 expm1f] [bf bfexpm1] [cost 70]
  [->c/double (curry format "expm1(~a)")]
  [->c/mpfr (curry format "mpfr_expm1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "(e^{~a} - 1)^*")]
  [nonffi (λ (x) (bigfloat->flonum (bfexpm1 (bf x))))])

(define-operator/libm (fabs real) real
  [libm fabs fabsf] [bf bfabs] [cost 40]
  [->c/double (curry format "fabs(~a)")]
  [->c/mpfr (curry format "mpfr_abs(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\left|~a\\right|")]
  [nonffi abs])

(define (bffdim x y)
  (if (bf> x y)
    (bf- x y)
    0.bf))

(define-operator/libm (fdim real real) real
  [libm fdim fdimf] [bf bffdim] [cost 55]
  [->c/double (curry format "fdim(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_dim(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fdim}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (max (- x y) 0))])

(define-operator/libm (floor real) real
  [libm floor floorf] [bf bffloor] [cost 55]
  [->c/double (curry format "floor(~a)")]
  [->c/mpfr (curry format "mpfr_floor(~a, ~a)")]
  [->tex (curry format "\\left\\lfloor~a\\right\\rfloor")]
  [nonffi (λ (x) (floor x))])

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define-operator/libm (fma real real real) real
  [libm fma fmaf] [bf bffma] [cost 55]
  [->c/double (curry format "fma(~a, ~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fma(~a, ~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "(~a \\cdot ~a + ~a)_*")]
  [nonffi (λ (x y z) (bigfloat->flonum (bf+ (bf* (bf x) (bf y)) (bf z))))])

(define-operator/libm (fmax real real) real
  [libm fmax fmaxf] [bf bfmax] [cost 55]
  [->c/double (curry format "fmax(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmax(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fmax}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (max x y)]))])

(define-operator/libm (fmin real real) real
  [libm fmin fminf] [bf bfmin] [cost 55]
  [->c/double (curry format "fmin(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmin(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fmin}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (min x y)]))])

(define (bffmod x mod)
  (bf- x (bf* mod (bffloor (bf/ x mod)))))

(define-operator/libm (fmod real real) real
  [libm fmod fmodf] [bf bffmod] [cost 70]
  [->c/double (curry format "fmod(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmod(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\bmod ~a")]
  [nonffi (λ (x y) (bigfloat->flonum (bffmod (bf x) (bf y))))])

(define-operator/libm (hypot real real) real
  [libm hypot hypotf] [bf bfhypot] [cost 55]
  [->c/double (curry format "hypot(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_hypot(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt{~a^2 + ~a^2}^*")]
  [nonffi (λ (x y) (bigfloat->flonum (bfhypot (bf x) (bf y))))])

(define-operator/libm (j0 real) real
  [libm j0 j0f] [bf bfbesj0] [cost 55]
  [->c/double (curry format "j0(~a)")]
  [->c/mpfr (curry format "mpfr_j0(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{j0} ~a")]
  [nonffi (λ (x) (bigfloat->flonum (bfbesj0 (bf x))))])

(define-operator/libm (j1 real) real
  [libm j1 j1f] [bf bfbesj1] [cost 55]
  [->c/double (curry format "j1(~a)")]
  [->c/mpfr (curry format "mpfr_j1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{j1} ~a")]
  [nonffi (λ (x) (bigfloat->flonum (bfbesj1 (bf x))))])

(define-operator/libm (lgamma real) real
  [libm lgamma lgammaf] [bf bflog-gamma] [cost 55]
  [->c/double (curry format "lgamma(~a)")]
  [->c/mpfr (curry format "mpfr_lngamma(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_* \\left( \\mathsf{gamma} ~a \\right)")]
  [nonffi log-gamma])

(define-operator/libm (log real) real
  [libm log logf] [bf bflog] [cost 70]
  [->c/double (curry format "log(~a)")]
  [->c/mpfr (curry format "mpfr_log(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log ~a")]
  [nonffi log])

(define-operator (log.c complex) complex
  [fl log] [bf bf-complex-log] [cost 265]
  [->c/double (curry format "log(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\log ~a")]
  [nonffi log])

(define-operator/libm (log10 real) real
  [libm log10 log10f] [bf bflog10] [cost 70]
  [->c/double (curry format "log10(~a)")]
  [->c/mpfr (curry format "mpfr_log10(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_{10} ~a")]
  [nonffi (λ (x) (log x 10))])

(define-operator/libm (log1p real) real
  [libm log1p log1pf] [bf bflog1p] [cost 90]
  [->c/double (curry format "log1p(~a)")]
  [->c/mpfr (curry format "mpfr_log1p(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_* (1 + ~a)")]
  [nonffi (λ (x) (bigfloat->flonum (bflog1p (bf x))))])

(define-operator/libm (log2 real) real
  [libm log2 log2f] [bf bflog2] [cost 70]
  [->c/double (curry format "log2(~a)")]
  [->c/mpfr (curry format "mpfr_log2(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_{2} ~a")]
  [nonffi (λ (x) (bigfloat->flonum (bflog2 (bf x))))])

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define-operator/libm (logb real) real
  [libm logb logbf] [bf bflogb] [cost 70]
  [->c/double (curry format "logb(~a)")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_exp(~a), MPFR_RNDN)")]
  [->tex (curry format "\\log^{*}_{b} ~a")]
  [nonffi (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))])

(define-operator/libm (pow real real) real
  [libm pow powf] [bf bfexpt] [cost 210]
  [->c/double (curry format "pow(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_pow(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "{~a}^{~a}")]
  [nonffi expt])

(define-operator (pow.c complex complex) complex
  [fl expt] [bf bf-complex-pow] [cost 210]
  [->c/double (curry format "pow(~a, ~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "{~a}^{~a}")]
  [nonffi expt])

(define-operator/libm (remainder real real) real
  [libm remainder remainderf] [bf bfremainder] [cost 70]
  [->c/double (curry format "remainder(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_remainder(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\mathsf{rem} ~a")]
  [nonffi remainder])

(define-operator/libm (rint real) real
  [libm rint rintf] [bf bfrint] [cost 70]
  [->c/double (curry format "rint(~a)")]
  [->c/mpfr (curry format "mpfr_rint(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{rint} ~a")]
  [nonffi round])

(define-operator/libm (round real) real
  [libm round roundf] [bf bfround] [cost 70]
  [->c/double (curry format "round(~a)")]
  [->c/mpfr (curry format "mpfr_round(~a, ~a)")]
  [->tex (curry format "\\mathsf{round} ~a")]
  [nonffi round])

(define-operator/libm (sin real) real
  [libm sin sinf] [bf bfsin] [cost 60]
  [->c/double (curry format "sin(~a)")]
  [->c/mpfr (curry format "mpfr_sin(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sin ~a")]
  [nonffi sin])

(define-operator/libm (sinh real) real
  [libm sinh sinhf] [bf bfsinh] [cost 55]
  [->c/double (curry format "sinh(~a)")]
  [->c/mpfr (curry format "mpfr_sinh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sinh ~a")]
  [nonffi sinh])

(define-operator/libm (sqrt real) real
  [libm sqrt sqrtf] [bf bfsqrt] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (curry format "mpfr_sqrt(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])

(define-operator (sqrt.c complex) complex
  [fl sqrt] [bf bf-complex-sqrt] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])

(define-operator/libm (tan real) real
  [libm tan tanf] [bf bftan] [cost 95]
  [->c/double (curry format "tan(~a)")]
  [->c/mpfr (curry format "mpfr_tan(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan ~a")]
  [nonffi tan])

(define-operator/libm (tanh real) real
  [libm tanh tanhf] [bf bftanh] [cost 55]
  [->c/double (curry format "tanh(~a)")]
  [->c/mpfr (curry format "mpfr_tanh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tanh ~a")]
  [nonffi tanh])

(define-operator/libm (tgamma real) real
  [libm tgamma tgammaf] [bf bfgamma] [cost 55]
  [->c/double (curry format "tgamma(~a)")]
  [->c/mpfr (curry format "mpfr_gamma(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{gamma} ~a")]
  [nonffi gamma])

(define-operator/libm (trunc real) real
  [libm trunc truncf] [bf bftruncate] [cost 55]
  [->c/double (curry format "trunc(~a)")]
  [->c/mpfr (curry format "mpfr_trunc(~a, ~a)")]
  [->tex (curry format "\\mathsf{trunc} ~a")]
  [nonffi truncate])

(define-operator/libm (y0 real) real
  [libm y0 y0f] [bf bfbesy0] [cost 55]
  [->c/double (curry format "y0(~a)")]
  [->c/mpfr (curry format "mpfr_y0(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{y0} ~a")]
  [nonffi (λ (x) (bigfloat->flonum (bfbesy0 (bf x))))])

(define-operator/libm (y1 real) real
  [libm y1 y1f] [bf bfbesy1] [cost 55]
  [->c/double (curry format "y1(~a)")]
  [->c/mpfr (curry format "mpfr_y1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{y1} ~a")]
  [nonffi (λ (x) (bigfloat->flonum (bfbesy1 (bf x))))])

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
  [fl if-fn] [bf if-fn] [cost 65]
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
  [fl (comparator =)] [bf (comparator bf=)] [cost 65]
  [->c/double (curry format "~a == ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) == 0, MPFR_RNDN)")] ; TODO: cannot handle variary =
  [->tex (infix-joiner " = ")]
  [nonffi (comparator =)])

(define-operator (complex real real) complex
  ; Override number of arguments
  [fl make-rectangular] [bf bigcomplex] [cost 0]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a + ~a i")]
  [nonffi make-rectangular])

(define-operator (re complex) real
  ; Override number of arguments
  [fl real-part] [bf bigcomplex-re] [cost 0]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\Re(~a)")]
  [nonffi real-part])

(define-operator (im complex) real
  ; Override number of arguments
  [fl imag-part] [bf bigcomplex-im] [cost 0]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\Im(~a)")]
  [nonffi imag-part])

(define-operator (conj complex) real
  ; Override number of arguments
  [fl conjugate] [bf bf-complex-conjugate] [cost 0]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\overline{~a}")]
  [nonffi conjugate])

(define-operator (!= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl !=-fn] [bf bf!=-fn] [cost 65]
  [->c/double (curry format "~a != ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) != 0, MPFR_RNDN)")] ; TODO: cannot handle variary !=
  [->tex (infix-joiner " \\ne ")]
  [nonffi !=-fn])

(define-operator (< real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator <)] [bf (comparator bf<)] [cost 65]
  [->c/double (curry format "~a < ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) < 0, MPFR_RNDN)")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator <)])

(define-operator (> real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator >)] [bf (comparator bf>)] [cost 65]
  [->c/double (curry format "~a > ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) > 0, MPFR_RNDN)")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator >)])

(define-operator (<= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator <=)] [bf (comparator bf<=)] [cost 65]
  [->c/double (curry format "~a <= ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) <= 0, MPFR_RNDN)")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator <=)])

(define-operator (>= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator >=)] [bf (comparator bf>=)] [cost 65]
  [->c/double (curry format "~a >= ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) >= 0, MPFR_RNDN)")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator >=)])

(define-operator (not bool) bool
  [fl not] [bf not] [cost 65]
  [->c/double (curry format "!~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, !mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (curry format "\\neg ~a")]
  [nonffi not])

(define-operator (and bool bool) bool
  ; Override number of arguments
  [type #hash((* . (((* bool) bool))))] [args '(*)]
  [fl and-fn] [bf and-fn] [cost 55]
  [->c/double (curry format "~a && ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) && mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (infix-joiner " \\land ")]
  [nonffi and-fn])

(define-operator (or bool bool) bool
  ; Override number of arguments
  [type #hash((* . (((* bool) bool))))] [args '(*)]
  [fl or-fn] [bf or-fn] [cost 55]
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
  #hash([+ . ((+ real real real) (+.c complex complex complex))]
        [- . ((- real real real) (- real real)
              (-.c complex complex complex) (neg.c complex complex))]
        [* . ((* real real real) (*.c complex complex complex))]
        [/ . ((/ real real real) (/.c complex complex complex))]
        [pow . ((pow real real real) (pow.c complex complex complex))]
        [exp . ((exp real real) (exp.c complex complex))]
        [log . ((log real real) (log.c complex complex))]
        [sqrt . ((sqrt real real) (sqrt.c complex complex))]))
