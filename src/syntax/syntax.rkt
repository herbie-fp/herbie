#lang racket

(require math/flonum)
(require math/bigfloat)
(require "../common.rkt")

(provide predicates constant? variable? operation?
         ->bf ->flonum
         program-body program-variables
         operator-info constant-info)

(define (type? x) (or (equal? x 'real) (equal? x 'bool)))

; Programs are just lambda expressions

(define program-body caddr)
(define program-variables cadr)

;; Constants's values are defined as functions to allow them to depend on (bf-precision) and (flag 'precision 'double).

(define-table* constants
  [type type?]
  [bf (->* () bigfloat?)]
  [fl (->* () flonum?)]
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
(define-table* operators
  [args  (listof (or/c '* natural-number/c))]
  [bf    (unconstrained-argument-number-> (or/c bigfloat? boolean?) (or/c bigfloat? boolean?))]
  [fl    (unconstrained-argument-number-> (or/c flonum? boolean?) (or/c flonum? boolean?))]
  [cost  natural-number/c]
  [type  (hash/c (or/c '* natural-number/c) (list/c (or/c (listof type?) (list/c '* type?)) type?))]
  [->c/double (unconstrained-argument-number-> string? string?)]
  [->c/mpfr   (unconstrained-argument-number-> string? string?)]
  [->tex      (unconstrained-argument-number-> string? string?)])

(define (operator-info operator field) (table-ref operators operator field))

(define-syntax-rule (define-operator (operator atypes ...) rtype [key value] ...)
  (let ([type (hash (length '(atypes ...)) (list '(atypes ...) 'rtype))]
        [args (list (length '(atypes ...)))])
    (table-set! operators 'operator
                (make-hash (list (cons 'type type) (cons 'args args) (cons 'key value) ...)))))

(define-operator (+ real real) real
  [fl +] [bf bf+] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (curry format "mpfr_add(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a + ~a")])

(define-operator (- real [real]) real
  ;; Override the normal argument handling because - can be unary
  [args '(1 2)] [type (hash 1 '((real) real) 2 '((real real) real))]
  [fl -] [bf bf-] [cost 40]
  [->c/double (curry format "~a - ~a")]
  [->c/mpfr (curry format "mpfr_sub(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a - ~a")])

(define-operator (* real real) real
  [fl *] [bf bf*] [cost 40]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (curry format "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\cdot ~a")])

(define-operator (/ real real) real
  [fl /] [bf bf/] [cost 40]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (curry format "mpfr_div(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\frac{~a}{~a}")])

; Use C ffi to get numerical ops from libm
(require ffi/unsafe ffi/unsafe/define)
(define-ffi-definer define-libm #f
  #:default-make-fail make-not-available)

(define-syntax (define-operator/libm stx)
  (syntax-case stx (real libm)
    [(_ (operator real ...) real [libm id_d id_f] [key value] ...)
     (let ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))])
       #`(begin
           (define-libm id_d (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double))
           (define-libm id_f (_fun #,@(build-list num-args (λ (_) #'_float)) -> _float))
           (define-operator (operator #,@(build-list num-args (λ (_) #'real))) real
             [fl (λ args (apply ((flag 'precision 'double) id_d id_f) args))]
             [key value] ...)))]))

(define-operator/libm (acos real) real
  [libm acos acosf] [bf bfacos] [cost 90]
  [->c/double (curry format "acos(~a)")]
  [->c/mpfr (curry format "mpfr_acos(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cos^{-1} ~a")])

(define-operator/libm (acosh real) real
  [libm acosh acoshf] [bf bfacosh] [cost 55]
  [->c/double (curry format "acosh(~a)")]
  [->c/mpfr (curry format "mpfr_acosh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cosh^{-1} ~a")])

(define-operator/libm (asin real) real
  [libm asin asinf] [bf bfasin] [cost 105]
  [->c/double (curry format "asin(~a)")]
  [->c/mpfr (curry format "mpfr_asin(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sin^{-1} ~a")])

(define-operator/libm (asinh real) real
  [libm asinh asinhf] [bf bfasinh] [cost 55]
  [->c/double (curry format "asinh(~a)")]
  [->c/mpfr (curry format "mpfr_asinh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sinh^{-1} ~a")])

(define-operator/libm (atan real) real
  [libm atan atanf] [bf bfatan] [cost 105]
  [->c/double (curry format "atan(~a)")]
  [->c/mpfr (curry format "mpfr_atan(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan^{-1} ~a")])

(define-operator/libm (atan2 real real) real
  [libm atan2 atan2f] [bf bfatan2] [cost 140]
  [->c/double (curry format "atan2(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_atan2(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan^{-1}_* \\frac{~a}{~a}")])

(define-operator/libm (atanh real) real
  [libm atanh atanhf] [bf bfatanh] [cost 55]
  [->c/double (curry format "atanh(~a)")]
  [->c/mpfr (curry format "mpfr_atanh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tanh^{-1} ~a")])

(define-operator/libm (cbrt real) real
  [libm cbrt cbrtf] [bf bfcbrt] [cost 80]
  [->c/double (curry format "cbrt(~a)")]
  [->c/mpfr (curry format "mpfr_cbrt(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt[3]{~a}")])

(define-operator/libm (ceil real) real
  [libm ceil ceilf] [bf bfceiling] [cost 80]
  [->c/double (curry format "ceil(~a)")]
  [->c/mpfr (curry format "mpfr_ceil(~a, ~a)")]
  [->tex (curry format "\\left\\lceil~a\\right\\rceil")])

(define (bfcopysign x y)
  (bf* (bfabs x) (bf (expt -1 (bigfloat-signbit y)))))

(define-operator/libm (copysign real real) real
  [libm copysign copysignf] [bf bfcopysign] [cost 80]
  [->c/double (curry format "copysign(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_copysign(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{copysign}\\left(~a, ~a\\right)")])

(define-operator/libm (cos real) real
  [libm cos cosf] [bf bfcos] [cost 60]
  [->c/double (curry format "cos(~a)")]
  [->c/mpfr (curry format "mpfr_cos(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cos ~a")])

(define-operator/libm (cosh real) real
  [libm cosh coshf] [bf bfcosh] [cost 55]
  [->c/double (curry format "cosh(~a)")]
  [->c/mpfr (curry format "mpfr_cosh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cosh ~a")])

(define-operator/libm (erf real) real
  [libm erf erff] [bf bferf] [cost 70]
  [->c/double (curry format "erf(~a)")]
  [->c/mpfr (curry format "mpfr_erf(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{erf} ~a")])

(define-operator/libm (erfc real) real
  [libm erfc erfcf] [bf bferfc] [cost 70]
  [->c/double (curry format "erfc(~a)")]
  [->c/mpfr (curry format "mpfr_erfc(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{erfc} ~a")])

(define-operator/libm (exp real) real
  [libm exp expf] [bf bfexp] [cost 70]
  [->c/double (curry format "exp(~a)")]
  [->c/mpfr (curry format "mpfr_exp(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "e^{~a}")])

(define-operator/libm (exp2 real) real
  [libm exp2 exp2f] [bf bfexp2] [cost 70]
  [->c/double (curry format "exp2(~a)")]
  [->c/mpfr (curry format "mpfr_exp2(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "2^{~a}")])

(define-operator/libm (expm1 real) real
  [libm expm1 expm1f] [bf bfexpm1] [cost 70]
  [->c/double (curry format "expm1(~a)")]
  [->c/mpfr (curry format "mpfr_expm1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "(e^{~a} - 1)^*")])

(define-operator/libm (fabs real) real
  [libm fabs fabsf] [bf bfabs] [cost 40]
  [->c/double (curry format "fabs(~a)")]
  [->c/mpfr (curry format "mpfr_abs(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\left|~a\\right|")])

(define (bffdim x y)
  (if (bf> x y)
    (bf- x y)
    0.bf))

(define-operator/libm (fdim real real) real
  [libm fdim fdimf] [bf bffdim] [cost 55]
  [->c/double (curry format "fdim(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_dim(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fdim}\\left(~a, ~a\\right)")])

(define-operator/libm (floor real) real
  [libm floor floorf] [bf bffloor] [cost 55]
  [->c/double (curry format "floor(~a)")]
  [->c/mpfr (curry format "mpfr_floor(~a, ~a)")]
  [->tex (curry format "\\left\\lfloor~a\\right\\rfloor")])

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define-operator/libm (fma real real real) real
  [libm fma fmaf] [bf bffma] [cost 55]
  [->c/double (curry format "fma(~a, ~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fma(~a, ~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "(~a \\cdot ~a + ~a)_*")])

(define-operator/libm (fmax real real) real
  [libm fmax fmaxf] [bf bfmax] [cost 55]
  [->c/double (curry format "fmax(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmax(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fmax}\\left(~a, ~a\\right)")])

(define-operator/libm (fmin real real) real
  [libm fmin fminf] [bf bfmin] [cost 55]
  [->c/double (curry format "fmin(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmin(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fmin}\\left(~a, ~a\\right)")])

(define (bffmod x mod)
  (bf- x (bf* mod (bffloor (bf/ x mod)))))

(define-operator/libm (fmod real real) real
  [libm fmod fmodf] [bf bffmod] [cost 70]
  [->c/double (curry format "fmod(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmod(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\bmod ~a")])

(define-operator/libm (hypot real real) real
  [libm hypot hypotf] [bf bfhypot] [cost 55]
  [->c/double (curry format "hypot(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_hypot(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt{~a^2 + ~a^2}^*")])

(define-operator/libm (j0 real) real
  [libm j0 j0f] [bf bfbesj0] [cost 55]
  [->c/double (curry format "j0(~a)")]
  [->c/mpfr (curry format "mpfr_j0(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{j0} ~a")])

(define-operator/libm (j1 real) real
  [libm j1 j1f] [bf bfbesj1] [cost 55]
  [->c/double (curry format "j1(~a)")]
  [->c/mpfr (curry format "mpfr_j1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{j1} ~a")])

(define-operator/libm (lgamma real) real
  [libm lgamma lgammaf] [bf bflog-gamma] [cost 55]
  [->c/double (curry format "lgamma(~a)")]
  [->c/mpfr (curry format "mpfr_lngamma(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_* \\left( \\mathsf{gamma} ~a \\right)")])

(define-operator/libm (log real) real
  [libm log logf] [bf bflog] [cost 70]
  [->c/double (curry format "log(~a)")]
  [->c/mpfr (curry format "mpfr_log(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log ~a")])

(define-operator/libm (log10 real) real
  [libm log10 log10f] [bf bflog10] [cost 70]
  [->c/double (curry format "log10(~a)")]
  [->c/mpfr (curry format "mpfr_log10(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_{10} ~a")])

(define-operator/libm (log1p real) real
  [libm log1p log1pf] [bf bflog1p] [cost 90]
  [->c/double (curry format "log1p(~a)")]
  [->c/mpfr (curry format "mpfr_log1p(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_* (1 + ~a)")])

(define-operator/libm (log2 real) real
  [libm log2 log2f] [bf bflog2] [cost 70]
  [->c/double (curry format "log2(~a)")]
  [->c/mpfr (curry format "mpfr_log2(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_{2} ~a")])

(define (bflogb x)
  (bigfloat-exponent x))

(define-operator/libm (logb real) real
  [libm logb logbf] [bf bflogb] [cost 70]
  [->c/double (curry format "logb(~a)")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_exp(~a), MPFR_RNDN)")]
  [->tex (curry format "\\log^{*}_{b} ~a")])

(define-operator/libm (pow real real) real
  [libm pow powf] [bf bfexpt] [cost 210]
  [->c/double (curry format "pow(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_pow(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "{~a}^{~a}")])

(define-operator/libm (remainder real real) real
  [libm remainder remainderf] [bf bfremainder] [cost 70]
  [->c/double (curry format "remainder(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_remainder(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\mathsf{rem} ~a")])

(define-operator/libm (rint real) real
  [libm rint rintf] [bf bfrint] [cost 70]
  [->c/double (curry format "rint(~a)")]
  [->c/mpfr (curry format "mpfr_rint(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{rint} ~a")])

(define-operator/libm (round real) real
  [libm round roundf] [bf bfround] [cost 70]
  [->c/double (curry format "round(~a)")]
  [->c/mpfr (curry format "mpfr_round(~a, ~a)")]
  [->tex (curry format "\\mathsf{round} ~a")])

(define-operator/libm (sin real) real
  [libm sin sinf] [bf bfsin] [cost 60]
  [->c/double (curry format "sin(~a)")]
  [->c/mpfr (curry format "mpfr_sin(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sin ~a")])

(define-operator/libm (sinh real) real
  [libm sinh sinhf] [bf bfsinh] [cost 55]
  [->c/double (curry format "sinh(~a)")]
  [->c/mpfr (curry format "mpfr_sinh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sinh ~a")])

(define-operator/libm (sqrt real) real
  [libm sqrt sqrtf] [bf bfsqrt] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (curry format "mpfr_sqrt(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt{~a}")])

(define-operator/libm (tan real) real
  [libm tan tanf] [bf bftan] [cost 95]
  [->c/double (curry format "tan(~a)")]
  [->c/mpfr (curry format "mpfr_tan(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan ~a")])

(define-operator/libm (tanh real) real
  [libm tanh tanhf] [bf bftanh] [cost 55]
  [->c/double (curry format "tanh(~a)")]
  [->c/mpfr (curry format "mpfr_tanh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tanh ~a")])

(define-operator/libm (tgamma real) real
  [libm tgamma tgammaf] [bf bfgamma] [cost 55]
  [->c/double (curry format "tgamma(~a)")]
  [->c/mpfr (curry format "mpfr_gamma(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{gamma} ~a")])

(define-operator/libm (trunc real) real
  [libm trunc truncf] [bf bftruncate] [cost 55]
  [->c/double (curry format "trunc(~a)")]
  [->c/mpfr (curry format "mpfr_trunc(~a, ~a)")]
  [->tex (curry format "\\mathsf{trunc} ~a")])

(define-operator/libm (y0 real) real
  [libm y0 y0f] [bf bfbesy0] [cost 55]
  [->c/double (curry format "y0(~a)")]
  [->c/mpfr (curry format "mpfr_y0(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{y0} ~a")])

(define-operator/libm (y1 real) real
  [libm y1 y1f] [bf bfbesy1] [cost 55]
  [->c/double (curry format "y1(~a)")]
  [->c/mpfr (curry format "mpfr_y1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{y1} ~a")])

(define-operator (sqr real real) real
  [fl sqr] [bf bfsqr] [cost 40]
  [->c/double (λ (x) (format "~a * ~a" x x))]
  [->c/mpfr (curry format "mpfr_sqr(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "{~a}^2")])

(define-operator (cube real real) real
  [fl (λ (x) (* x (* x x)))] [bf (λ (x) (bf* x (bf* x x)))] [cost 80]
  [->c/double (λ (x) (format "~a * (~a * ~a)" x x x))]
  [->c/mpfr (λ (out x) (format "mpfr_sqr(~a, ~a, MPFR_RNDN); mpfr_mul(~a, ~a, ~a, MPFR_RNDN)" out x out out x))]
  [->tex (curry format "{~a}^3")])

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
     (format "if (mpfr_get_si(~a, MPFR_RNDN)) { mpfr_set(~a, ~a, MPFR_RNDN); } else { mpfr_set(~a, ~a, MPFR_RNDN); }")
     c out a out b)]
  [->tex (curry format "~a ? ~a : ~a")])

(define-operator (== real real) bool
  ; Override number of arguments
  [type #hash((* . ((* real) bool)))] [args '(*)]
  [fl (comparator =)] [bf (comparator bf=)] [cost 65]
  [->c/double (curry format "~a == ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) == 0, MPFR_RNDN)")] ; TODO: cannot handle variary =
  [->tex (curryr string-join " = ")])

(define-operator (!= real real) bool
  ; Override number of arguments
  [type #hash((* . ((* real) bool)))] [args '(*)]
  [fl !=-fn] [bf bf!=-fn] [cost 65]
  [->c/double (curry format "~a != ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) != 0, MPFR_RNDN)")] ; TODO: cannot handle variary !=
  [->tex (curryr string-join " \\ne ")])

(define-operator (< real real) bool
  ; Override number of arguments
  [type #hash((* . ((* real) bool)))] [args '(*)]
  [fl (comparator <)] [bf (comparator bf<)] [cost 65]
  [->c/double (curry format "~a < ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) < 0, MPFR_RNDN)")] ; TODO: cannot handle variary <
  [->tex (curryr string-join " < ")])

(define-operator (> real real) bool
  ; Override number of arguments
  [type #hash((* . ((* real) bool)))] [args '(*)]
  [fl (comparator >)] [bf (comparator bf>)] [cost 65]
  [->c/double (curry format "~a > ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) > 0, MPFR_RNDN)")] ; TODO: cannot handle variary >
  [->tex (curryr string-join " > ")])

(define-operator (<= real real) bool
  ; Override number of arguments
  [type #hash((* . ((* real) bool)))] [args '(*)]
  [fl (comparator <=)] [bf (comparator bf<=)] [cost 65]
  [->c/double (curry format "~a <= ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) <= 0, MPFR_RNDN)")] ; TODO: cannot handle variary <=
  [->tex (curryr string-join " \\le ")])

(define-operator (>= real real) bool
  ; Override number of arguments
  [type #hash((* . ((* real) bool)))] [args '(*)]
  [fl (comparator >=)] [bf (comparator bf>=)] [cost 65]
  [->c/double (curry format "~a >= ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) >= 0, MPFR_RNDN)")] ; TODO: cannot handle variary >=
  [->tex (curryr string-join " \\ge ")])

(define-operator (not bool) bool
  [fl not] [bf not] [cost 65]
  [->c/double (curry format "!~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, !mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (curry format "\\neg ~a")])

(define-operator (and bool bool) bool
  ; Override number of arguments
  [type #hash((* . ((* bool) bool)))] [args '(*)]
  [fl and-fn] [bf and-fn] [cost 55]
  [->c/double (curry format "~a && ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) && mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (curryr string-join " \\land ")])

(define-operator (or bool bool) bool
  ; Override number of arguments
  [type #hash((* . ((* bool) bool)))] [args '(*)]
  [fl or-fn] [bf or-fn] [cost 55]
  [->c/double (curry format "~a || ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) || mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (curryr string-join " \\lor ")])

(define predicates '(not or and < > <= >= == !=))

(define (operation? op)
  (dict-has-key? (cdr operators) op))

(define (constant? var)
  (or (number? var) (dict-has-key? (cdr constants var))))

(define (variable? var)
  (and (symbol? var) (not (constant? var))))

(define (->flonum x)
  (define convert
    ((flag 'precision 'double) real->double-flonum real->single-flonum))

  (cond
   [(real? x) (convert x)]
   [(bigfloat? x) (convert (bigfloat->flonum x))]
   [(complex? x)
    (if (= (imag-part x) 0)
        (->flonum (real-part x))
        +nan.0)]
   [(constant? x)
    (convert ((constant-info x 'fl)))]
   [else x]))

(define (->bf x)
  (cond
   [(real? x) (bf x)]
   [(bigfloat? x) x]
   [(complex? x)
    (if (= (imag-part x) 0) (->bf (real-part x)) +nan.bf)]
   [(constant? x) ((constant-info x 'bf))]
   [else x]))
