#lang racket

(require math/flonum math/base math/bigfloat math/special-functions)
(require "../common.rkt" "../errors.rkt" "types.rkt" "../biginterval.rkt")

(provide constant? variable? operator? operator-info constant-info
         get-parametric-operator parametric-operators parametric-operators-reverse
         *unknown-d-ops* *unknown-f-ops* *loaded-ops*)

(module+ internals (provide operators constants define-constant define-operator declare-parametric-operator! infix-joiner))

(module+ test (require rackunit))

(define *unknown-d-ops* (make-parameter '()))
(define *unknown-f-ops* (make-parameter '()))

(define *loaded-ops* (make-parameter '()))

;; Constants's values are defined as functions to allow them to
;; depend on (bf-precision) and (flag 'precision 'double).

(define-table constants
  [type type?]
  [bf (->* () bigvalue?)]
  [fl (->* () value?)]
  [ival (or/c (->* () ival?) #f)]
  [nonffi (->* () value?)]
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
  [->tex "\\pi"])

(define-constant E real
  [bf (λ () (bfexp 1.bf))]
  [fl (λ () (exp 1.0))]
  [ival ival-e]
  [nonffi (λ () (exp 1.0))]
  [->tex "e"])

(define-constant INFINITY real
  [bf (λ () +inf.bf)]
  [fl (λ () +inf.0)]
  [ival (λ () (mk-ival +inf.bf))]
  [nonffi (λ () +inf.0)]
  [->tex "\\infty"])

(define-constant NAN real
  [bf (λ () +nan.bf)]
  [fl (λ () +nan.0)]
  [ival (λ () (mk-ival +nan.bf))]
  [nonffi (λ () +nan.0)]
  [->tex "\\mathsf{NaN}"])

(define-constant TRUE bool
  [bf (const true)]
  [fl (const true)]
  [nonffi (const true)]
  [ival (λ () (ival-bool true))]
  [->tex "\\top"])

(define-constant FALSE bool
  [bf (const false)]
  [fl (const false)]
  [nonffi (const false)]
  [ival (λ () (ival-bool false))]
  [->tex "\\perp"])

;; TODO: The contracts for operators are tricky because the number of arguments is unknown
;; There's no easy way to write such a contract in Racket, so I only constrain the output type.
(define (unconstrained-argument-number-> from/c to/c)
  (unconstrained-domain-> to/c))

(define-table operators
  [itype (or/c (listof type?) type?)]
  [otype type?]
  [bf    (unconstrained-argument-number-> bigvalue? bigvalue?)]
  [fl    (unconstrained-argument-number-> value? value?)]
  [nonffi (unconstrained-argument-number-> value? value?)]
  [ival (or/c #f (unconstrained-argument-number-> ival? ival?))]
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
  (begin
    (*loaded-ops* (cons 'operator (*loaded-ops*)))
    (table-set! operators 'operator
                (make-hash (list (cons 'itype '(atypes ...)) (cons 'otype 'rtype)
                                 (cons 'key value) ...)))))

(define (no-complex fun)
  (λ xs
     (define res (apply fun xs))
     (if (real? res)
       res
       +nan.0)))

(define (default-nonffi . args)
  (raise
   (make-exn:fail:unsupported
    (format "couldn't find ~a and no default implementation defined" 'operator)
    (current-continuation-marks))))

(define-operator (+ real real) real 
  [fl +] [bf bf+] [ival ival-add]
  [->tex (curry format "~a + ~a")]
  [nonffi +])

(define-operator (- real real) real
  [fl -] [bf bf-] [ival ival-sub]
  [->tex (curry format "~a - ~a")]
  [nonffi -])

(define-operator (neg real) real
  [fl -] [bf bf-] [ival ival-neg]
  [->tex (curry format "-~a")]
  [nonffi -])

(define-operator (* real real) real
  [fl *] [bf bf*] [ival ival-mult]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (/ real real) real
  [fl /] [bf bf/] [ival ival-div]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

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
  [libm acos acosf] [bf bfacos] [ival ival-acos]
  [->tex (curry format "\\cos^{-1} ~a")]
  [nonffi (no-complex acos)])

(define-operator/libm (acosh real) real
  [libm acosh acoshf] [bf bfacosh] [ival ival-acosh]
  [->tex (curry format "\\cosh^{-1} ~a")]
  [nonffi (no-complex acosh)])

(define-operator/libm (asin real) real
  [libm asin asinf] [bf bfasin] [ival ival-asin]
  [->tex (curry format "\\sin^{-1} ~a")]
  [nonffi (no-complex asin)])

(define-operator/libm (asinh real) real
  [libm asinh asinhf] [bf bfasinh] [ival ival-asinh]
  [->tex (curry format "\\sinh^{-1} ~a")]
  [nonffi (no-complex asinh)])

(define-operator/libm (atan real) real
  [libm atan atanf] [bf bfatan] [ival ival-atan]
  [->tex (curry format "\\tan^{-1} ~a")]
  [nonffi (no-complex atan)])

(define-operator/libm (atan2 real real) real
  [libm atan2 atan2f] [bf bfatan2] [ival ival-atan2]
  [->tex (curry format "\\tan^{-1}_* \\frac{~a}{~a}")]
  [nonffi (no-complex atan)])

(define-operator/libm (atanh real) real
  [libm atanh atanhf] [bf bfatanh] [ival ival-atanh]
  [->tex (curry format "\\tanh^{-1} ~a")]
  [nonffi (no-complex atanh)])

(define-operator/libm (cbrt real) real
  [libm cbrt cbrtf] [bf bfcbrt] [ival ival-cbrt]
  [->tex (curry format "\\sqrt[3]{~a}")]
  [nonffi (no-complex (λ (x) (expt x (/ 1 3))))])

(define-operator/libm (ceil real) real
  [libm ceil ceilf] [bf bfceiling] [ival ival-ceil]
  [->tex (curry format "\\left\\lceil~a\\right\\rceil")]
  [nonffi ceiling])

(define (bfcopysign x y)
  (bf* (bfabs x) (bf (expt -1 (bigfloat-signbit y)))))

(define-operator/libm (copysign real real) real
  [libm copysign copysignf] [bf bfcopysign] [ival ival-copysign]
  [->tex (curry format "\\mathsf{copysign}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (if (>= y 0) (abs x) (- (abs x))))])

(define-operator/libm (cos real) real
  [libm cos cosf] [bf bfcos] [ival ival-cos]
  [->tex (curry format "\\cos ~a")]
  [nonffi cos])

(define-operator/libm (cosh real) real
  [libm cosh coshf] [bf bfcosh] [ival ival-cosh]
  [->tex (curry format "\\cosh ~a")]
  [nonffi cosh])

(define-operator/libm (erf real) real
  [libm erf erff] [bf bferf] [ival ival-erf]
  [->tex (curry format "\\mathsf{erf}\\left(~a\\right)")]
  [nonffi (no-complex erf)])

(define-operator/libm (erfc real) real
  [libm erfc erfcf] [bf bferfc] [ival ival-erfc]
  [->tex (curry format "\\mathsf{erfc}\\left(~a\\right)")]
  [nonffi erfc])

(define-operator/libm (exp real) real
  [libm exp expf] [bf bfexp] [ival ival-exp]
  [->tex (curry format "e^{~a}")]
  [nonffi exp])

(define-operator/libm (exp2 real) real
  [libm exp2 exp2f] [bf bfexp2] [ival ival-exp2]
  [->tex (curry format "2^{~a}")]
  [nonffi (no-complex (λ (x) (expt 2 x)))])

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define-operator/libm (expm1 real) real
  [libm expm1 expm1f] [bf bfexpm1] [ival ival-expm1]
  [->tex (curry format "\\mathsf{expm1}\\left(~a\\right)")]
  [nonffi (from-bigfloat bfexpm1)])

(define-operator/libm (fabs real) real
  [libm fabs fabsf] [bf bfabs] [ival ival-fabs]
  [->tex (curry format "\\left|~a\\right|")]
  [nonffi abs])

(define (bffdim x y)
  (if (bf> x y)
    (bf- x y)
    0.bf))

(define-operator/libm (fdim real real) real
  [libm fdim fdimf] [bf bffdim] [ival ival-fdim]
  [->tex (curry format "\\mathsf{fdim}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (max (- x y) 0))])

(define-operator/libm (floor real) real
  [libm floor floorf] [bf bffloor] [ival ival-floor]
  [->tex (curry format "\\left\\lfloor~a\\right\\rfloor")]
  [nonffi (λ (x) (floor x))])

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define-operator/libm (fma real real real) real
  [libm fma fmaf] [bf bffma] [ival ival-fma]
  [->tex (curry format "\\mathsf{fma}\\left(~a, ~a, ~a\\right)")]
  [nonffi (λ (x y z) (bigfloat->flonum (bf+ (bf* (bf x) (bf y)) (bf z))))])

(define-operator/libm (fmax real real) real
  [libm fmax fmaxf] [bf bfmax] [ival ival-fmax]
  [->tex (curry format "\\mathsf{max}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (max x y)]))])

(define-operator/libm (fmin real real) real
  [libm fmin fminf] [bf bfmin] [ival ival-fmin]
  [->tex (curry format "\\mathsf{min}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (min x y)]))])

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define-operator/libm (fmod real real) real
  [libm fmod fmodf] [bf bffmod] [ival ival-fmod]
  [->tex (curry format "~a \\bmod ~a")]
  [nonffi (from-bigfloat bffmod)])

(define-operator/libm (hypot real real) real
  [libm hypot hypotf] [bf bfhypot] [ival ival-hypot]
  [->tex (curry format "\\mathsf{hypot}\\left(~a, ~a\\right)")]
  [nonffi (from-bigfloat bfhypot)])

(define-operator/libm (j0 real) real
  [libm j0 j0f] [bf bfbesj0] [ival #f]
  [->tex (curry format "j_0\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesj0)])

(define-operator/libm (j1 real) real
  [libm j1 j1f] [bf bfbesj1] [ival #f]
  [->tex (curry format "j_1\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesj1)])

(define-operator/libm (lgamma real) real
  [libm lgamma lgammaf] [bf bflog-gamma] [ival #f]
  [->tex (curry format "\\mathsf{lgamma} \\left( ~a \\right)")]
  [nonffi log-gamma])

(define-operator/libm (log real) real
  [libm log logf] [bf bflog] [ival ival-log]
  [->tex (curry format "\\log ~a")]
  [nonffi (no-complex log)])

(define-operator/libm (log10 real) real
  [libm log10 log10f] [bf bflog10] [ival ival-log10]
  [->tex (curry format "\\log_{10} ~a")]
  [nonffi (no-complex (λ (x) (log x 10)))])

(define-operator/libm (log1p real) real
  [libm log1p log1pf] [bf bflog1p] [ival ival-log1p]
  [->tex (curry format "\\mathsf{log1p}\\left(~a\\right)")]
  [nonffi (from-bigfloat bflog1p)])

(define-operator/libm (log2 real) real
  [libm log2 log2f] [bf bflog2] [ival ival-log2]
  [->tex (curry format "\\log_{2} ~a")]
  [nonffi (from-bigfloat bflog2)])

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define-operator/libm (logb real) real
  [libm logb logbf] [bf bflogb] [ival ival-logb]
  [->tex (curry format "\\log_{b} ~a")]
  [nonffi (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))])

(define-operator/libm (pow real real) real
  [libm pow powf] [bf bfexpt] [ival ival-pow]
  [->tex (curry format "{~a}^{~a}")]
  [nonffi (no-complex expt)])

(define (bfremainder x mod)
  (bf- x (bf* (bfround (bf/ x mod)) mod)))

(define-operator/libm (remainder real real) real
  [libm remainder remainderf] [bf bfremainder] [ival ival-remainder] 
  [->tex (curry format "~a \\mathsf{rem} ~a")]
  [nonffi remainder])

(define-operator/libm (rint real) real
  [libm rint rintf] [bf bfrint] [ival ival-rint]
  [->tex (curry format "\\mathsf{rint} ~a")]
  [nonffi round])

(define-operator/libm (round real) real
  [libm round roundf] [bf bfround] [ival ival-round]
  [->tex (curry format "\\mathsf{round} ~a")]
  [nonffi round])

(define-operator/libm (sin real) real
  [libm sin sinf] [bf bfsin] [ival ival-sin]
  [->tex (curry format "\\sin ~a")]
  [nonffi sin])

(define-operator/libm (sinh real) real
  [libm sinh sinhf] [bf bfsinh] [ival ival-sinh]
  [->tex (curry format "\\sinh ~a")]
  [nonffi sinh])

(define-operator/libm (sqrt real) real
  [libm sqrt sqrtf] [bf bfsqrt] [ival ival-sqrt]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi (no-complex sqrt)])

(define-operator/libm (tan real) real
  [libm tan tanf] [bf bftan] [ival ival-tan]
  [->tex (curry format "\\tan ~a")]
  [nonffi tan])

(define-operator/libm (tanh real) real
  [libm tanh tanhf] [bf bftanh] [ival ival-tanh]
  [->tex (curry format "\\tanh ~a")]
  [nonffi tanh])

(define-operator/libm (tgamma real) real
  [libm tgamma tgammaf] [bf bfgamma] [ival #f]
  [->tex (curry format "\\Gamma\\left(~a\\right)")]
  [nonffi gamma])

(define-operator/libm (trunc real) real
  [libm trunc truncf] [bf bftruncate] [ival ival-trunc]
  [->tex (curry format "\\mathsf{trunc}\\left(~a\\right)")]
  [nonffi truncate])

(define-operator/libm (y0 real) real
  [libm y0 y0f] [bf bfbesy0] [ival #f] 
  [->tex (curry format "y_0\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesy0)])

(define-operator/libm (y1 real) real
  [libm y1 y1f] [bf bfbesy1] [ival #f]
  [->tex (curry format "y_1\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesy1)])

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

(define-operator (if bool real real) real ; types not used, special cased in type checker
  [fl if-fn] [bf if-fn] [ival ival-if]
  [->tex (curry format "~a ? ~a : ~a")]
  [nonffi if-fn])

(define ((infix-joiner x) . args)
  (string-join args x))

(define-operator (== real real) bool
  [itype 'real] [otype 'bool] ; Override number of arguments
  [fl (comparator =)] [bf (comparator bf=)] [ival ival-==]
  [->tex (infix-joiner " = ")]
  [nonffi (comparator =)])

(define-operator (!= real real) bool
  [itype 'real] [otype 'bool] ; Override number of arguments
  [fl !=-fn] [bf bf!=-fn] [ival ival-!=]
  [->tex (infix-joiner " \\ne ")]
  [nonffi !=-fn])

(define-operator (< real real) bool
  [itype 'real] [otype 'bool] ; Override number of arguments
  [fl (comparator <)] [bf (comparator bf<)] [ival ival-<]
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator <)])

(define-operator (> real real) bool
  [itype 'real] [otype 'bool] ; Override number of arguments
  [fl (comparator >)] [bf (comparator bf>)] [ival ival->]
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator >)])

(define-operator (<= real real) bool
  [itype 'real] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)] [bf (comparator bf<=)] [ival ival-<=]
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator <=)])

(define-operator (>= real real) bool
  [itype 'real] [otype 'bool] ; Override number of arguments
  [fl (comparator >=)] [bf (comparator bf>=)] [ival ival->=]
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator >=)])

(define-operator (not bool) bool
  [fl not] [bf not] [ival ival-not]
  [->tex (curry format "\\neg ~a")]
  [nonffi not])

(define-operator (and bool bool) bool
  [itype 'bool] [otype 'bool] ; Override number of arguments
  [fl and-fn] [bf and-fn] [ival ival-and]
  [->tex (infix-joiner " \\land ")]
  [nonffi and-fn])

(define-operator (or bool bool) bool
  [itype 'bool] [otype 'bool] ; Override number of arguments
  [fl or-fn] [bf or-fn] [ival ival-or]
  [->tex (infix-joiner " \\lor ")]
  [nonffi or-fn])

(define (operator? op)
  (and (symbol? op) (dict-has-key? (cdr operators) op)))

(define (constant? var)
  (or (value? var) (and (symbol? var) (dict-has-key? (cdr constants) var))))

(define (variable? var)
  (and (symbol? var) (not (constant? var))))

(define parametric-operators (make-hash))

(define (get-parametric-operator name actual-types)
  (for/or ([sig (hash-ref parametric-operators name)])
    (match-define (list* true-name rtype atypes) sig)
    (and
     (if (symbol? atypes)
         (andmap (curry equal? atypes) actual-types)
         (equal? atypes actual-types))
     (cons true-name rtype))))

(define (declare-parametric-operator! name op inputs output)
  (hash-update! parametric-operators name (curry cons (list* op output inputs)) '()))

(declare-parametric-operator! '+ '+ '(real real) 'real)
(declare-parametric-operator! '- '- '(real real) 'real)
(declare-parametric-operator! '- 'neg '(real) 'real)
(declare-parametric-operator! '* '* '(real real) 'real)
(declare-parametric-operator! '/ '/ '(real real) 'real)
(declare-parametric-operator! 'pow 'pow '(real real) 'real)
(declare-parametric-operator! 'exp 'exp '(real) 'real)
(declare-parametric-operator! 'log 'log '(real) 'real)
(declare-parametric-operator! 'sqrt 'sqrt '(real) 'real)
(declare-parametric-operator! '<  '<  'real 'bool)
(declare-parametric-operator! '<= '<= 'real 'bool)
(declare-parametric-operator! '>  '>  'real 'bool)
(declare-parametric-operator! '>= '>= 'real 'bool)
(declare-parametric-operator! '== '== 'real 'bool)
(declare-parametric-operator! '!= '!= 'real 'bool)

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
