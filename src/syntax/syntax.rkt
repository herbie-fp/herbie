#lang racket

(require math/flonum math/base math/bigfloat math/special-functions rival)
(require "../common.rkt" "../interface.rkt" "../errors.rkt" "../float32.rkt" "types.rkt")

(provide constant? variable? operator? operator-info operator-exists?
         constant-info get-operator-arity
         get-parametric-operator parametric-operators parametric-operators-reverse
         get-parametric-constant parametric-constants parametric-constants-reverse
         *unknown-ops* *loaded-ops*
         repr-conv? rewrite-repr-op? get-repr-conv)

(module+ internals 
  (provide operator-impls constant-impls infix-joiner
           define-constant-impl define-operator-impl
           register-constant-impl! register-operator-impl!
           define-constant define-operator
           register-constant! register-operator!))

;; The new, contracts-using version of the above

(define-syntax-rule (define-table name [field type] ...)
  (define name (cons (list (cons 'field type) ...) (make-hash))))

(define (table-ref tbl key field)
  (match-let ([(cons header rows) tbl])
    (for/first ([(field-name type) (in-dict header)]
                [value (in-list (hash-ref rows key))]
                #:when (equal? field-name field))
      value)))

(define (table-set! tbl key fields)
  (match-let ([(cons header rows) tbl])
    (define row (for/list ([(hkey htype) (in-dict header)]) (dict-ref fields hkey)))
    (hash-set! rows key row)))

(define (table-remove! tbl key)
  (hash-remove! (cdr tbl) key))

(define (table-ref-all tbl key)
  (match-let ([(cons header rows) tbl])
    (and (hash-has-key? rows key)
         (map cons (map car header) (hash-ref rows key)))))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(module+ test (require rackunit))

;; Abstract constant table
;; Implementations inherit attributes

(define-table constants
  [bf (->* () bigvalue?)]
  [ival (or/c (->* () ival?) #f)])

(define (register-constant! name attrib-dict)
  (table-set! constants name (make-hash attrib-dict)))

(define-syntax-rule (define-constant name [key value] ...)
  (register-constant! 'name (list (cons 'key value) ...)))

(define-constant PI
  [bf (λ () pi.bf)] 
  [ival ival-pi])

(define-constant E
  [bf (λ () (bfexp 1.bf))]
  [ival ival-e])

(define-constant INFINITY
  [bf (λ () +inf.bf)]
  [ival (λ () (mk-ival +inf.bf))])

(define-constant NAN
  [bf (λ () +nan.bf)]
  [ival (λ () (mk-ival +nan.bf))])

(define-constant TRUE
  [bf (const true)]
  [ival (const (ival-bool true))])

(define-constant FALSE
  [bf (const false)]
  [ival (const (ival-bool false))])

;; Constant implementations

(define-table constant-impls
  [type type-name?]
  [bf (->* () bigvalue?)]
  [fl (->* () value?)]
  [ival (or/c (->* () ival?) #f)])

(define parametric-constants (make-hash))
(define parametric-constants-reverse (make-hash))

(define (constant-info constant field)
  (with-handlers ([exn:fail?
                   (λ (e) (error 'constant-info "Unknown constant or field: ~a ~a"
                                                constant field))])
    (table-ref constant-impls constant field)))

(define (dict-merge dict dict2)
  (for/fold ([dict dict]) ([(key value) (in-dict dict2)])
    (dict-set dict key value)))

(define (register-constant-impl! constant name ctype attrib-dict)
  (define default-attrib (table-ref-all constants constant))
  (unless default-attrib
    (error 'register-constant-impl! "Real constant does not exist: ~a" constant))
  (define attrib-dict* (dict-merge default-attrib attrib-dict))
  (table-set! constant-impls name
              (make-hash (cons (cons 'type ctype) attrib-dict*)))
  (hash-update! parametric-constants constant (curry cons (list* name ctype)) '())
  (hash-set! parametric-constants-reverse name constant))

(define-syntax-rule (define-constant-impl (constant name) ctype [key value] ...)
  (register-constant-impl! 'constant 'name 'ctype (list (cons 'key value) ...)))

(define (get-parametric-constant name type)
  (for/first ([(true-name rtype) (in-dict (hash-ref parametric-constants name))]
              #:when (equal? rtype type))
    true-name))

;; binary64 ;;
(define-constant-impl (PI PI.f64) binary64
  [fl (const pi)])

(define-constant-impl (E E.f64) binary64
  [fl (const (exp 1.0))])

(define-constant-impl (INFINITY INFINITY.f64) binary64
  [fl (const +inf.0)])

(define-constant-impl (NAN NAN.f64) binary64
  [fl (const +nan.0)])

;; binary32 ;;
(define-constant-impl (PI PI.f32) binary32
  [fl (const (->float32 pi))])

(define-constant-impl (E E.f32) binary32
  [fl (const (->float32 (exp 1.0)))])

(define-constant-impl (INFINITY INFINITY.f32) binary32
  [fl (const (->float32 +inf.0))])

(define-constant-impl (NAN NAN.f32) binary32
  [fl (const (->float32 +nan.0))])

;; bool ;;
(define-constant-impl (TRUE TRUE) bool
  [fl (const true)])

(define-constant-impl (FALSE FALSE) bool
  [fl (const false)])

;; TODO: The contracts for operators are tricky because the number of arguments is unknown
;; There's no easy way to write such a contract in Racket, so I only constrain the output type.
(define (unconstrained-argument-number-> from/c to/c)
  (unconstrained-domain-> to/c))

;; Abstract operator table
;; Implementations inherit attributes

(define-table operators
  [itype (or/c (listof type-name?) type-name?)]
  [otype type-name?]
  [bf    (unconstrained-argument-number-> bigvalue? bigvalue?)]
  [nonffi (unconstrained-argument-number-> value? value?)]
  [ival (or/c #f (unconstrained-argument-number-> ival? ival?))]) 

(define (register-operator! name itypes otype attrib-dict)
  (define itypes* (dict-ref attrib-dict 'itype itypes))
  (define otype* (dict-ref attrib-dict 'otype otype))
  (table-set! operators name
              (make-hash (append (list (cons 'itype itypes*) (cons 'otype otype*))
                         attrib-dict))))

(define-syntax-rule (define-operator (name itypes ...) otype [key value] ...)
  (register-operator! 'name '(itypes ...) 'otype
                      (list (cons 'key value) ...)))

(define-operator (neg real) real
  [bf bf-] [ival ival-neg] [nonffi -])

(define-operator (+ real real) real
  [bf bf+] [ival ival-add] [nonffi +])

(define-operator (- real real) real
  [bf bf-] [ival ival-sub] [nonffi -])

(define-operator (* real real) real
  [bf bf*] [ival ival-mult] [nonffi *])

(define-operator (/ real real) real
  [bf bf/] [ival ival-div] [nonffi /])

;; Operator implementations

(define-table operator-impls
  [itype (or/c (listof representation-name?) representation-name?)]
  [otype representation-name?]
  [bf    (unconstrained-argument-number-> bigvalue? bigvalue?)]
  [fl    (unconstrained-argument-number-> value? value?)]
  [nonffi (unconstrained-argument-number-> value? value?)]
  [ival (or/c #f (unconstrained-argument-number-> ival? ival?))])

(define parametric-operators (make-hash))
(define parametric-operators-reverse (make-hash))

(define (operator-info operator field)
  (with-handlers ([exn:fail? 
                   (λ (e) (error 'operator-info "Unknown operator or field: ~a ~a"
                                                operator field))])
    (table-ref operator-impls operator field)))

(define (operator-exists? op)
  (with-handlers ([exn:fail? (const false)])
    (table-ref operator-impls op 'otype)
    true))

(define (operator-remove! operator)
  (table-remove! operator-impls operator))

(define (*loaded-ops*)
  (hash-keys parametric-operators-reverse))

(define (check-operator-types! inherited itypes otype)
  (define itypes* (dict-ref inherited 'itype))
  (define otype* (dict-ref inherited 'otype))
  (define prec->type (compose representation-type get-representation))
  (and (equal? (prec->type otype) otype*)
       (or (and (type-name? itypes*) (type-name? itypes)
                (equal? (prec->type itypes) itypes*))
           (map (λ (x y) (equal? (prec->type x) y)) itypes itypes*))))

(define (register-operator-impl! operator name atypes rtype attrib-dict)
  (define default-attrib (table-ref-all operators operator))
  (unless default-attrib
    (error 'register-operator-impl! "Real operator does not exist: ~a" operator))
  ;; merge inherited and explicit attributes
  (define attrib-dict* (dict-merge default-attrib attrib-dict))
  (define itypes (dict-ref attrib-dict 'itype atypes))
  (define otype (dict-ref attrib-dict 'otype rtype))
  (unless (equal? operator 'if) ;; if does not work here
    (check-operator-types! default-attrib itypes otype))
  ;; Convert attributes to hash, update tables
  (define fields (make-hash attrib-dict*))
  (hash-set! fields 'itype itypes)
  (hash-set! fields 'otype otype)
  (table-set! operator-impls name fields)
  (hash-update! parametric-operators operator
                (curry cons (list* name otype (operator-info name 'itype)))
                '())
  (hash-set! parametric-operators-reverse name operator))
  

(define-syntax-rule (define-operator-impl (operator name atypes ...) rtype [key value] ...)
  (register-operator-impl! 'operator 'name '(atypes ...) 'rtype (list (cons 'key value) ...)))

(define (no-complex fun)
  (λ xs
     (define res (apply fun xs))
     (if (real? res) res +nan.0)))

(define (default-nonffi . args)
  (raise
   (make-exn:fail:unsupported
    (format "couldn't find ~a and no default implementation defined" 'operator)
    (current-continuation-marks))))

(define (get-parametric-operator name #:fail-fast? [fail-fast? #t] . actual-types)
  (or
    (for/or ([sig (hash-ref parametric-operators name)])
      (match-define (list* true-name rtype atypes) sig)
        (and (if (representation-name? atypes)
                 (andmap (curry equal? atypes) actual-types)
                 (equal? atypes actual-types))
             true-name))
    (and fail-fast?
         (error 'get-parametric-operator
                "parametric operator with op ~a and input types ~a not found"
                name actual-types))))

;; mainly useful for getting arg count of an unparameterized operator
;; will break if operator impls have different aritys
;; returns #f for variary operators
(define (get-operator-arity op)
  (let ([itypes (table-ref operators op 'itype)])
    (if (type-name? itypes) #f (length itypes))))
  
;; binary64 4-function ;;
(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])
 
;; binary32 4-function ;;
(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

(define *unknown-ops* (make-parameter '()))

(register-reset
 (λ ()
   (unless (flag-set? 'precision 'fallback)
     (for-each operator-remove! (*unknown-ops*)))))

(require ffi/unsafe)
(define-syntax (define-libm-operator stx)
  (syntax-case stx (real libm)
    [(_ (op opf64 opf32 real ...) real [libm id_d id_f] [key value] ...)
     (let* ([num-args (length (cdddr (syntax-e (cadr (syntax-e stx)))))])
       #`(begin
           (define (fallback prec . args)
             (warn 'fallback #:url "faq.html#native-ops"
                   "native `~a` not supported on your system, using fallback; ~a"
                   'op
                   "use --disable precision:fallback to disable fallbacks")
             (match prec
              ['double (apply (operator-info 'opf64 'nonffi) args)]
              ['float (apply (operator-info 'opf32 'nonffi) args)]))
           (define double-proc (get-ffi-obj 'id_d #f (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double)
                                            (lambda () (*unknown-ops* (cons 'opf64 (*unknown-ops*))) (curry fallback #'double))))
           (define float-proc (get-ffi-obj 'id_f #f (_fun #,@(build-list num-args (λ (_) #'_float)) -> _float)
                                           (lambda () (*unknown-ops* (cons 'opf32 (*unknown-ops*))) (curry fallback #'float))))
           (define-operator (op #,@(build-list num-args (λ (_) #'real))) real [key value] ...)
           (define-operator-impl (op opf64 #,@(build-list num-args (λ (_) #'binary64))) binary64
             [fl (λ args (apply double-proc args))])
           (define-operator-impl (op opf32 #,@(build-list num-args (λ (_) #'binary32))) binary32
             [fl (λ args (->float32 (apply float-proc args)))])
       ))]))

(define-libm-operator (acos acos.f64 acos.f32 real) real
  [libm acos acosf] [bf bfacos] [ival ival-acos]
  [nonffi (no-complex acos)])

(define-libm-operator (acosh acosh.f64 acosh.f32 real) real
  [libm acosh acoshf] [bf bfacosh] [ival ival-acosh]
  [nonffi (no-complex acosh)])

(define-libm-operator (asin asin.f64 asin.f32 real) real
  [libm asin asinf] [bf bfasin] [ival ival-asin]
  [nonffi (no-complex asin)])

(define-libm-operator (asinh asinh.f64 asinh.f32 real) real
  [libm asinh asinhf] [bf bfasinh] [ival ival-asinh]
  [nonffi (no-complex asinh)])

(define-libm-operator (atan atan.f64 atan.f32 real) real
  [libm atan atanf] [bf bfatan] [ival ival-atan]
  [nonffi (no-complex atan)])

(define-libm-operator (atan2 atan2.f64 atan2.f32 real real) real
  [libm atan2 atan2f] [bf bfatan2] [ival ival-atan2]
  [nonffi (no-complex atan)])

(define-libm-operator (atanh atanh.f64 atanh.f32 real) real
  [libm atanh atanhf] [bf bfatanh] [ival ival-atanh]
  [nonffi (no-complex atanh)])

(define-libm-operator (cbrt cbrt.f64 cbrt.f32 real) real
  [libm cbrt cbrtf] [bf bfcbrt] [ival ival-cbrt]
  [nonffi (no-complex (λ (x) (expt x (/ 1 3))))])

(define-libm-operator (ceil ceil.f64 ceil.f32 real) real
  [libm ceil ceilf] [bf bfceiling] [ival ival-ceil]
  [nonffi ceiling])

(define (bfcopysign x y)
  (bf* (bfabs x) (bf (expt -1 (bigfloat-signbit y)))))

(define-libm-operator (copysign copysign.f64 copysign.f32 real real) real
  [libm copysign copysignf] [bf bfcopysign] [ival ival-copysign]
  [nonffi (λ (x y) (if (>= y 0) (abs x) (- (abs x))))])

(define-libm-operator (cos cos.f64 cos.f32 real) real
  [libm cos cosf] [bf bfcos] [ival ival-cos]
  [nonffi cos])

(define-libm-operator (cosh cosh.f64 cosh.f32 real) real
  [libm cosh coshf] [bf bfcosh] [ival ival-cosh]
  [nonffi cosh])

(define-libm-operator (erf erf.f64 erf.f32 real) real
  [libm erf erff] [bf bferf] [ival ival-erf]
  [nonffi (no-complex erf)])

(define-libm-operator (erfc erfc.f64 erfc.f32 real) real
  [libm erfc erfcf] [bf bferfc] [ival ival-erfc]
  [nonffi erfc])

(define-libm-operator (exp exp.f64 exp.f32 real) real
  [libm exp expf] [bf bfexp] [ival ival-exp]
  [nonffi exp])

(define-libm-operator (exp2 exp2.f64 exp2.f32 real) real
  [libm exp2 exp2f] [bf bfexp2] [ival ival-exp2]
  [nonffi (no-complex (λ (x) (expt 2 x)))])

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define-libm-operator (expm1 expm1.f64 expm1.f32 real) real
  [libm expm1 expm1f] [bf bfexpm1] [ival ival-expm1]
  [nonffi (from-bigfloat bfexpm1)])

(define-libm-operator (fabs fabs.f64 fabs.f32 real) real
  [libm fabs fabsf] [bf bfabs] [ival ival-fabs]
  [nonffi abs])

(define (bffdim x y)
  (if (bf> x y)
    (bf- x y)
    0.bf))

(define-libm-operator (fdim fdim.f64 fdim.f32 real real) real
  [libm fdim fdimf] [bf bffdim] [ival ival-fdim]
  [nonffi (λ (x y) (max (- x y) 0))])

(define-libm-operator (floor floor.f64 floor.f32 real) real
  [libm floor floorf] [bf bffloor] [ival ival-floor]
  [nonffi (λ (x) (floor x))])

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define-libm-operator (fma fma.f64 fma.f32 real real real) real
  [libm fma fmaf] [bf bffma] [ival ival-fma]
  [nonffi (λ (x y z) (bigfloat->flonum (bf+ (bf* (bf x) (bf y)) (bf z))))])

(define-libm-operator (fmax fmax.f64 fmax.f32 real real) real
  [libm fmax fmaxf] [bf bfmax] [ival ival-fmax]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (max x y)]))])

(define-libm-operator (fmin fmin.f64 fmin.f32 real real) real
  [libm fmin fminf] [bf bfmin] [ival ival-fmin]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (min x y)]))])

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define-libm-operator (fmod fmod.f64 fmod.f32 real real) real
  [libm fmod fmodf] [bf bffmod] [ival ival-fmod]
  [nonffi (from-bigfloat bffmod)])

(define-libm-operator (hypot hypot.f64 hypot.f32 real real) real
  [libm hypot hypotf] [bf bfhypot] [ival ival-hypot]
  [nonffi (from-bigfloat bfhypot)])

(define-libm-operator (j0 j0.f64 j0.f32 real) real
  [libm j0 j0f] [bf bfbesj0] [ival #f]
  [nonffi (from-bigfloat bfbesj0)])

(define-libm-operator (j1 j1.f64 j1.f32 real) real
  [libm j1 j1f] [bf bfbesj1] [ival #f]
  [nonffi (from-bigfloat bfbesj1)])

(define-libm-operator (lgamma lgamma.f64 lgamma.f32 real) real
  [libm lgamma lgammaf] [bf bflog-gamma] [ival #f]
  [nonffi log-gamma])

(define-libm-operator (log log.f64 log.f32 real) real
  [libm log logf] [bf bflog] [ival ival-log]
  [nonffi (no-complex log)])

(define-libm-operator (log10 log10.f64 log10.f32 real) real
  [libm log10 log10f] [bf bflog10] [ival ival-log10]
  [nonffi (no-complex (λ (x) (log x 10)))])

(define-libm-operator (log1p log1p.f64 log1p.f32 real) real
  [libm log1p log1pf] [bf bflog1p] [ival ival-log1p]
  [nonffi (from-bigfloat bflog1p)])

(define-libm-operator (log2 log2.f64 log2.f32 real) real
  [libm log2 log2f] [bf bflog2] [ival ival-log2]
  [nonffi (from-bigfloat bflog2)])

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define-libm-operator (logb logb.f64 logb.f32 real) real
  [libm logb logbf] [bf bflogb] [ival ival-logb]
  [nonffi (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))])

(define-libm-operator (pow pow.f64 pow.f32 real real) real
  [libm pow powf] [bf bfexpt] [ival ival-pow]
  [nonffi (no-complex expt)])

(define (bfremainder x mod)
  (bf- x (bf* (bfround (bf/ x mod)) mod)))

(define-libm-operator (remainder remainder.f64 remainder.f32 real real) real
  [libm remainder remainderf] [bf bfremainder] [ival ival-remainder] 
  [nonffi remainder])

(define-libm-operator (rint rint.f64 rint.f32 real) real
  [libm rint rintf] [bf bfrint] [ival ival-rint]
  [nonffi round])

(define-libm-operator (round round.f64 round.f32 real) real
  [libm round roundf] [bf bfround] [ival ival-round]
  [nonffi round])

(define-libm-operator (sin sin.f64 sin.f32 real) real
  [libm sin sinf] [bf bfsin] [ival ival-sin]
  [nonffi sin])

(define-libm-operator (sinh sinh.f64 sinh.f32 real) real
  [libm sinh sinhf] [bf bfsinh] [ival ival-sinh]
  [nonffi sinh])

(define-libm-operator (sqrt sqrt.f64 sqrt.f32 real) real
  [libm sqrt sqrtf] [bf bfsqrt] [ival ival-sqrt]
  [nonffi (no-complex sqrt)])

(define-libm-operator (tan tan.f64 tan.f32 real) real
  [libm tan tanf] [bf bftan] [ival ival-tan]
  [nonffi tan])

(define-libm-operator (tanh tanh.f64 tanh.f32 real) real
  [libm tanh tanhf] [bf bftanh] [ival ival-tanh]
  [nonffi tanh])

(define-libm-operator (tgamma tgamma.f64 tgamma.f32 real) real
  [libm tgamma tgammaf] [bf bfgamma] [ival #f]
  [nonffi gamma])

(define-libm-operator (trunc trunc.f64 trunc.f32 real) real
  [libm trunc truncf] [bf bftruncate] [ival ival-trunc]
  [nonffi truncate])

(define-libm-operator (y0 y0.f64 y0.f32 real) real
  [libm y0 y0f] [bf bfbesy0] [ival #f] 
  [nonffi (from-bigfloat bfbesy0)])

(define-libm-operator (y1 y1.f64 y1.f32 real) real
  [libm y1 y1f] [bf bfbesy1] [ival #f]
  [nonffi (from-bigfloat bfbesy1)])

;; If

(define (if-fn test if-true if-false) (if test if-true if-false))

(define-operator (if real real) real
  [bf if-fn] [ival ival-if] [nonffi if-fn])

(define-operator-impl (if if bool real real) real [fl if-fn]) ; types not used

(define ((infix-joiner x) . args)
  (string-join args x))

;; real operators
(define-operator (==) real
  [itype 'real] [bf (comparator bf=)] [ival ival-==] [nonffi (comparator =)])

(define-operator (!=) real
  [itype 'real] [bf (negate (comparator bf=))] [ival ival-!=]
  [nonffi (negate (comparator =))])

(define-operator (<) real
  [itype 'real] [bf (comparator bf<)] [ival ival-<] [nonffi (comparator <)])

(define-operator (>) real
  [itype 'real] [bf (comparator bf>)] [ival ival->] [nonffi (comparator >)])

(define-operator (<=) real
  [itype 'real] [bf (comparator bf<=)] [ival ival-<=] [nonffi (comparator <=)])

(define-operator (>=) real
  [itype 'real] [bf (comparator bf>=)] [ival ival->=] [nonffi (comparator >=)])

;; binary64 comparators ;;
(define-operator-impl (== ==.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator =)])

(define-operator-impl (!= !=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (negate (comparator =))])

(define-operator-impl (< <.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator <)])

(define-operator-impl (> >.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator >)])

(define-operator-impl (<= <=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)])

(define-operator-impl (>= >=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator >=)])

;; binary32 comparators
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

;; logical operators ;;

(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define-operator (not bool) bool
  [bf not] [ival ival-not] [nonffi not])

(define-operator (and bool bool) bool
  [itype 'bool] ; override number of arguments
  [bf and-fn] [ival ival-and] [nonffi and-fn])

(define-operator (or bool bool) bool
  [itype 'bool] ; override number of arguments
  [bf or-fn] [ival ival-or] [nonffi or-fn])

(define-operator-impl (not not bool) bool
  [fl not])

(define-operator-impl (and and bool bool) bool
  [itype 'bool] [otype 'bool] ; Override number of arguments
  [fl and-fn])

(define-operator-impl (or or bool bool) bool
  [itype 'bool] [otype 'bool] ; Override number of arguments
  [fl or-fn])

;; Miscellaneous operators ;;

(define (repr-conv? expr)
  (and (symbol? expr) (regexp-match? #px"^[\\S]+(->)[\\S]+$" (symbol->string expr))))

(define (rewrite-repr-op? expr)
  (and (symbol? expr) (regexp-match? #px"^(<-)[\\S]+$" (symbol->string expr))))

(define (get-repr-conv iprec oprec)
  (for/or ([sig (hash-ref parametric-operators 'cast)])
    (match-define (list* true-name rtype atypes) sig)
      (and (repr-conv? true-name)
           (equal? rtype oprec)
           (equal? (car atypes) iprec)
           true-name)))

;; Conversions

(define-operator (cast real) real
  [bf identity] [ival identity] [nonffi identity])

(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])

;; Expression predicates ;;

(define (operator? op)
  (and (symbol? op) (not (equal? op 'if))
       (or (hash-has-key? parametric-operators op)
           (dict-has-key? (cdr operator-impls) op))))

(define (constant? var)
  (or (real? var)
      (and (symbol? var)
           (or (hash-has-key? parametric-constants var) 
               (dict-has-key? (cdr constant-impls) var)))))

(define (variable? var)
  (and (symbol? var) (not (constant? var))))
