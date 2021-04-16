#lang racket

(require math/flonum math/base math/bigfloat math/special-functions rival)
(require "../common.rkt" "../interface.rkt" "../errors.rkt" "../float32.rkt" "types.rkt")

(provide constant? variable? operator? operator-info constant-info get-operator-itype
         get-parametric-operator parametric-operators parametric-operators-reverse
         get-parametric-constant parametric-constants parametric-constants-reverse
         *unknown-ops* *loaded-ops*
         repr-conv? rewrite-repr-op? get-repr-conv)

(module+ internals 
  (provide operators constants define-constant define-operator infix-joiner
           register-constant! register-operator!))

(module+ test (require rackunit))

;; Abstract constant table
;; Implementations inherit attributes

(define-table real-constants
  [bf (->* () bigvalue?)]
  [ival (or/c (->* () ival?) #f)])

(define (register-real-constant! name attrib-dict)
  (table-set! real-constants name (make-hash attrib-dict)))

(define-syntax-rule (define-real-constant name [key value] ...)
  (register-real-constant! 'name (list (cons 'key value) ...)))

(define-real-constant PI
  [bf (λ () pi.bf)] 
  [ival ival-pi])

(define-real-constant E
  [bf (λ () (exp 1.0))] 
  [ival ival-e])

(define-real-constant INFINITY
  [bf (λ () +inf.bf)]
  [ival (λ () (mk-ival +inf.bf))])

(define-real-constant NAN
  [bf (λ () +nan.bf)]
  [ival (λ () (mk-ival +nan.bf))])

(define-real-constant TRUE
  [bf (const true)]
  [ival (const (ival-bool true))])

(define-real-constant FALSE
  [bf (const false)]
  [ival (const (ival-bool false))])

;; Constant implementations

(define-table constants
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
    (table-ref constants constant field)))

(define (dict-merge dict dict2)
  (for/fold ([dict dict]) ([(key value) (in-dict dict2)])
    (dict-set dict key value)))

(define (register-constant! constant name ctype attrib-dict)
  (define default-attrib (table-ref-all real-constants constant))
  (unless default-attrib
    (error 'register-constant! "Real constant does not exist: ~a" constant))
  (define attrib-dict* (dict-merge default-attrib attrib-dict))
  (table-set! constants name
              (make-hash (cons (cons 'type ctype) attrib-dict*)))
  (hash-update! parametric-constants constant (curry cons (list* name ctype)) '())
  (hash-set! parametric-constants-reverse name constant))

(define-syntax-rule (define-constant (constant name) ctype [key value] ...)
  (register-constant! 'constant 'name 'ctype (list (cons 'key value) ...)))

(define (get-parametric-constant name type)
  (for/first ([(true-name rtype) (in-dict (hash-ref parametric-constants name))]
              #:when (equal? rtype type))
    true-name))

;; binary64 ;;
(define-constant (PI PI.f64) binary64
  [fl (λ () pi)])

(define-constant (E E.f64) binary64
  [fl (λ () (exp 1.0))])

(define-constant (INFINITY INFINITY.f64) binary64
  [fl (λ () +inf.0)])

(define-constant (NAN NAN.f64) binary64
  [fl (λ () +nan.0)])

;; binary32 ;;
(define-constant (PI PI.f32) binary32
  [fl (λ () pi)])

(define-constant (E E.f32) binary32
  [fl (λ () (exp 1.0))])

(define-constant (INFINITY INFINITY.f32) binary32
  [fl (λ () +inf.0)])

(define-constant (NAN NAN.f32) binary32
  [fl (λ () +nan.0)])

;; bool ;;
(define-constant (TRUE TRUE) bool
  [fl (const true)])

(define-constant (FALSE FALSE) bool
  [fl (const false)])

;; TODO: The contracts for operators are tricky because the number of arguments is unknown
;; There's no easy way to write such a contract in Racket, so I only constrain the output type.
(define (unconstrained-argument-number-> from/c to/c)
  (unconstrained-domain-> to/c))

;; Abstract operator table
;; Implementations inherit attributes

(define-table real-operators
  [bf    (unconstrained-argument-number-> bigvalue? bigvalue?)]
  [nonffi (unconstrained-argument-number-> value? value?)]
  [ival (or/c #f (unconstrained-argument-number-> ival? ival?))]) 

(define (register-real-operator! name attrib-dict)
  (table-set! real-operators name (make-hash attrib-dict)))

(define-syntax-rule (define-real-operator name [key value] ...)
  (register-real-operator! 'name (list (cons 'key value) ...)))

(define (ival-sub/neg x [y #f])
  (if y (ival-sub x y) (ival-neg x)))

(define-real-operator + [bf bf+] [ival ival-add] [nonffi +])
(define-real-operator - [bf bf-] [ival ival-sub/neg] [nonffi -])
(define-real-operator * [bf bf*] [ival ival-mult] [nonffi *])
(define-real-operator / [bf bf/] [ival ival-div] [nonffi /])

;; Operator implementations

(define-table operators
  [itype (or/c (listof type-name?) type-name?)]
  [otype type-name?]
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
    (table-ref operators operator field)))

(define (operator-remove! operator)
  (table-remove! operators operator))

(define (*loaded-ops*)
  (hash-keys parametric-operators-reverse))

(define (register-operator! operator name atypes rtype attrib-dict)
  (define default-attrib (table-ref-all real-operators operator))
  (unless default-attrib
    (error 'register-operator! "Real operator does not exist: ~a" operator))
  (define attrib-dict* (dict-merge default-attrib attrib-dict))
  (define itypes (dict-ref attrib-dict* 'itype atypes))
  (define otype (dict-ref attrib-dict* 'otype rtype))
  (table-set! operators name
              (make-hash (append (list (cons 'itype itypes) (cons 'otype otype)) attrib-dict*)))
  (hash-update! parametric-operators operator
                (λ (h) (hash-set h itypes (cons name otype)))
                (hash))
  (hash-set! parametric-operators-reverse name operator))

(define-syntax-rule (define-operator (operator name atypes ...) rtype [key value] ...)
  (register-operator! 'operator 'name '(atypes ...) 'rtype (list (cons 'key value) ...)))

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
  (let ([op-info (hash-ref parametric-operators name)])
    (or (car (hash-ref op-info actual-types (cons #f #f))) ; dumb way to fail with #f
        (car (hash-ref op-info (car actual-types) (cons #f #f)))
        (if fail-fast?
            (error 'get-parametric-operator
                   "parametric operator with op ~a and input types ~a not found"
                   name actual-types)
            #f))))

;; mainly useful for getting arg count of an unparameterized operator
;; TODO: hopefully will be fixed when the way operators are declared
;; gets overhauled
(define (get-operator-itype op) 
  (operator-info
    (if (hash-has-key? parametric-operators op)
        (car (first (hash-values (hash-ref parametric-operators op))))
        op)
    'itype))

;; binary64 4-function ;;
(define-operator (- neg.f64 binary64) binary64 [fl -])
(define-operator (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator (/ /.f64 binary64 binary64) binary64 [fl /])
 
;; binary32 4-function ;;
(define-operator (- neg.f32 binary32) binary32 [fl -])
(define-operator (+ +.f32 binary32 binary32) binary32 [fl +])
(define-operator (- -.f32 binary32 binary32) binary32 [fl -])
(define-operator (* *.f32 binary32 binary32) binary32 [fl *])
(define-operator (/ /.f32 binary32 binary32) binary32 [fl /])

(define *unknown-ops* (make-parameter '()))

(register-reset
 (λ ()
   (unless (flag-set? 'precision 'fallback)
     (for-each operator-remove! (*unknown-ops*)))))

(require ffi/unsafe)
(define-syntax (define-operator/libm stx)
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
           (define-real-operator op [key value] ...)
           (define-operator (op opf64 #,@(build-list num-args (λ (_) #'binary64))) binary64
             [fl (λ args (apply double-proc args))])
           (define-operator (op opf32 #,@(build-list num-args (λ (_) #'binary32))) binary32
             [fl (λ args (->float32 (apply float-proc args)))])
       ))]))

(define-operator/libm (acos acos.f64 acos.f32 real) real
  [libm acos acosf] [bf bfacos] [ival ival-acos]
  [nonffi (no-complex acos)])

(define-operator/libm (acosh acosh.f64 acosh.f32 real) real
  [libm acosh acoshf] [bf bfacosh] [ival ival-acosh]
  [nonffi (no-complex acosh)])

(define-operator/libm (asin asin.f64 asin.f32 real) real
  [libm asin asinf] [bf bfasin] [ival ival-asin]
  [nonffi (no-complex asin)])

(define-operator/libm (asinh asinh.f64 asinh.f32 real) real
  [libm asinh asinhf] [bf bfasinh] [ival ival-asinh]
  [nonffi (no-complex asinh)])

(define-operator/libm (atan atan.f64 atan.f32 real) real
  [libm atan atanf] [bf bfatan] [ival ival-atan]
  [nonffi (no-complex atan)])

(define-operator/libm (atan2 atan2.f64 atan2.f32 real real) real
  [libm atan2 atan2f] [bf bfatan2] [ival ival-atan2]
  [nonffi (no-complex atan)])

(define-operator/libm (atanh atanh.f64 atanh.f32 real) real
  [libm atanh atanhf] [bf bfatanh] [ival ival-atanh]
  [nonffi (no-complex atanh)])

(define-operator/libm (cbrt cbrt.f64 cbrt.f32 real) real
  [libm cbrt cbrtf] [bf bfcbrt] [ival ival-cbrt]
  [nonffi (no-complex (λ (x) (expt x (/ 1 3))))])

(define-operator/libm (ceil ceil.f64 ceil.f32 real) real
  [libm ceil ceilf] [bf bfceiling] [ival ival-ceil]
  [nonffi ceiling])

(define (bfcopysign x y)
  (bf* (bfabs x) (bf (expt -1 (bigfloat-signbit y)))))

(define-operator/libm (copysign copysign.f64 copysign.f32 real real) real
  [libm copysign copysignf] [bf bfcopysign] [ival ival-copysign]
  [nonffi (λ (x y) (if (>= y 0) (abs x) (- (abs x))))])

(define-operator/libm (cos cos.f64 cos.f32 real) real
  [libm cos cosf] [bf bfcos] [ival ival-cos]
  [nonffi cos])

(define-operator/libm (cosh cosh.f64 cosh.f32 real) real
  [libm cosh coshf] [bf bfcosh] [ival ival-cosh]
  [nonffi cosh])

(define-operator/libm (erf erf.f64 erf.f32 real) real
  [libm erf erff] [bf bferf] [ival ival-erf]
  [nonffi (no-complex erf)])

(define-operator/libm (erfc erfc.f64 erfc.f32 real) real
  [libm erfc erfcf] [bf bferfc] [ival ival-erfc]
  [nonffi erfc])

(define-operator/libm (exp exp.f64 exp.f32 real) real
  [libm exp expf] [bf bfexp] [ival ival-exp]
  [nonffi exp])

(define-operator/libm (exp2 exp2.f64 exp2.f32 real) real
  [libm exp2 exp2f] [bf bfexp2] [ival ival-exp2]
  [nonffi (no-complex (λ (x) (expt 2 x)))])

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define-operator/libm (expm1 expm1.f64 expm1.f32 real) real
  [libm expm1 expm1f] [bf bfexpm1] [ival ival-expm1]
  [nonffi (from-bigfloat bfexpm1)])

(define-operator/libm (fabs fabs.f64 fabs.f32 real) real
  [libm fabs fabsf] [bf bfabs] [ival ival-fabs]
  [nonffi abs])

(define (bffdim x y)
  (if (bf> x y)
    (bf- x y)
    0.bf))

(define-operator/libm (fdim fdim.f64 fdim.f32 real real) real
  [libm fdim fdimf] [bf bffdim] [ival ival-fdim]
  [nonffi (λ (x y) (max (- x y) 0))])

(define-operator/libm (floor floor.f64 floor.f32 real) real
  [libm floor floorf] [bf bffloor] [ival ival-floor]
  [nonffi (λ (x) (floor x))])

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define-operator/libm (fma fma.f64 fma.f32 real real real) real
  [libm fma fmaf] [bf bffma] [ival ival-fma]
  [nonffi (λ (x y z) (bigfloat->flonum (bf+ (bf* (bf x) (bf y)) (bf z))))])

(define-operator/libm (fmax fmax.f64 fmax.f32 real real) real
  [libm fmax fmaxf] [bf bfmax] [ival ival-fmax]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (max x y)]))])

(define-operator/libm (fmin fmin.f64 fmin.f32 real real) real
  [libm fmin fminf] [bf bfmin] [ival ival-fmin]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (min x y)]))])

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define-operator/libm (fmod fmod.f64 fmod.f32 real real) real
  [libm fmod fmodf] [bf bffmod] [ival ival-fmod]
  [nonffi (from-bigfloat bffmod)])

(define-operator/libm (hypot hypot.f64 hypot.f32 real real) real
  [libm hypot hypotf] [bf bfhypot] [ival ival-hypot]
  [nonffi (from-bigfloat bfhypot)])

(define-operator/libm (j0 j0.f64 j0.f32 real) real
  [libm j0 j0f] [bf bfbesj0] [ival #f]
  [nonffi (from-bigfloat bfbesj0)])

(define-operator/libm (j1 j1.f64 j1.f32 real) real
  [libm j1 j1f] [bf bfbesj1] [ival #f]
  [nonffi (from-bigfloat bfbesj1)])

(define-operator/libm (lgamma lgamma.f64 lgamma.f32 real) real
  [libm lgamma lgammaf] [bf bflog-gamma] [ival #f]
  [nonffi log-gamma])

(define-operator/libm (log log.f64 log.f32 real) real
  [libm log logf] [bf bflog] [ival ival-log]
  [nonffi (no-complex log)])

(define-operator/libm (log10 log10.f64 log10.f32 real) real
  [libm log10 log10f] [bf bflog10] [ival ival-log10]
  [nonffi (no-complex (λ (x) (log x 10)))])

(define-operator/libm (log1p log1p.f64 log1p.f32 real) real
  [libm log1p log1pf] [bf bflog1p] [ival ival-log1p]
  [nonffi (from-bigfloat bflog1p)])

(define-operator/libm (log2 log2.f64 log2.f32 real) real
  [libm log2 log2f] [bf bflog2] [ival ival-log2]
  [nonffi (from-bigfloat bflog2)])

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define-operator/libm (logb logb.f64 logb.f32 real) real
  [libm logb logbf] [bf bflogb] [ival ival-logb]
  [nonffi (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))])

(define-operator/libm (pow pow.f64 pow.f32 real real) real
  [libm pow powf] [bf bfexpt] [ival ival-pow]
  [nonffi (no-complex expt)])

(define (bfremainder x mod)
  (bf- x (bf* (bfround (bf/ x mod)) mod)))

(define-operator/libm (remainder remainder.f64 remainder.f32 real real) real
  [libm remainder remainderf] [bf bfremainder] [ival ival-remainder] 
  [nonffi remainder])

(define-operator/libm (rint rint.f64 rint.f32 real) real
  [libm rint rintf] [bf bfrint] [ival ival-rint]
  [nonffi round])

(define-operator/libm (round round.f64 round.f32 real) real
  [libm round roundf] [bf bfround] [ival ival-round]
  [nonffi round])

(define-operator/libm (sin sin.f64 sin.f32 real) real
  [libm sin sinf] [bf bfsin] [ival ival-sin]
  [nonffi sin])

(define-operator/libm (sinh sinh.f64 sinh.f32 real) real
  [libm sinh sinhf] [bf bfsinh] [ival ival-sinh]
  [nonffi sinh])

(define-operator/libm (sqrt sqrt.f64 sqrt.f32 real) real
  [libm sqrt sqrtf] [bf bfsqrt] [ival ival-sqrt]
  [nonffi (no-complex sqrt)])

(define-operator/libm (tan tan.f64 tan.f32 real) real
  [libm tan tanf] [bf bftan] [ival ival-tan]
  [nonffi tan])

(define-operator/libm (tanh tanh.f64 tanh.f32 real) real
  [libm tanh tanhf] [bf bftanh] [ival ival-tanh]
  [nonffi tanh])

(define-operator/libm (tgamma tgamma.f64 tgamma.f32 real) real
  [libm tgamma tgammaf] [bf bfgamma] [ival #f]
  [nonffi gamma])

(define-operator/libm (trunc trunc.f64 trunc.f32 real) real
  [libm trunc truncf] [bf bftruncate] [ival ival-trunc]
  [nonffi truncate])

(define-operator/libm (y0 y0.f64 y0.f32 real) real
  [libm y0 y0f] [bf bfbesy0] [ival #f] 
  [nonffi (from-bigfloat bfbesy0)])

(define-operator/libm (y1 y1.f64 y1.f32 real) real
  [libm y1 y1f] [bf bfbesy1] [ival #f]
  [nonffi (from-bigfloat bfbesy1)])

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

;; If
(define-real-operator if [bf if-fn] [ival ival-if] [nonffi if-fn])
(define-operator (if if bool real real) real [fl if-fn]) ; types not used

(define ((infix-joiner x) . args)
  (string-join args x))

;; real operators
(define-real-operator == [bf (comparator bf=)] [ival ival-==] [nonffi (comparator =)])
(define-real-operator != [bf (inv-comparator bf=)] [ival ival-!=] [nonffi (inv-comparator =)])
(define-real-operator < [bf (comparator bf>)] [ival ival-<] [nonffi (comparator <)])
(define-real-operator > [bf (comparator bf<)] [ival ival->] [nonffi (comparator >)])
(define-real-operator <= [bf (comparator bf<=)] [ival ival-<=] [nonffi (comparator <=)])
(define-real-operator >= [bf (comparator bf<=)] [ival ival->=] [nonffi (comparator >=)])

;; binary64 comparators ;;
(define-operator (== ==.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator =)])

(define-operator (!= !=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (inv-comparator =)])

(define-operator (< <.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator <)])

(define-operator (> >.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator >)])

(define-operator (<= <=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)])

(define-operator (>= >=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator >=)])

;; binary32 comparators
(define-operator (== ==.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator =)])

(define-operator (!= !=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (inv-comparator =)])

(define-operator (< <.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator <)])

(define-operator (> >.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator >)])

(define-operator (<= <=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)])

(define-operator (>= >=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator >=)])

;; logical operators ;;
(define-real-operator not [bf not] [ival ival-not] [nonffi not])
(define-operator (not not bool) bool [fl not])

(define-real-operator and [bf and-fn] [ival ival-and] [nonffi and-fn])
(define-operator (and and bool bool) bool
  [itype 'bool] [otype 'bool] ; Override number of arguments
  [fl and-fn])

(define-real-operator or [bf or-fn] [ival ival-or] [nonffi or-fn])
(define-operator (or or bool bool) bool
  [itype 'bool] [otype 'bool] ; Override number of arguments
  [fl or-fn])

;; Miscellaneous operators ;;

(define (repr-conv? expr)
  (and (symbol? expr) (regexp-match? #px"^[\\S]+(->)[\\S]+$" (symbol->string expr))))

(define (rewrite-repr-op? expr)
  (and (symbol? expr) (regexp-match? #px"^(<-)[\\S]+$" (symbol->string expr))))

(define (get-repr-conv iprec oprec)
  (for/or ([(op table) (in-hash parametric-operators)] #:when (repr-conv? op))
    (for/first ([(atypes info) (in-hash table)] #:when #t)
      (and (equal? (cdr info) oprec)
           (equal? (car atypes) iprec)
           (car info)))))

;; Casts
(define-real-operator cast [bf identity] [ival identity] [nonffi identity])
(define-operator (cast cast.f64 binary64) binary64 [fl identity])
(define-operator (cast cast.f32 binary32) binary32 [fl identity])

;; Conversions

(define-real-operator binary64->binary32 [bf identity] [ival identity] [nonffi (curryr ->float32)])
(define-operator (binary64->binary32 binary64->binary32 binary64)
  binary32 [fl (curryr ->float32)])

(define-real-operator binary32->binary64 [bf identity] [ival identity] [nonffi identity])
(define-operator (binary32->binary64 binary32->binary64 binary32)
  binary64 [fl identity])

;; Expression predicates ;;

(define (operator? op)
  (and (symbol? op) (not (equal? op 'if))
       (or (hash-has-key? parametric-operators op)
           (dict-has-key? (cdr operators) op))))

(define (constant? var)
  (or (real? var)
      (and (symbol? var)
           (or (hash-has-key? parametric-constants var) 
               (dict-has-key? (cdr constants) var)))))

(define (variable? var)
  (and (symbol? var) (not (constant? var))))
