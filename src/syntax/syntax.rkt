#lang racket

(require math/flonum math/base math/bigfloat math/special-functions rival)
(require "../common.rkt" "../interface.rkt" "../errors.rkt" "types.rkt")

(provide (rename-out [constant-or-impl? constant?]
                     [operator-or-impl? operator?])
         variable? operator-info operator-exists?
         *functions* register-function!
         constant-info get-operator-arity
         get-parametric-operator parametric-operators parametric-operators-reverse
         get-parametric-constant parametric-constants parametric-constants-reverse
         *unknown-ops* *loaded-ops*
         repr-conv? rewrite-repr-op? get-repr-conv)

(module+ internals 
  (provide define-constant-impl define-operator-impl
           register-constant-impl! register-operator-impl!
           define-constant define-operator
           register-constant! register-operator!))

;; The new, contracts-using version of the above

(module+ test (require rackunit))

;; Abstract constant table
;; Implementations inherit attributes

(struct constant (bf ival))
(define constants (make-hasheq))

(define (register-constant! name attrib-dict)
  (hash-set! constants name (apply constant (map (curry dict-ref attrib-dict) '(bf ival)))))

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

(struct constant-impl (type bf fl ival))
(define constant-impls (make-hasheq))

(define parametric-constants (hash))
(define parametric-constants-reverse (hash))

(define/contract (constant-info constant field)
  (-> symbol? (or/c 'type 'bf 'fl 'ival) any/c)
  (unless (hash-has-key? constant-impls constant)
    (error 'constant-info "Unknown constant ~a" constant))
  (define accessor
    (match field
      ['type constant-impl-type]
      ['bf constant-impl-bf]
      ['fl constant-impl-fl]
      ['ival constant-impl-ival]))
  (accessor (hash-ref constant-impls constant)))

(define (dict-merge dict dict2)
  (for/fold ([dict dict]) ([(key value) (in-dict dict2)])
    (dict-set dict key value)))

(define (register-constant-impl! constant name ctype attrib-dict)
  (define op (hash-ref constants constant))
  (define default-attrib
    (list (cons 'bf (constant-bf op))
          (cons 'ival (constant-ival op))))
  (unless default-attrib
    (error 'register-constant-impl! "Real constant does not exist: ~a" constant))
  (define attrib-dict* (make-hasheq (cons (cons 'type ctype) (dict-merge default-attrib attrib-dict))))
  (hash-set! constant-impls name
             (apply constant-impl (map (curry dict-ref attrib-dict*) '(type bf fl ival))))
  (set! parametric-constants
    (hash-update parametric-constants constant
                 (curry cons (list* name ctype))
                 '()))
  (set! parametric-constants-reverse
    (hash-set parametric-constants-reverse name constant)))

(define-syntax-rule (define-constant-impl (constant name) ctype [key value] ...)
  (register-constant-impl! 'constant 'name 'ctype (list (cons 'key value) ...)))

(define (get-parametric-constant name type)
  (for/first ([(true-name rtype) (in-dict (hash-ref parametric-constants name))]
              #:when (equal? rtype type))
    true-name))


;; TODO: The contracts for operators are tricky because the number of arguments is unknown
;; There's no easy way to write such a contract in Racket, so I only constrain the output type.
(define (unconstrained-argument-number-> from/c to/c)
  (unconstrained-domain-> to/c))

;; Abstract operator table
;; Implementations inherit attributes

(struct operator (itype otype bf ival))
(define operators (make-hasheq))

(define (register-operator! name itypes otype attrib-dict)
  (define itypes* (dict-ref attrib-dict 'itype itypes))
  (define otype* (dict-ref attrib-dict 'otype otype))
  (define fields (make-hasheq (append (list (cons 'itype itypes*) (cons 'otype otype*)) attrib-dict)))

  (hash-set! operators name (apply operator (map (curry hash-ref fields) '(itype otype bf ival)))))

(define-syntax define-operator
  (syntax-rules ()
    [(define-operator (name itypes ...) otype [key value] ...)
     (register-operator! 'name '(itypes ...) 'otype
                         (list (cons 'key value) ...))]
    [(define-operator (name . itype) otype [key value] ...)
     (register-operator! 'name 'itype 'otype
                         (list (cons 'key value) ...))]))

(define-syntax-rule (define-1ary-real-operator name bf-impl ival-impl)
  (define-operator (name real) real
    [bf bf-impl] [ival ival-impl]))

(define-syntax-rule (define-2ary-real-operator name bf-impl ival-impl)
  (define-operator (name real real) real
    [bf bf-impl] [ival ival-impl]))

(define-syntax-rule (define-1ary-real-operators [name bf-impl ival-impl] ...)
  (begin (define-1ary-real-operator name bf-impl ival-impl) ...))

(define-syntax-rule (define-2ary-real-operators [name bf-impl ival-impl] ...)
  (begin (define-2ary-real-operator name bf-impl ival-impl) ...))

(define (bfcopysign x y)
  (bf* (bfabs x) (bf (expt -1 (bigfloat-signbit y)))))

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define (bffdim x y)
  (if (bf> x y) (bf- x y) 0.bf))

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define (bfremainder x mod)
  (bf- x (bf* (bfround (bf/ x mod)) mod)))

(define-1ary-real-operators
 [neg bf- ival-neg]
 [acos bfacos ival-acos]
 [acosh bfacosh ival-acosh]
 [asin bfasin ival-asin]
 [asinh bfasinh ival-asinh]
 [atan bfatan ival-atan]
 [atanh bfatanh ival-atanh]
 [cbrt bfcbrt ival-cbrt]
 [ceil bfceiling ival-ceil]
 [cos bfcos ival-cos]
 [cosh bfcosh ival-cosh]
 [erf bferf ival-erf]
 [erfc bferfc ival-erfc]
 [exp bfexp ival-exp]
 [exp2 bfexp2 ival-exp2]
 [expm1 bfexpm1 ival-expm1]
 [fabs bfabs ival-fabs]
 [floor bffloor ival-floor]
 [j0 bfbesj0 #f]
 [j1 bfbesj1 #f]
 [lgamma bflog-gamma #f]
 [log bflog ival-log]
 [log10 bflog10 ival-log10]
 [log1p bflog1p ival-log1p]
 [log2 bflog2 ival-log2]
 [logb bflogb ival-logb]
 [rint bfrint ival-rint]
 [round bfround ival-round]
 [sin bfsin ival-sin]
 [sinh bfsinh ival-sinh]
 [sqrt bfsqrt ival-sqrt]
 [tan bftan ival-tan]
 [tanh bftanh ival-tanh]
 [tgamma bfgamma #f]
 [trunc bftruncate ival-trunc]
 [y0 bfbesy0 #f]
 [y1 bfbesy1 #f])

(define-2ary-real-operators
 [+ bf+ ival-add]
 [- bf- ival-sub]
 [* bf* ival-mult]
 [/ bf/ ival-div]
 [atan2 bfatan2 ival-atan2]
 [copysign bfcopysign ival-copysign]
 [fdim bffdim ival-fdim]
 [fmax bfmax ival-fmax]
 [fmin bfmin ival-fmin]
 [fmod bffmod ival-fmod]
 [hypot bfhypot ival-hypot]
 [pow bfexpt ival-pow]
 [remainder bfremainder ival-remainder])

(define-operator (fma real real real) real
 [bf bffma] [ival ival-fma])

(define (operator-exists? op)
  (hash-has-key? operators op))

;; Operator implementations

(struct operator-impl (itype otype bf fl ival))
(define operator-impls (make-hasheq))

(define parametric-operators (hash))
(define parametric-operators-reverse (hash))

(define/contract (operator-info operator field)
  (-> symbol? (or/c 'itype 'otype 'bf 'fl 'ival) any/c)
  (unless (hash-has-key? operator-impls operator)
    (error 'operator-info "Unknown operator ~a" operator))
  (define accessor
    (match field
      ['itype operator-impl-itype]
      ['otype operator-impl-otype]
      ['bf operator-impl-bf]
      ['fl operator-impl-fl]
      ['ival operator-impl-ival]))
  (accessor (hash-ref operator-impls operator)))

(define/contract (operator-remove! operator)
  (-> symbol? any/c)
  (hash-remove! operator-impls operator))

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
  (define op (hash-ref operators operator))
  (define default-attrib
    (list (cons 'itype (operator-itype op))
          (cons 'otype (operator-otype op))
          (cons 'bf (operator-bf op))
          (cons 'ival (operator-ival op))))
  (unless default-attrib
    (error 'register-operator-impl! "Real operator does not exist: ~a" operator))
  ;; merge inherited and explicit attributes
  (define attrib-dict* (dict-merge default-attrib attrib-dict))
  (define itypes (dict-ref attrib-dict 'itype atypes))
  (define otype (dict-ref attrib-dict 'otype rtype))
  (unless (equal? operator 'if) ;; if does not work here
    (check-operator-types! default-attrib itypes otype))
  ;; Convert attributes to hash, update tables
  (define fields (make-hasheq attrib-dict*))
  (hash-set! fields 'itype itypes)
  (hash-set! fields 'otype otype)
  (hash-set! operator-impls name (apply operator-impl (map (curry hash-ref fields) '(itype otype bf fl ival))))
  (set! parametric-operators
    (hash-update parametric-operators operator
                 (curry cons (list* name otype (operator-info name 'itype)))
                 '()))
  (set! parametric-operators-reverse
    (hash-set parametric-operators-reverse name operator)))
  

(define-syntax define-operator-impl
  (syntax-rules ()
    [(define-operator-impl (operator name atypes ...) rtype [key value] ...)
     (register-operator-impl! 'operator 'name '(atypes ...) 'rtype (list (cons 'key value) ...))]
    [(define-operator-impl (operator name . atype) rtype [key value] ...)
     (register-operator-impl! 'operator 'name 'atype 'rtype (list (cons 'key value) ...))]))

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
  (let ([itypes (operator-itype (hash-ref operators op))])
    (if (type-name? itypes) #f (length itypes))))

(define *unknown-ops* (make-parameter '()))

(register-reset
 (λ ()
   (unless (flag-set? 'precision 'fallback)
     (for-each operator-remove! (*unknown-ops*)))))

;; real operators
(define-operator (== . real) real
  [bf (comparator bf=)] [ival ival-==])

(define-operator (!= . real) real
  [bf (negate (comparator bf=))] [ival ival-!=])

(define-operator (< . real) real
  [bf (comparator bf<)] [ival ival-<])

(define-operator (> . real) real
  [bf (comparator bf>)] [ival ival->])

(define-operator (<= . real) real
  [bf (comparator bf<=)] [ival ival-<=])

(define-operator (>= . real) real
  [bf (comparator bf>=)] [ival ival->=])

;; logical operators ;;

(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define-operator (not bool) bool
  [bf not] [ival ival-not])

(define-operator (and . bool) bool
  [bf and-fn] [ival ival-and])

(define-operator (or . bool) bool
  [bf or-fn] [ival ival-or])

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
  [bf identity] [ival identity])

;; Expression predicates ;;

(define (operator-or-impl? op)
  (and (symbol? op) (not (equal? op 'if))
       (or (hash-has-key? parametric-operators op)
           (hash-has-key? operator-impls op))))

(define (constant-or-impl? var)
  (and (symbol? var)
       (or (hash-has-key? parametric-constants var) 
           (hash-has-key? constant-impls var))))

(define (variable? var)
  (and (symbol? var) (not (constant-or-impl? var))))

;; name -> (vars repr body)
(define *functions* (make-parameter (make-hasheq)))

(define (register-function! name args repr body)
  (hash-set! (*functions*) name (list args repr body)))
