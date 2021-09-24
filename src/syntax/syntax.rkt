#lang racket

(require math/flonum math/base math/bigfloat math/special-functions rival)
(require "../common.rkt" "../interface.rkt" "../errors.rkt" "types.rkt")

(provide (rename-out [operator-or-impl? operator?])
         variable? constant-operator? operator-exists? impl-exists?
         real-operator-info operator-info 
         impl->operator all-constants operator-all-impls
         *functions* register-function!
         get-parametric-operator
         repr-conv? rewrite-repr-op? get-repr-conv)

(module+ internals 
  (provide define-operator-impl
           register-operator-impl!
           define-operator
           register-operator!))

;; Abstract operator table
;; Implementations inherit attributes

(struct operator (name itype otype bf ival))
(define operators (make-hasheq))

(define (register-operator! name itypes otype attrib-dict)
  (define itypes* (dict-ref attrib-dict 'itype itypes))
  (define otype* (dict-ref attrib-dict 'otype otype))
  (define fields (make-hasheq (append (list (cons 'itype itypes*) (cons 'otype otype*)) attrib-dict)))

  (hash-set! operators name (apply operator name (map (curry hash-ref fields) '(itype otype bf ival)))))

(define-syntax-rule (define-operator (name itypes ...) otype [key value] ...)
  (register-operator! 'name '(itypes ...) 'otype (list (cons 'key value) ...)))

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

(struct operator-impl (name op itype otype fl))
(define operator-impls (make-hasheq))

(define operators-to-impls (make-hasheq))

(define/contract (real-operator-info operator field)
  (-> symbol? (or/c 'itype 'otype 'bf 'fl 'ival) any/c)
  (unless (hash-has-key? operators operator)
    (error 'real-operator-info "Unknown operator ~a" operator))
  (define accessor
    (match field
      ['itype operator-itype]
      ['otype operator-otype]
      ['bf operator-bf]
      ['ival operator-ival]))
  (accessor (hash-ref operators operator)))

(define/contract (operator-info operator field)
  (-> symbol? (or/c 'itype 'otype 'bf 'fl 'ival) any/c)
  (unless (hash-has-key? operator-impls operator)
    (error 'operator-info "Unknown operator ~a" operator))
  (define accessor
    (match field
      ['itype operator-impl-itype]
      ['otype operator-impl-otype]
      ['bf (compose operator-bf operator-impl-op)]
      ['fl operator-impl-fl]
      ['ival (compose operator-ival operator-impl-op)]))
  (accessor (hash-ref operator-impls operator)))

(define/contract (operator-remove! operator)
  (-> symbol? any/c)
  (hash-remove! operator-impls operator))

(define (register-operator-impl! operator name atypes rtype attrib-dict)
  (define areprs (map get-representation atypes))
  (define rrepr (get-representation rtype))

  (unless (hash-has-key? operators operator)
    (error 'register-operator-impl!
           "Cannot register ~a as implementation of ~a: no such operator"
           name operator))


  (define op (hash-ref operators operator))
  (define fl-fun (dict-ref attrib-dict 'fl))

  (unless (equal? operator 'if) ;; Type check all operators except if
    (for ([arepr (cons rrepr areprs)]
          [itype (cons (operator-otype op) (operator-itype op))])
      (unless (equal? (representation-type arepr) itype)
        (error 'register-operator-impl!
               "Cannot register ~a as implementation of ~a: ~a is not a representation of ~a"
               name operator rrepr (operator-otype op)))))

  (hash-set! operator-impls name (operator-impl name op areprs rrepr fl-fun))
  (hash-update! operators-to-impls operator (curry cons name) '()))
  

(define-syntax-rule (define-operator-impl (operator name atypes ...) rtype [key value] ...)
  (register-operator-impl! 'operator 'name '(atypes ...) 'rtype (list (cons 'key value) ...)))

(define (get-parametric-operator name #:fail-fast? [fail-fast? #t] . actual-types)
  (or
    (for/or ([impl (operator-all-impls name)])
      (define atypes (operator-info impl 'itype))
      (and (equal? atypes actual-types) impl))
    (and fail-fast?
         (error 'get-parametric-operator
                "parametric operator with op ~a and input types ~a not found"
                name actual-types))))

(define (impl->operator name)
  (operator-name (operator-impl-op (hash-ref operator-impls name))))

(define (operator-all-impls name)
  (hash-ref operators-to-impls name))

;; real operators
(define-operator (== real real) bool
  [bf (comparator bf=)] [ival ival-==])

(define-operator (!= real real) bool
  [bf (negate (comparator bf=))] [ival ival-!=])

(define-operator (< real real) bool
  [bf (comparator bf<)] [ival ival-<])

(define-operator (> real real) bool
  [bf (comparator bf>)] [ival ival->])

(define-operator (<= real real) bool
  [bf (comparator bf<=)] [ival ival-<=])

(define-operator (>= real real) bool
  [bf (comparator bf>=)] [ival ival->=])

;; logical operators ;;

(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define-operator (not bool) bool
  [bf not] [ival ival-not])

(define-operator (and bool bool) bool
  [bf and-fn] [ival ival-and])

(define-operator (or bool bool) bool
  [bf or-fn] [ival ival-or])

;; Miscellaneous operators ;;

(define (repr-conv? expr)
  (and (symbol? expr) (regexp-match? #px"^[\\S]+(->)[\\S]+$" (symbol->string expr))))

(define (rewrite-repr-op? expr)
  (and (symbol? expr) (regexp-match? #px"^(<-)[\\S]+$" (symbol->string expr))))

(define (get-repr-conv irepr orepr)
  (for/or ([name (operator-all-impls 'cast)])
    (and (repr-conv? name)
         (equal? (operator-info name 'otype) orepr)
         (equal? (first (operator-info name 'itype)) irepr)
         name)))

(define-operator (PI) real
  [bf (λ () pi.bf)] 
  [ival ival-pi])

(define-operator (E) real
  [bf (λ () (bfexp 1.bf))]
  [ival ival-e])

(define-operator (INFINITY) real
  [bf (λ () +inf.bf)]
  [ival (λ () (mk-ival +inf.bf))])

(define-operator (NAN) real
  [bf (λ () +nan.bf)]
  [ival (λ () (mk-ival +nan.bf))])

(define-operator (TRUE) bool
  [bf (const true)]
  [ival (const (ival-bool true))])

(define-operator (FALSE) bool
  [bf (const false)]
  [ival (const (ival-bool false))])

(define (dict-merge dict dict2)
  (for/fold ([dict dict]) ([(key value) (in-dict dict2)])
    (dict-set dict key value)))

;; Conversions

(define-operator (cast real) real
  [bf identity] [ival identity])

;; Expression predicates ;;

(define (impl-exists? op)
  (hash-has-key? operator-impls op))

(define (operator-or-impl? op)
  (and (symbol? op) (not (equal? op 'if))
       (or (hash-has-key? operators op)
           (hash-has-key? operator-impls op))))

(define (constant-operator? op)
  (and (symbol? op)
       (or (and (hash-has-key? operators op) 
                (null? (operator-itype (hash-ref operators op))))
           (and (hash-has-key? operator-impls op)
                (null? (operator-itype (hash-ref operator-impls op)))))))

(define (variable? var)
  (and (symbol? var)
       (or (not (hash-has-key? operators var))
           (not (null? (operator-itype (hash-ref operators var)))))
       (or (not (hash-has-key? operator-impls var))
           (not (null? (operator-impl-itype (hash-ref operator-impls var)))))))

;; name -> (vars repr body)
(define *functions* (make-parameter (make-hasheq)))

(define (register-function! name args repr body)
  (hash-set! (*functions*) name (list args repr body)))

(define (all-constants)
  (for/list ([(name rec) (in-hash operators)]
             #:when (= (length (operator-itype rec)) 0))
    name))
