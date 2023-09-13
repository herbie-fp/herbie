#lang racket

(require math/bigfloat rival)
(require "../arb.rkt")
(require "../common.rkt" "../errors.rkt" "types.rkt")

(provide (rename-out [operator-or-impl? operator?])
         variable? constant-operator?
         operator-exists? operator-deprecated? impl-exists?
         real-operator-info operator-info 
         impl->operator all-constants operator-all-impls
         *functions* register-function!
         get-parametric-operator get-parametric-constant
         generate-conversion-impl!
         repr-conv? rewrite-repr-op?
         get-repr-conv get-rewrite-operator)

(module+ internals 
  (provide define-operator-impl
           register-operator-impl!
           define-operator
           register-operator!
           register-conversion-generator!))

;; Real operator table
;; Implementations inherit attributes

(struct operator (name itype otype bf arb deprecated))

(define operators (make-hasheq))
(define operators-to-impls (make-hasheq))

(define (operator-exists? op)
  (hash-has-key? operators op))

(define (operator-deprecated? op)
  (operator-deprecated (hash-ref operators op)))

(define (register-operator! name itypes otype attrib-dict)
  (define override-dict
    (list
      (cons 'itype (dict-ref attrib-dict 'itype itypes))
      (cons 'otype (dict-ref attrib-dict 'otype otype))
      (cons 'deprecated (dict-ref attrib-dict 'deprecated #f))))
  (define fields (make-hasheq (append attrib-dict override-dict)))
  (define field-names '(itype otype bf arb deprecated))
  (hash-set! operators name (apply operator name (map (curry hash-ref fields) field-names)))
  (hash-set! operators-to-impls name '()))

(define-syntax-rule (define-operator (name itypes ...) otype [key value] ...)
  (register-operator! 'name '(itypes ...) 'otype (list (cons 'key value) ...)))

(define-syntax-rule (define-1ary-real-operator name bf-impl arb-impl)
  (define-operator (name real) real
    [bf bf-impl] [arb arb-impl]))

(define-syntax-rule (define-2ary-real-operator name bf-impl arb-impl)
  (define-operator (name real real) real
    [bf bf-impl] [arb arb-impl]))

(define-syntax-rule (define-1ary-real-operators [name bf-impl arb-impl] ...)
  (begin (define-1ary-real-operator name bf-impl arb-impl) ...))

(define-syntax-rule (define-2ary-real-operators [name bf-impl arb-impl] ...)
  (begin (define-2ary-real-operator name bf-impl arb-impl) ...))

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
 [neg bf- arb-neg]
 [acos bfacos arb-acos]
 [acosh bfacosh arb-acosh]
 [asin bfasin arb-asin]
 [asinh bfasinh arb-asinh]
 [atan bfatan arb-atan]
 [atanh bfatanh arb-atanh]
 [cbrt bfcbrt arb-cbrt]
 [ceil bfceiling arb-ceil]
 [cos bfcos arb-cos]
 [cosh bfcosh arb-cosh]
 [erf bferf arb-erf]
 [erfc bferfc arb-erfc]
 [exp bfexp arb-exp]
 [exp2 bfexp2 arb-exp2]
 [expm1 bfexpm1 arb-expm1]
 [fabs bfabs arb-fabs]
 [floor bffloor arb-floor]
 [lgamma bflog-gamma arb-lgamma]
 [log bflog arb-log]
 [log10 bflog10 arb-log10]
 [log1p bflog1p arb-log1p]
 [log2 bflog2 arb-log2]
 [logb bflogb arb-logb]
 [rint bfrint arb-rint]
 [round bfround arb-round]
 [sin bfsin arb-sin]
 [sinh bfsinh arb-sinh]
 [sqrt bfsqrt arb-sqrt]
 [tan bftan arb-tan]
 [tanh bftanh arb-tanh]
 [tgamma bfgamma arb-tgamma]
 [trunc bftruncate arb-trunc])
 
(define-2ary-real-operators
 [+ bf+ arb-add]
 [- bf- arb-sub]
 [* bf* arb-mul]
 [/ bf/ arb-div]
 [atan2 bfatan2 arb-atan2]
 [copysign bfcopysign arb-copysign]
 [fdim bffdim arb-fdim]
 [fmax bfmax arb-fmax]
 [fmin bfmin arb-fmin]
 [fmod bffmod arb-fmod]
 [hypot bfhypot arb-hypot]
 [pow bfexpt arb-pow]
 [remainder bfremainder arb-remainder])

(define-operator (fma real real real) real
 [bf bffma] [arb arb-fma])

;; Deprecated operators

(module hairy racket/base
  (require ffi/unsafe)
  (provide check-native-1ary-exists?)

  (define (check-native-1ary-exists? op)
    (let ([f32-name (string->symbol (string-append (symbol->string op) "f"))])
      (or (get-ffi-obj op #f (_fun _double -> _double) (λ () #f))
          (get-ffi-obj f32-name #f (_fun _float -> _float) (λ () #f)))))
)

(require (submod "." hairy))

;; Operator implementations

(struct operator-impl (name op itype otype fl bf arb))
(define operator-impls (make-hasheq))

(define/contract (real-operator-info operator field)
  (-> symbol? (or/c 'itype 'otype 'bf 'fl 'arb) any/c)
  (unless (hash-has-key? operators operator)
    (raise-herbie-missing-error "Unknown operator ~a" operator))
  (define accessor
    (match field
      ['itype operator-itype]
      ['otype operator-otype]
      ['bf operator-bf]
      ['arb operator-arb]))
      ;;['ival operator-ival]))
  (accessor (hash-ref operators operator)))

(define/contract (operator-info operator field)
  (-> symbol? (or/c 'itype 'otype 'bf 'fl 'arb) any/c)
  (unless (hash-has-key? operator-impls operator)
    (error 'operator-info "Unknown operator ~a" operator))
    ; (raise-herbie-missing-error "Unknown operator ~a" operator))
  (define accessor
    (match field
      ['itype operator-impl-itype]
      ['otype operator-impl-otype]
      ['bf operator-impl-bf]
      ['fl operator-impl-fl]
      ['arb operator-impl-arb]))
      ;;['ival operator-impl-ival]))
  (accessor (hash-ref operator-impls operator)))

(define/contract (operator-remove! operator)
  (-> symbol? any/c)
  (hash-remove! operator-impls operator))

(define (register-operator-impl! operator name areprs rrepr attrib-dict)
  (unless (hash-has-key? operators operator)
    (raise-herbie-missing-error
      "Cannot register ~a as implementation of ~a: no such operator"
      name operator))

  (define op (hash-ref operators operator))
  (define fl-fun (dict-ref attrib-dict 'fl))
  (define bf-fun (dict-ref attrib-dict 'bf (λ () (operator-bf op))))
  (define arb-fun (dict-ref attrib-dict 'arb (λ () (operator-arb op))))
  ;;(define ival-fun (dict-ref attrib-dict 'ival (λ () (operator-ival op))))

  (unless (equal? operator 'if) ;; Type check all operators except if
    (for ([arepr (cons rrepr areprs)]
          [itype (cons (operator-otype op) (operator-itype op))])
      (unless (equal? (representation-type arepr) itype)
        (raise-herbie-missing-error
          "Cannot register ~a as implementation of ~a: ~a is not a representation of ~a"
          name operator (representation-name rrepr) (operator-otype op)))))

  (define impl (operator-impl name op areprs rrepr fl-fun bf-fun arb-fun))
  (hash-set! operator-impls name impl)
  (hash-update! operators-to-impls operator (curry cons name)))


(define-syntax-rule (define-operator-impl (operator name atypes ...) rtype [key value] ...)
  (register-operator-impl! 'operator 'name
                           (list (get-representation 'atypes) ...)
                           (get-representation 'rtype)
                           (list (cons 'key value) ...)))

(define (get-parametric-operator name . actual-types)
  (or
   (for/first ([impl (operator-all-impls name)]
               #:when (equal? (operator-info impl 'itype) actual-types))
     impl)
   (raise-herbie-missing-error
    "Parametric operator (~a ~a) not found"
    name
    (string-join (map (λ (r) (format "<~a>" (representation-name r))) actual-types) " "))))

(define (get-parametric-constant name repr)
  (let/ec k
    (for/list ([impl (operator-all-impls name)])
      (define rtype (operator-info impl 'otype))
      (when (or (equal? rtype repr) (equal? (representation-type rtype) 'bool))
        (k impl)))
      (raise-herbie-missing-error
        "Could not find constant implementation for ~a at ~a"
        name (representation-name repr))))

(define (impl->operator name)
  (operator-name (operator-impl-op (hash-ref operator-impls name))))

(define (operator-all-impls name)
  (hash-ref operators-to-impls name))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

;; real operators
(define-operator (== real real) bool
  [bf (comparator bf=)] [arb arb-==])

(define-operator (!= real real) bool
  [bf (negate (comparator bf=))] [arb arb-!=])

(define-operator (< real real) bool
  [bf (comparator bf<)] [arb arb-<])

(define-operator (> real real) bool
  [bf (comparator bf>)] [arb arb->])

(define-operator (<= real real) bool
  [bf (comparator bf<=)] [arb arb-<=])

(define-operator (>= real real) bool
  [bf (comparator bf>=)] [arb arb->=])

;; logical operators ;;

(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define-operator (not bool) bool
  [bf not] [arb arb-not])

(define-operator (and bool bool) bool
  [bf and-fn] [arb arb-and])

(define-operator (or bool bool) bool
  [bf or-fn] [arb arb-or])

;; Miscellaneous operators ;;

(define (repr-conv? expr)
  (and (symbol? expr) (set-member? (operator-all-impls 'cast) expr)))

(define (rewrite-repr-op? expr)
  (and (symbol? expr) (set-member? (operator-all-impls 'convert) expr)))

(define (get-repr-conv irepr orepr)
  (for/or ([name (operator-all-impls 'cast)])
    (and (equal? (operator-info name 'otype) orepr)
         (equal? (first (operator-info name 'itype)) irepr)
         name)))

(define (get-rewrite-operator repr)
  (for/or ([name (operator-all-impls 'convert)])
    (and (equal? (operator-info name 'itype) (list repr))
         name)))

(define-operator (PI) real
  [bf (λ () pi.bf)] 
  [arb arb-pi])

(define-operator (E) real
  [bf (λ () (bfexp 1.bf))]
  [arb arb-e])

(define-operator (INFINITY) real
  [bf (λ () +inf.bf)]
  [arb (λ () (arb +inf.bf))])

(define-operator (NAN) real
  [bf (λ () +nan.bf)]
  [arb (λ () (arb +nan.bf))])

(define-operator (TRUE) bool
  [bf (const true)]
  [arb (const (arb-bool true))])

(define-operator (FALSE) bool
  [bf (const false)]
  [arb (const (arb-bool false))])

(define (dict-merge dict dict2)
  (for/fold ([dict dict]) ([(key value) (in-dict dict2)])
    (dict-set dict key value)))

;; Conversions

(define-operator (convert real) real
  [bf identity] [arb identity])

(define-operator (cast real) real
  [bf identity] [arb identity])

; Similar to representation generators, conversion generators
; allow Herbie to query plugins for optimized implementations
; of representation conversions, rather than the default
; bigfloat implementation
(define conversion-generators '())

(define/contract (register-conversion-generator! proc)
  (-> (-> any/c any/c boolean?) void?)
  (unless (set-member? conversion-generators proc)
    (set! conversion-generators (cons proc conversion-generators))))

(define (generate-conversion-impl! conv1 conv2 repr1 repr2)
  (or (impl-exists? conv1)
      (impl-exists? conv2)
      (for ([generate conversion-generators])
        (generate (representation-name repr1) (representation-name repr2)))))

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
                (null? (operator-impl-itype (hash-ref operator-impls op)))))))

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
