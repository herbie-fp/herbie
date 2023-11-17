#lang racket

(require math/bigfloat "../rival.rkt")
(require "../errors.rkt" "types.rkt")

(provide (rename-out [operator-or-impl? operator?])
         variable? constant-operator?
         operator-exists? operator-deprecated? impl-exists?
         operator-info impl-info 
         impl->operator all-operators all-constants operator-all-impls
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

(struct operator (name itype otype ival deprecated))

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
  (define field-names '(itype otype ival deprecated))
  (hash-set! operators name (apply operator name (map (curry hash-ref fields) field-names)))
  (hash-set! operators-to-impls name '()))

(define-syntax-rule (define-operator (name itypes ...) otype [key value] ...)
  (register-operator! 'name '(itypes ...) 'otype (list (cons 'key value) ...)))

(define-syntax-rule (define-1ary-real-operator name ival-impl)
  (define-operator (name real) real [ival ival-impl]))

(define-syntax-rule (define-2ary-real-operator name ival-impl)
  (define-operator (name real real) real [ival ival-impl]))

(define-syntax-rule (define-1ary-real-operators [name ival-impl] ...)
  (begin (define-1ary-real-operator name ival-impl) ...))

(define-syntax-rule (define-2ary-real-operators [name ival-impl] ...)
  (begin (define-2ary-real-operator name ival-impl) ...))

(define-1ary-real-operators
  [neg ival-neg]
  [acos ival-acos]
  [acosh ival-acosh]
  [asin ival-asin]
  [asinh ival-asinh]
  [atan ival-atan]
  [atanh ival-atanh]
  [cbrt ival-cbrt]
  [ceil ival-ceil]
  [cos ival-cos]
  [cosh ival-cosh]
  [erf ival-erf]
  [erfc ival-erfc]
  [exp ival-exp]
  [exp2 ival-exp2]
  [expm1 ival-expm1]
  [fabs ival-fabs]
  [floor ival-floor]
  [lgamma ival-lgamma]
  [log ival-log]
  [log10 ival-log10]
  [log1p ival-log1p]
  [log2 ival-log2]
  [logb ival-logb]
  [rint ival-rint]
  [round ival-round]
  [sin ival-sin]
  [sinh ival-sinh]
  [sqrt ival-sqrt]
  [tan ival-tan]
  [tanh ival-tanh]
  [tgamma ival-tgamma]
  [trunc ival-trunc])

(define-2ary-real-operators
  [+ ival-add]
  [- ival-sub]
  [* ival-mult]
  [/ ival-div]
  [atan2 ival-atan2]
  [copysign ival-copysign]
  [fdim ival-fdim]
  [fmax ival-fmax]
  [fmin ival-fmin]
  [fmod ival-fmod]
  [hypot ival-hypot]
  [pow ival-pow]
  [remainder ival-remainder])

(define-operator (fma real real real) real
  [ival ival-fma])

;; Operator implementations

(struct operator-impl (name op itype otype fl))
(define operator-impls (make-hasheq))

(define/contract (operator-info operator field)
  (-> symbol? (or/c 'itype 'otype 'ival) any/c)
  (unless (hash-has-key? operators operator)
    (raise-herbie-missing-error "Unknown operator ~a" operator))
  (define accessor
    (match field
      ['itype operator-itype]
      ['otype operator-otype]
      ['ival operator-ival]))
  (accessor (hash-ref operators operator)))

(define/contract (impl-info operator field)
  (-> symbol? (or/c 'itype 'otype 'fl) any/c)
  (unless (hash-has-key? operator-impls operator)
    (error 'impl-info "Unknown operator ~a" operator))
    ; (raise-herbie-missing-error "Unknown operator ~a" operator))
  (define accessor
    (match field
      ['itype operator-impl-itype]
      ['otype operator-impl-otype]
      ['fl operator-impl-fl]))
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

  (unless (equal? operator 'if) ;; Type check all operators except if
    (for ([arepr (cons rrepr areprs)]
          [itype (cons (operator-otype op) (operator-itype op))])
      (unless (equal? (representation-type arepr) itype)
        (raise-herbie-missing-error
          "Cannot register ~a as implementation of ~a: ~a is not a representation of ~a"
          name operator (representation-name rrepr) (operator-otype op)))))

  (define impl (operator-impl name op areprs rrepr fl-fun))
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
               #:when (equal? (impl-info impl 'itype) actual-types))
     impl)
   (raise-herbie-missing-error
    "Parametric operator (~a ~a) not found"
    name
    (string-join (map (λ (r) (format "<~a>" (representation-name r))) actual-types) " "))))

(define (get-parametric-constant name repr)
  (let/ec k
    (for/list ([impl (operator-all-impls name)])
      (define rtype (impl-info impl 'otype))
      (when (or (equal? rtype repr) (equal? (representation-type rtype) 'bool))
        (k impl)))
      (raise-herbie-missing-error
        "Could not find constant implementation for ~a at ~a"
        name (representation-name repr))))

(define (impl->operator name)
  (operator-name (operator-impl-op (hash-ref operator-impls name))))

(define (operator-all-impls name)
  (hash-ref operators-to-impls name))

;; comparators ;;

(define-operator (== real real) bool
  [ival ival-==])

(define-operator (!= real real) bool
  [ival ival-!=])

(define-operator (< real real) bool
  [ival ival-<])

(define-operator (> real real) bool
  [ival ival->])

(define-operator (<= real real) bool
  [ival ival-<=])

(define-operator (>= real real) bool
  [ival ival->=])

;; logical operators ;;

(define-operator (not bool) bool
   [ival ival-not])

(define-operator (and bool bool) bool
   [ival ival-and])

(define-operator (or bool bool) bool
   [ival ival-or])

;; Miscellaneous operators ;;

(define (repr-conv? expr)
  (and (symbol? expr) (set-member? (operator-all-impls 'cast) expr)))

(define (rewrite-repr-op? expr)
  (and (symbol? expr) (set-member? (operator-all-impls 'convert) expr)))

(define (get-repr-conv irepr orepr)
  (for/or ([name (operator-all-impls 'cast)])
    (and (equal? (impl-info name 'otype) orepr)
         (equal? (first (impl-info name 'itype)) irepr)
         name)))

(define (get-rewrite-operator repr)
  (for/or ([name (operator-all-impls 'convert)])
    (and (equal? (impl-info name 'itype) (list repr))
         name)))

(define-operator (PI) real
  [ival ival-pi])

(define-operator (E) real
  [ival ival-e])

(define-operator (INFINITY) real
  [ival (λ () (ival +inf.bf))])

(define-operator (NAN) real
  [ival (λ () (ival +nan.bf))])

(define-operator (TRUE) bool
  [ival (const (ival-bool true))])

(define-operator (FALSE) bool
  [ival (const (ival-bool false))])

;; Conversions

(define-operator (convert real) real
  [ival identity])

(define-operator (cast real) real
  [ival identity])

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

(define (all-operators)
  (sort (hash-keys operators) symbol<?))

(define (all-constants)
  (for/list ([(name rec) (in-hash operators)]
             #:when (= (length (operator-itype rec)) 0))
    name))
