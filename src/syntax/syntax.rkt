#lang racket

(require math/bigfloat rival)
(require "../errors.rkt" "types.rkt")

(provide (rename-out [operator-or-impl? operator?])
         (struct-out literal)
         variable? constant-operator?
         operator-exists? operator-deprecated? operator-accelerator?
         operator-info all-operators all-constants all-accelerators
         impl-exists? impl-info impl->operator operator-all-impls
         operator-active-impls activate-operator-impl! clear-active-operator-impls!
         *functions* register-function!
         get-parametric-operator get-parametric-constant
         generate-conversion-impl repr-conv? rewrite-repr-op?
         get-repr-conv get-rewrite-operator)

(module+ internals 
  (provide define-operator-impl
           register-operator-impl!
           define-operator
           register-operator!
           register-conversion-generator!
           variable?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real operators
;; Pure mathematical operations

;; A real operator requires
;;  - a (unique) name
;;  - input and output types
;;  - exactly one of:
;;     - a Rival interval implementation
;;     - a specification ["accelerator"]
;;  - deprectated? flag [#f by default]
;; Operator implementations _implement_ a real operator
;; for a particular set of input and output representations.
(struct operator (name itype otype ival spec deprecated))

;; All real operators
(define operators (make-hasheq))

;; Checks if an operator has been registered.
(define (operator-exists? op)
  (hash-has-key? operators op))

;; Checks if an operator has been registered as deprecated.
(define (operator-deprecated? op)
  (operator-deprecated (hash-ref operators op)))

;; Checks if an operator is an ``accelerator``.
(define (operator-accelerator? op)
  (and (hash-has-key? operators op)
       (operator-spec (hash-ref operators op))))

;; Returns all operators.
(define (all-operators)
  (sort (hash-keys operators) symbol<?))

;; Returns all constant operators (operators with no arguments).
(define (all-constants)
  (sort
    (for/list ([(name rec) (in-hash operators)]
               #:when (null? (operator-itype rec)))
      name)
    symbol<?))

;; Returns all ``accelerator`` operators
(define (all-accelerators)
  (sort
    (for/list ([(name rec) (in-hash operators)]
               #:when (operator-spec rec))
      name)
    symbol<?))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (operator-info op field)
  (-> symbol? (or/c 'itype 'otype 'ival 'spec) any/c)
  (unless (hash-has-key? operators op)
    (error 'operator-info "Unknown operator ~a" op))
  (define info (hash-ref operators op))
  (case field
    [(itype) (operator-itype info)]
    [(otype) (operator-otype info)]
    [(ival) (operator-ival info)]
    [(spec) (operator-spec info)]))

;; Map from operator to its implementations
(define operators-to-impls (make-hasheq))

;; All implementations of an operator `op`.
;; Panics if the operator is not found.
(define (operator-all-impls op)
  (unless (hash-has-key? operators op)
    (error 'operator-info "Unknown operator ~a" op))
  (hash-ref operators-to-impls op))

;; Registers an operator with an attribute mapping.
;; Panics if an operator with name `name` has already been registered.
;; By default, the input types are specified by `itypes`, the output type
;; is specified by `otype`, and the operator is not deprecated; but
;; `attrib-dict` can override these properties.
(define (register-operator! name itypes otype attrib-dict)
  (when (hash-has-key? operators name)
    (error 'register-operator! "operator already registered: ~a" name))
  ; extract relevant fields
  (define itypes* (dict-ref attrib-dict 'itype itypes))
  (define otype* (dict-ref attrib-dict 'otype otype))
  (define ival-fn (dict-ref attrib-dict 'ival #f))
  (define spec (dict-ref attrib-dict 'spec #f))
  (define deprecated? (dict-ref attrib-dict 'deprecated #f))
  ; check we have the required fields
  (match* (ival-fn spec)
    [(#f #f) (error 'register-operator! "no interval implementation or spec for `~a`" name)]
    [(_ #f) (void)]
    [(#f _) (void)]
    [(_ _) (error 'register-operator! "both interval implementation and spec for `~a` given" name)])
  ; update tables
  (define info (operator name itypes* otype* ival-fn spec deprecated?))
  (hash-set! operators name info)
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
  [exp ival-exp]
  [exp2 ival-exp2]
  [fabs ival-fabs]
  [floor ival-floor]
  [lgamma ival-lgamma]
  [log ival-log]
  [log10 ival-log10]
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
  [pow ival-pow]
  [remainder ival-remainder])

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

(define-operator (PI) real
  [ival ival-pi])

(define-operator (E) real
  [ival ival-e])

(define-operator (INFINITY) real
  [ival (λ () (ival (bfprev +inf.bf) +inf.bf))])

(define-operator (NAN) real
  [ival (λ () ival-illegal)])

(define-operator (TRUE) bool
  [ival (const (ival-bool true))])

(define-operator (FALSE) bool
  [ival (const (ival-bool false))])

;; Conversions

(define-operator (convert real) real
  [ival identity])

(define-operator (cast real) real
  [ival identity])

;; Accelerators

(define-operator (expm1 real) real
  [spec '(lambda (x) (- (exp x) 1))])

(define-operator (log1p real) real
  [spec '(lambda (x) (log (+ 1 x)))])

(define-operator (hypot real real) real
  [spec '(lambda (x y) (sqrt (+ (* x x) (* y y))))])

(define-operator (fma real real real) real
  [spec '(lambda (x y z) (+ (* x y) z))])

(define-operator (erfc real) real
  [spec '(lambda (x) (- 1 (erf x)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator implementations
;; Floating-point operations that approximate mathematical operations

;; Operator implementations
;; An "operator implementation" implements a mathematical operator for
;; a particular set of representations satisfying the types described
;; by the `itype` and `otype` properties of the operator.
(struct operator-impl (name op itype otype fl))

;; Operator implementation table
;; Tracks implementations that are loaded into Racket's runtime
(define operator-impls (make-hasheq))

;; "Active" operator set
;; Tracks implementations that will be used by Herbie during the improvement loop.
;; Guaranteed to be a subset of the `operator-impls` table.
(define active-operator-impls (mutable-set))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (impl-info operator field)
  (-> symbol? (or/c 'itype 'otype 'fl) any/c)
  (unless (hash-has-key? operator-impls operator)
    (error 'impl-info "Unknown operator implementation ~a" operator))
  (define accessor
    (match field
      ['itype operator-impl-itype]
      ['otype operator-impl-otype]
      ['fl operator-impl-fl]))
  (accessor (hash-ref operator-impls operator)))

;; Like `operator-all-impls`, but filters for only active implementations.
(define (operator-active-impls name)
  (filter (curry set-member? active-operator-impls)
          (operator-all-impls name)))

;; Looks up the name of an operator corresponding to an implementation `name`.
;; Panics if the operator is not found.
(define (impl->operator name)
  (unless (hash-has-key? operator-impls name)
    (raise-herbie-missing-error "Unknown operator implementation ~a" name))
  (define impl (hash-ref operator-impls name))
  (operator-name (operator-impl-op impl)))

;; Activates an implementation.
;; Panics if the operator is not found.
(define (activate-operator-impl! name)
  (unless (hash-has-key? operator-impls name)
    (raise-herbie-missing-error "Unknown operator implementation ~a" name))
  (set-add! active-operator-impls name))

;; Clears the table of active implementations.
(define (clear-active-operator-impls!)
  (set-clear! active-operator-impls))

;; Registers an operator implementation `name` or real operator `op`.
;; The input and output representations must satisfy the types
;; specified by the `itype` and `otype` fields for `op`.
(define (register-operator-impl! operator name areprs rrepr attrib-dict)
  ; Ideally we check for uniqueness, but the loading code may fire multiple times
  ; (unless (hash-has-key? operator-impls name)
  ;   (error 'register-operator-impl! "implementation already registered ~a" name))
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

;; Among active implementations, looks up an implementation with
;; the operator name `name` and argument representations `ireprs`.
(define (get-parametric-operator #:all? [all? #f] name . ireprs)
  (define get-impls (if all? operator-all-impls operator-active-impls))
  (let/ec k
    (for/first ([impl (get-impls name)]
                #:when (equal? (impl-info impl 'itype) ireprs))
      (k impl))
    (raise-herbie-missing-error
      "Could not find operator implementation for ~a with ~a"
      name (string-join (map (λ (r) (format "<~a>" (representation-name r))) ireprs) " "))))

;; Among active implementations, looks up an implementation of
;; a constant (nullary operator) with the operator name `name`
;; and representation `repr`.
(define (get-parametric-constant name repr #:all? [all? #f])
  (define get-impls (if all? operator-all-impls operator-active-impls))
  (let/ec k
    (for ([impl (get-impls name)])
      (define rtype (impl-info impl 'otype))
      (when (or (equal? rtype repr) (equal? (representation-type rtype) 'bool))
        (k impl)))
    (raise-herbie-missing-error
      "Could not find constant implementation for ~a with ~a"
      name (format "<~a>" (representation-name repr)))))

;; Casts and precision changes

(define (repr-conv? expr)
  (and (symbol? expr) (set-member? (operator-all-impls 'cast) expr)))

(define (rewrite-repr-op? expr)
  (and (symbol? expr) (set-member? (operator-all-impls 'convert) expr)))

(define (get-repr-conv irepr orepr #:all? [all? #f])
  (define get-impls (if all? operator-all-impls operator-active-impls))
  (for/or ([name (get-impls 'cast)])
    (and (equal? (impl-info name 'otype) orepr)
         (equal? (first (impl-info name 'itype)) irepr)
         name)))

(define (get-rewrite-operator repr #:all? [all? #f])
  (define get-impls (if all? operator-all-impls operator-active-impls))
  (for/or ([name (get-impls 'convert)])
    (and (equal? (impl-info name 'itype) (list repr))
         name)))

; Similar to representation generators, conversion generators
; allow Herbie to query plugins for optimized implementations
; of representation conversions, rather than the default
; bigfloat implementation
(define conversion-generators '())

(define/contract (register-conversion-generator! proc)
  (-> (-> any/c any/c boolean?) void?)
  (unless (set-member? conversion-generators proc)
    (set! conversion-generators (cons proc conversion-generators))))

(define (generate-conversion-impl irepr orepr)
  (define maybe-impl (get-repr-conv irepr orepr))
  (cond
    [maybe-impl maybe-impl]
    [else
     (for/first ([gen conversion-generators])
        (gen (representation-name irepr) (representation-name orepr)))
     (get-repr-conv irepr orepr)]))

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


;; Floating-point expressions require that number
;; be rounded to a particular precision.
(struct literal (value precision) #:prefab)

;; name -> (vars repr body)	;; name -> (vars prec body)
(define *functions* (make-parameter (make-hasheq)))

(define (register-function! name args repr body)	;; Adds a function definition.
  (hash-set! (*functions*) name (list args repr body)))
