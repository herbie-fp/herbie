#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "types.rkt"
         "platform.rkt"
         "syntax.rkt"
         (except-in "platform-language.rkt" quasisyntax))
(provide assert-program-typed!)

(define (precision->reprs prec-name)
  (define repr (get-representation prec-name))
  (define scalar-repr
    (if (array-representation? repr)
        (array-representation-elem repr)
        repr))
  (values repr scalar-repr (representation-name scalar-repr)))

(define (normalize-prop-dict dict)
  (define-values (_1 _2 scalar-name) (precision->reprs (dict-ref dict ':precision)))
  (dict-set dict ':precision scalar-name))

(define (type->string t)
  (cond
    [(representation? t) (~a (representation-name t))]
    [(array-representation? t)
     (format "array[~a] of ~a"
             (string-join (for/list ([d (array-representation-dims t)])
                            (~a d))
                          " ")
             (type->string (array-representation-elem t)))]
    [else (~a t)]))

(define (flatten-type t)
  (cond
    [(array-representation? t) (values (array-representation-elem t) (array-representation-dims t))]
    [else (values t '())]))

(define (array-of dims elem)
  (unless (and (list? dims) (not (null? dims)) (andmap exact-positive-integer? dims))
    (error 'array-of "Arrays require one or more positive dimensions, got ~a" dims))
  (define repr (make-array-representation #:elem elem #:dims dims))
  (define name (representation-name repr))
  (define existing (and (repr-exists? name) (get-representation name)))
  (or existing repr))

(define (assert-program-typed! stx)
  (define-values (vars props body)
    (match (syntax-e stx)
      [(list (app syntax-e 'FPCore) _ (app syntax-e (list vars ...)) props ... body)
       (values vars props body)]
      [(list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) props ... body)
       (values vars props body)]))

  (define default-dict `((:precision . ,(*default-precision*))))
  (define prop-dict (apply dict-set* default-dict (map syntax->datum props)))
  (define prec (dict-ref prop-dict ':precision))
  (define-values (program-repr scalar-repr _) (precision->reprs prec))
  (define prop-dict* (normalize-prop-dict prop-dict))

  (define-values (var-names var-types)
    (for/lists (var-names var-types)
               ([var (in-list vars)])
               (match (syntax->datum var)
                 [(list '! props ... name dims ...)
                  (define prop-dict (props->dict props))
                  (define arg-prec (dict-ref prop-dict ':precision prec))
                  (define-values (repr base-repr _) (precision->reprs arg-prec))
                  (values name
                          (if (null? dims)
                              repr
                              (array-of dims base-repr)))]
                 [(list (? symbol? name) dims ...)
                  (values name
                          (if (null? dims)
                              program-repr
                              (array-of dims scalar-repr)))]
                 [(? symbol? name) (values name program-repr)])))

  (define ctx (context var-names program-repr var-types))
  (assert-expression-type! body prop-dict* ctx))

(define (assert-expression-type! stx props ctx)
  (define errs '())
  (define (error! stx fmt . args)
    (define args*
      (for/list ([arg (in-list args)])
        (match arg
          [(? representation?) (representation-name arg)]
          [(? array-representation?) (type->string arg)]
          [_ arg])))
    (set! errs (cons (cons stx (apply format fmt args*)) errs)))

  (define repr (expression->type stx props ctx error!))
  (define expected (context-repr ctx))
  (define (comparable? a b)
    (or (and (representation? a) (representation? b))
        (and (array-representation? a) (array-representation? b))))
  (when (and expected (comparable? repr expected) (not (equal? repr expected)))
    (error! stx
            "Expected program of type ~a, got type ~a"
            (type->string expected)
            (type->string repr)))

  (unless (null? errs)
    (raise-herbie-syntax-error "Program has type errors" #:locations errs)))

(define (application->string op types)
  (format "(~a ~a)"
          op
          (string-join (for/list ([t types])
                         (if t
                             (format "<~a>" (type->string t))
                             "<?>"))
                       " ")))

(define (expression->type stx prop-dict ctx error!)
  (define prop-dict* (normalize-prop-dict prop-dict))
  (define (current-repr pd)
    (get-representation (dict-ref pd ':precision)))
  (define (bool-type? t)
    (and (representation? t) (equal? (representation-type t) 'bool)))
  (define (assert-bool who ty)
    (unless (bool-type? ty)
      (error! who "Expected boolean type, got ~a" (type->string ty))))
  (define (array-literal-type elem-types current-dict)
    (cond
      [(null? elem-types)
       (error! stx "Array literal must have at least one element")
       (array-of '(1) (current-repr current-dict))]
      [else
       (define-values (base base-dims) (flatten-type (first elem-types)))
       (for ([t (in-list (rest elem-types))])
         (define-values (elem elem-dims) (flatten-type t))
         (unless (and (equal? elem base) (equal? elem-dims base-dims))
           (error! stx
                   "Array elements have mismatched types: ~a vs ~a"
                   (type->string base)
                   (type->string t))))
       (array-of (cons (length elem-types) base-dims) base)]))
  (define (types-match? a b)
    (equal? a b))
  (define (id->sym x)
    (if (syntax? x)
        (syntax-e x)
        x))

  (let loop ([stx stx]
             [prop-dict prop-dict*]
             [ctx ctx])
    (match stx
      [#`,(? number?) (current-repr prop-dict)]
      [#`,(? operator-exists? op)
       (match (get-fpcore-impl op prop-dict '())
         [#f ; no implementation found
          (error! stx "No implementation of `~a` in platform for context `~a`" op prop-dict)
          (current-repr prop-dict)]
         [impl (impl-info impl 'otype)])]
      [#`,(? symbol? x) (context-lookup ctx x)]
      [#`(let ([,ids #,exprs] ...) #,body)
       (define ctx*
         (for/fold ([ctx* ctx])
                   ([id (in-list ids)]
                    [expr (in-list exprs)])
           (context-extend ctx* (id->sym id) (loop expr prop-dict ctx))))
       (loop body prop-dict ctx*)]
      [#`(let* ([,ids #,exprs] ...) #,body)
       (define ctx*
         (for/fold ([ctx* ctx])
                   ([id (in-list ids)]
                    [expr (in-list exprs)])
           (context-extend ctx* (id->sym id) (loop expr prop-dict ctx*))))
       (loop body prop-dict ctx*)]
      [#`(if #,branch #,ifstmt #,elsestmt)
       (define cond-ctx (struct-copy context ctx [repr (get-representation 'bool)]))
       (define cond-repr (loop branch prop-dict cond-ctx))
       (assert-bool stx cond-repr)
       (define ift-repr (loop ifstmt prop-dict ctx))
       (define iff-repr (loop elsestmt prop-dict ctx))
       (unless (types-match? ift-repr iff-repr)
         (error! stx
                 "If statement has different types for if (~a) and else (~a)"
                 (type->string ift-repr)
                 (type->string iff-repr)))
       ift-repr]
      [#`(! #,props ... #,body)
       (define prop-dict* (normalize-prop-dict (apply dict-set prop-dict (map syntax->datum props))))
       (loop body prop-dict* ctx)]
      [#`(,(? (curry hash-has-key? (*functions*)) fname) #,args ...)
       ; TODO: inline functions expect uniform types, this is clearly wrong
       (match-define (list vars prec _) (hash-ref (*functions*) fname))
       (define repr (get-representation prec))
       (define ireprs (map (lambda (arg) (loop arg prop-dict ctx)) args))
       (define expected (map (const repr) vars))
       (unless (andmap equal? ireprs expected)
         (error! stx
                 "Invalid arguments to ~a; expects ~a but got ~a"
                 fname
                 fname
                 (application->string fname expected)
                 (application->string fname ireprs)))
       repr]
      [#`(array #,elems ...)
       (define elem-types (map (lambda (e) (loop e prop-dict ctx)) elems))
       (array-literal-type elem-types prop-dict)]
      [#`(ref #,arr #,idx)
       (define arr-type (loop arr prop-dict ctx))
       (define raw (syntax-e idx))
       (unless (integer? raw)
         (error! idx "Array index must be a literal integer, got ~a" idx))
       (match arr-type
         [(? array-representation?)
          (define dims (array-representation-dims arr-type))
          (define elem (array-representation-elem arr-type))
          (define len (first dims))
          (when (and (integer? raw) (or (< raw 0) (>= raw len)))
            (error! idx "Array index ~a out of bounds for length ~a" raw len))
          (if (= (length dims) 1)
              elem
              (array-of (cdr dims) elem))]
         [_
          (error! stx "ref expects an array, got ~a" (type->string arr-type))
          (current-repr prop-dict)])]
      [#`(cast #,arg)
       (define irepr (loop arg prop-dict ctx))
       (define repr (current-repr prop-dict))
       (cond
         [(equal? irepr repr) repr]
         [else
          (match (get-fpcore-impl 'cast prop-dict (list irepr))
            [#f ; no implementation found
             (error! stx
                     "No implementation of `~a` in platform for context `~a`"
                     (application->string 'cast (list irepr))
                     prop-dict)
             (current-repr prop-dict)]
            [impl (impl-info impl 'otype)])])]
      [#`(,(? symbol? op) #,args ...)
       (define ireprs (map (lambda (arg) (loop arg prop-dict ctx)) args))
       (match (get-fpcore-impl op prop-dict ireprs)
         [#f ; no implementation found
          (error! stx
                  "No implementation of `~a` in platform for context `~a`"
                  (application->string op ireprs)
                  prop-dict)
          (current-repr prop-dict)]
         [impl (impl-info impl 'otype)])])))
(module+ test
  (require rackunit)
  (require "platform.rkt"
           "../syntax/load-platform.rkt")
  (activate-platform! (*platform-name*))

  ;; Dummy representation registration
  (check-false (repr-exists? 'dummy))
  (define pf (platform-copy (*active-platform*)))
  (parameterize ([*active-platform* pf])
    (define dummy-repr
      (make-representation #:name 'dummy
                           #:bf->repr identity
                           #:repr->bf identity
                           #:ordinal->repr identity
                           #:repr->ordinal identity
                           #:total-bits 0
                           #:special-value? (const #f)))
    (hash-set! (platform-representations pf) 'dummy dummy-repr)
    (hash-set! (platform-representation-costs pf) 'dummy 1)
    (check-true (repr-exists? 'dummy))

    (define dummy (get-representation 'dummy))
    (check-equal? (representation-name dummy) 'dummy)
    (check-equal? (get-representation 'dummy) dummy)

    ;; Context operations
    (define <b64> (get-representation 'binary64))
    (define <bool> (get-representation 'bool))

    (define ctx (context '() <b64> '()))
    (define ctx1 (context-extend ctx 'x <b64>))
    (check-equal? (context-vars ctx1) '(x))
    (check-equal? (context-lookup ctx1 'x) <b64>)

    (define ctx2 (context-extend ctx1 'y <bool>))
    (check-equal? (context-vars ctx2) '(y x))
    (check-equal? (context-lookup ctx2 'y) <bool>)
    (check-equal? (context-lookup ctx2 'x) <b64>)

    (define (fail! stx msg . args)
      (error (apply format msg args) stx))

    (define (check-types env-type rtype expr #:env [env '()])
      (define ctx (context (map car env) env-type (map cdr env)))
      (define repr (expression->type expr (repr->prop env-type) ctx fail!))
      (cond
        [(and (representation? repr) (representation? rtype))
         (check-equal? (representation-name repr) (representation-name rtype))]
        [else (check-equal? repr rtype)]))

    (define (check-fails type expr #:env [env '()])
      (define fail? #f)
      (define ctx (context (map car env) type (map cdr env)))
      (expression->type expr (repr->prop type) ctx (lambda _ (set! fail? #t)))
      (check-true fail?))

    (check-types <b64> <b64> #'4)
    (check-types <b64> <b64> #'x #:env `((x . ,<b64>)))
    (check-types <b64> <b64> #'(acos x) #:env `((x . ,<b64>)))
    (check-fails <b64> #'(acos x) #:env `((x . ,<bool>)))
    (check-types <b64> <bool> #'(and a b) #:env `((a . ,<bool>) (b . ,<bool>)))
    (check-types <b64> <b64> #'(if (== a 1) 1 0) #:env `((a . ,<b64>)))
    (check-fails <b64> #'(if (== a 1) 1 0) #:env `((a . ,<bool>)))
    (check-types <b64> <bool> #'(let ([a 1]) TRUE))
    (check-fails <b64> #'(if (== a 1) 1 TRUE) #:env `((a . ,<b64>)))
    (check-types <b64> <b64> #'(let ([a 1]) a) #:env `((a . ,<bool>)))

    ;; Array-aware typing
    (define vec-type (array-of '(2) <b64>))
    (define vec3-type (array-of '(3) <b64>))
    (check-types <b64> vec-type #'(array 1 2))
    (check-types <b64> vec3-type #'(array 1 2 3))
    (define ragged-fail #f)
    (expression->type #'(array (array 1) (array 1 2))
                      (repr->prop <b64>)
                      (context '() <b64> '())
                      (lambda _ (set! ragged-fail #t)))
    (check-true ragged-fail)
    (check-types <b64> <b64> #'(ref (array 5 6) 0))
    (check-types <b64> <b64> #'(ref A 2) #:env `((A . ,vec3-type)))
    (check-fails <b64> #'(ref A 3) #:env `((A . ,vec3-type)))
    (check-fails <b64> #'(ref x 0) #:env `((x . ,<b64>))))

  ;; Array precision should normalize to element precision for typing
  (check-not-exn (lambda ()
                   (assert-program-typed! #'(FPCore () :precision arraybinary64-2 (array 1.0 2.0)))))
  (check-not-exn (lambda () (assert-program-typed! #'(FPCore ((v 3)) :precision binary64 (ref v 2)))))
  (check-not-exn (lambda ()
                   (assert-program-typed! #'(FPCore ((a 3) (b 3))
                                                    :precision
                                                    binary64
                                                    (+ (+ (ref a 0) (ref b 0))
                                                       (+ (ref a 2) (ref b 2))))))))
