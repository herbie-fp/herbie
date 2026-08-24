#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "types.rkt"
         "platform.rkt"
         "syntax.rkt"
         (except-in "platform-language.rkt" quasisyntax))
(provide assert-program-typed!)

(define (repr-description t)
  (match t
    [(? representation?) (representation-name t)]
    [_ t]))

(define (repr-compatible-with-precision? repr precision-repr)
  (match repr
    ;; A tuple-typed program is compatible with any ambient precision: unless a
    ;; slot is explicitly annotated with `!`, it already types at the ambient
    ;; precision, so a differing slot can only come from a deliberate annotation
    ;; (mixed-precision tuples are the point of the type).
    [(? tuple-representation?) #t]
    [(? representation?)
     (or (equal? (representation-type repr) 'bool) (equal? repr precision-repr))]))

;; A dimensioned argument such as (x 2 3) is a homogeneous tuple tree.
(define (tuple-of elem dims)
  (for/fold ([out elem]) ([d (in-list (reverse dims))])
    (unless (exact-positive-integer? d)
      (raise-herbie-error "Argument dimensions must be positive integers, got ~a" d))
    (make-tuple-representation #:slots (make-list d out))))

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
  (define program-repr (get-representation prec))

  (define-values (var-names var-types)
    (for/lists (var-names var-types)
               ([var (in-list vars)])
               (match (syntax->datum var)
                 [(list '! props ... name dims ...)
                  (define prop-dict (props->dict props))
                  (define arg-prec (dict-ref prop-dict ':precision prec))
                  (define arg-repr (get-representation arg-prec))
                  (values name (tuple-of arg-repr dims))]
                 [(list (? symbol? name) dims ...) (values name (tuple-of program-repr dims))]
                 [(? symbol? name) (values name program-repr)])))

  (define ctx (context var-names program-repr var-types))
  (values (assert-expression-type! body prop-dict ctx) ctx))

(define (assert-expression-type! stx props ctx)
  (define errs '())
  (define (error! stx fmt . args)
    (define args* (map repr-description args))
    (set! errs (cons (cons stx (apply format fmt args*)) errs)))

  (define repr (expression->type stx props ctx error!))
  (define expected (context-repr ctx))
  (unless (repr-compatible-with-precision? repr expected)
    (error! stx
            "Expected program of type ~a, got type ~a"
            (repr-description expected)
            (repr-description repr)))

  (unless (null? errs)
    (raise-herbie-syntax-error "Program has type errors" #:locations errs))
  repr)

(define (application->string op types)
  (format "(~a ~a)"
          op
          (string-join (for/list ([t types])
                         (if t
                             (format "<~a>" (repr-description t))
                             "<?>"))
                       " ")))

(define (expression->type stx prop-dict ctx error!)
  (let loop ([stx stx]
             [prop-dict prop-dict]
             [ctx ctx])
    (match stx
      [#`,(? number?) (get-representation (dict-ref prop-dict ':precision))]
      [#`,(? operator-exists? op)
       (match (get-fpcore-impl op prop-dict '())
         [#f ; no implementation found
          (error! stx "No implementation of `~a` in platform for context `~a`" op prop-dict)
          (get-representation (dict-ref prop-dict ':precision))]
         [impl (impl-info impl 'otype)])]
      [#`,(? symbol? x) (context-lookup ctx x)]
      [#`(let ([,ids #,exprs] ...) #,body)
       (define ctx*
         (for/fold ([ctx* ctx])
                   ([id (in-list ids)]
                    [expr (in-list exprs)])
           (context-extend ctx* id (loop expr prop-dict ctx))))
       (loop body prop-dict ctx*)]
      [#`(let* ([,ids #,exprs] ...) #,body)
       (define ctx*
         (for/fold ([ctx* ctx])
                   ([id (in-list ids)]
                    [expr (in-list exprs)])
           (context-extend ctx* id (loop expr prop-dict ctx*))))
       (loop body prop-dict ctx*)]
      [#`(if #,branch #,ifstmt #,elsestmt)
       (define cond-ctx (struct-copy context ctx [repr (get-representation 'bool)]))
       (define cond-repr (loop branch prop-dict cond-ctx))
       (unless (equal? (representation-type cond-repr) 'bool)
         (error! stx "If statement has non-boolean type ~a for branch" (repr-description cond-repr)))
       (define ift-repr (loop ifstmt prop-dict ctx))
       (define iff-repr (loop elsestmt prop-dict ctx))
       (unless (equal? ift-repr iff-repr)
         (error! stx
                 "If statement has different types for if (~a) and else (~a)"
                 (repr-description ift-repr)
                 (repr-description iff-repr)))
       ift-repr]
      [#`(! #,props ... #,body) (loop body (apply dict-set prop-dict (map syntax->datum props)) ctx)]
      ;; An `array` literal is a homogeneous tuple
      [(or #`(array #,elems ...) #`(tuple #,elems ...))
       ;; Empty tuples are rejected in syntax-check.rkt.
       (define slots
         (for/list ([elem (in-list elems)])
           (loop elem prop-dict ctx)))
       (cond
         [(for/or ([slot (in-list slots)])
            (equal? (representation-type slot) 'bool))
          ;; Tuple slots are typed `real` in the spec language (see
          ;; `spec-arg-types` in egg-herbie.rkt), so a boolean slot would be
          ;; mistyped during rewriting.
          (error! stx "Tuple slots must not be boolean")
          (get-representation (dict-ref prop-dict ':precision))]
         [else (make-tuple-representation #:slots slots)])]
      [#`(ref #,arr #,idx)
       (define arr-type (loop arr prop-dict ctx))
       (define raw (syntax-e idx))
       (match arr-type
         [(? tuple-representation?)
          (define slots (tuple-representation-slots arr-type))
          (cond
            [(and (exact-nonnegative-integer? raw) (< raw (length slots))) (list-ref slots raw)]
            [(exact-nonnegative-integer? raw)
             (error! idx "Tuple index ~a out of bounds for ~a slots" raw (length slots))
             (get-representation (dict-ref prop-dict ':precision))]
            [else
             (error! idx "Index must be a nonnegative integer literal, got ~a" idx)
             (get-representation (dict-ref prop-dict ':precision))])]
         [_
          (error! stx "ref expects a tuple, got ~a" (repr-description arr-type))
          (get-representation (dict-ref prop-dict ':precision))])]
      [#`(cast #,arg)
       (define irepr (loop arg prop-dict ctx))
       (define repr (get-representation (dict-ref prop-dict ':precision)))
       (cond
         [(equal? irepr repr) repr]
         [else
          (match (get-fpcore-impl 'cast prop-dict (list irepr))
            [#f ; no implementation found
             (error! stx
                     "No implementation of `~a` in platform for context `~a`"
                     (application->string 'cast (list irepr))
                     prop-dict)
             (get-representation (dict-ref prop-dict ':precision))]
            [impl (impl-info impl 'otype)])])]
      [#`(,(? symbol? op) #,args ...)
       (define ireprs (map (lambda (arg) (loop arg prop-dict ctx)) args))
       (match (get-fpcore-impl op prop-dict ireprs)
         [#f ; no implementation found
          (error! stx
                  "No implementation of `~a` in platform for context `~a`"
                  (application->string op ireprs)
                  prop-dict)
          (get-representation (dict-ref prop-dict ':precision))]
         [impl (impl-info impl 'otype)])])))
(module+ test
  (require rackunit)
  (require "platform.rkt"
           "../syntax/load-platform.rkt")
  (activate-platform! (*platform-name*))

  ;; Dummy representation registration
  (check-false (hash-has-key? (platform-representations (*active-platform*)) 'dummy))
  (define pf
    (struct-copy $platform
                 (*active-platform*)
                 [representations (hash-copy (platform-representations (*active-platform*)))]
                 [implementations (hash-copy (platform-implementations (*active-platform*)))]
                 [representation-costs
                  (hash-copy (platform-representation-costs (*active-platform*)))]))
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
    (check-true (hash-has-key? (platform-representations pf) 'dummy))

    (define dummy (get-representation 'dummy))
    (check-equal? (representation-name dummy) 'dummy)
    (check-equal? (get-representation 'dummy) dummy)

    (define tuple2 (make-tuple-representation #:slots (list dummy dummy)))
    (check-equal? (representation-name tuple2) '(tuple dummy dummy))
    (check-true (repr-exists? '(tuple dummy dummy)))
    (check-equal? (representation-name (get-representation '(tuple dummy dummy)))
                  '(tuple dummy dummy))

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

    ;; Array literals are homogeneous tuples
    (define vec-type (tuple-of <b64> '(2)))
    (define vec3-type (tuple-of <b64> '(3)))
    (check-types <b64> vec-type #'(array 1 2))
    (check-types <b64> vec3-type #'(array 1 2 3))
    (check-types <b64>
                 (make-tuple-representation #:slots (list (tuple-of <b64> '(1)) vec-type))
                 #'(array (array 1) (array 1 2)))
    (check-types <b64> <b64> #'(ref (array 5 6) 0))
    (check-types <b64> <b64> #'(ref A 2) #:env `((A . ,vec3-type)))
    (check-fails <b64> #'(ref A 3) #:env `((A . ,vec3-type)))
    (check-fails <b64> #'(ref x 0) #:env `((x . ,<b64>))))

  (check-exn exn:fail?
             (lambda ()
               (assert-program-typed! #'(FPCore () :precision (array binary64 2) (array 1.0 2.0)))))
  (check-not-exn (lambda ()
                   (assert-program-typed! #'(FPCore () :precision binary64 (array 1.0 2.0)))))
  (check-not-exn (lambda () (assert-program-typed! #'(FPCore ((v 3)) :precision binary64 (ref v 2)))))
  (check-not-exn (lambda ()
                   (assert-program-typed! #'(FPCore ((a 3) (b 3))
                                                    :precision
                                                    binary64
                                                    (+ (+ (ref a 0) (ref b 0))
                                                       (+ (ref a 2) (ref b 2))))))))
