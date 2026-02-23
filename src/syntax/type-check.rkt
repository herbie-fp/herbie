#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "types.rkt"
         "platform.rkt"
         "syntax.rkt"
         (except-in "platform-language.rkt" quasisyntax))
(provide assert-program-typed!)

(define (type->string t)
  (cond
    [(representation? t) (~a (representation-name t))]
    [(array-representation? t)
     (format "array[~a] of ~a"
             (string-join (for/list ([d (array-representation-shape t)])
                            (~a d))
                          " ")
             (type->string (array-representation-base t)))]
    [else (~a t)]))

(define (array-of elem dims)
  (for/fold ([out elem]) ([d (in-list (reverse dims))])
    (make-array-representation #:elem out #:len d)))

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
                  (values name (array-of arg-repr dims))]
                 [(list (? symbol? name) dims ...) (values name (array-of program-repr dims))]
                 [(? symbol? name) (values name program-repr)])))

  (define ctx (context var-names program-repr var-types))
  (values (assert-expression-type! body prop-dict ctx) ctx))

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
  (define (unfold-real-type ty)
    (define base
      (if (array-representation? ty)
          (array-representation-base ty)
          ty))
    (and (representation? base) (equal? (representation-type base) 'real) base))
  (define actual-real (and expected (unfold-real-type repr)))
  (define expected-real (and expected (unfold-real-type expected)))
  (when (or (not actual-real)
            (not expected-real)
            (not (equal? (representation-name actual-real) (representation-name expected-real))))
    (error! stx
            "Expected program of type ~a, got type ~a"
            (type->string expected)
            (type->string repr)))

  (unless (null? errs)
    (raise-herbie-syntax-error "Program has type errors" #:locations errs))
  repr)

(define (application->string op types)
  (format "(~a ~a)"
          op
          (string-join (for/list ([t types])
                         (if t
                             (format "<~a>" (type->string t))
                             "<?>"))
                       " ")))

(define (expression->type stx prop-dict ctx error!)
  (define (array-literal-type elem-types current-dict)
    (cond
      [(null? elem-types)
       (error! stx "Array literal must have at least one element")
       (array-of (get-representation (dict-ref current-dict ':precision)) '(1))]
      [else
       (define first-type (first elem-types))
       (define-values (base base-dims)
         (if (array-representation? first-type)
             (values (array-representation-base first-type) (array-representation-shape first-type))
             (values first-type '())))
       (for ([t (in-list (rest elem-types))])
         (define-values (elem elem-dims)
           (if (array-representation? t)
               (values (array-representation-base t) (array-representation-shape t))
               (values t '())))
         (unless (and (equal? elem base) (equal? elem-dims base-dims))
           (error! stx
                   "Array elements have mismatched types: ~a vs ~a"
                   (type->string base)
                   (type->string t))))
       (array-of base (cons (length elem-types) base-dims))]))

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
         (error! stx "If statement has non-boolean type ~a for branch" (type->string cond-repr)))
       (define ift-repr (loop ifstmt prop-dict ctx))
       (define iff-repr (loop elsestmt prop-dict ctx))
       (unless (equal? ift-repr iff-repr)
         (error! stx
                 "If statement has different types for if (~a) and else (~a)"
                 (type->string ift-repr)
                 (type->string iff-repr)))
       ift-repr]
      [#`(! #,props ... #,body)
       (define prop-dict* (apply dict-set prop-dict (map syntax->datum props)))
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
          (define len (array-representation-len arr-type))
          (define elem (array-representation-elem arr-type))
          (when (and (integer? raw) (or (< raw 0) (>= raw len)))
            (error! idx "Array index ~a out of bounds for length ~a" raw len))
          elem]
         [_
          (error! stx "ref expects an array, got ~a" (type->string arr-type))
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
    (define vec-type (array-of <b64> '(2)))
    (define vec3-type (array-of <b64> '(3)))
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

  ;; Defensive array-precision rejection lives in syntax-check; type-check accepts known repr names.
  (check-not-exn (lambda ()
                   (assert-program-typed! #'(FPCore () :precision arraybinary64-2 (array 1.0 2.0)))))
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
