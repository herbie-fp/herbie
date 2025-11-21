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
    [(tensor-type? t)
     (format "tensor[~a] of ~a"
             (string-join (for/list ([d (tensor-type-dims t)])
                            (~a d))
                          " ")
             (type->string (tensor-type-elem t)))]
    [else (~a t)]))

(define (flatten-type t)
  (if (tensor-type? t)
      (values (tensor-type-elem t) (tensor-type-dims t))
      (values t '())))

(define (tensor-of dims elem)
  (tensor-type dims elem))

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

  (define-values (var-names var-types dim-names)
    (for/lists (var-names var-types dim-names)
               ([var (in-list vars)])
               (match (syntax->datum var)
                 [(list '! props ... name dims ...)
                  (define prop-dict (props->dict props))
                  (define arg-prec (dict-ref prop-dict ':precision prec))
                  (define repr (get-representation arg-prec))
                  (values name
                          (if (null? dims)
                              repr
                              (tensor-of dims repr))
                          (filter symbol? dims))]
                 [(list (? symbol? name) dims ...)
                  (define repr (get-representation prec))
                  (values name
                          (if (null? dims)
                              repr
                              (tensor-of dims repr))
                          (filter symbol? dims))]
                 [(? symbol? name) (values name (get-representation prec) '())])))

  (define unique-dim-names (remove-duplicates (apply append dim-names)))
  (define ctx
    (context (append var-names unique-dim-names)
             (get-representation prec)
             (append var-types (make-list (length unique-dim-names) (get-representation prec)))))
  (assert-expression-type! body prop-dict ctx))

(define (assert-expression-type! stx props ctx)
  (define errs '())
  (define (error! stx fmt . args)
    (define args*
      (for/list ([arg (in-list args)])
        (match arg
          [(? representation?) (representation-name arg)]
          [(? tensor-type?) (type->string arg)]
          [_ arg])))
    (set! errs (cons (cons stx (apply format fmt args*)) errs)))

  (define repr (expression->type stx props ctx error!))
  (define expected (context-repr ctx))
  (define (comparable? a b)
    (or (and (representation? a) (representation? b)) (and (tensor-type? a) (tensor-type? b))))
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
  (define (current-repr pd)
    (get-representation (dict-ref pd ':precision)))
  (define (bool-type? t)
    (and (representation? t) (equal? (representation-type t) 'bool)))
  (define (assert-bool who ty)
    (unless (bool-type? ty)
      (error! who "Expected boolean type, got ~a" (type->string ty))))
  (define (assert-scalar who ty purpose)
    (unless (representation? ty)
      (error! who purpose (type->string ty))))
  (define (array-type elem-types current-dict)
    (cond
      [(null? elem-types)
       (error! stx "Array literal must have at least one element")
       (tensor-of '(0) (current-repr current-dict))]
      [else
       (define-values (base base-dims) (flatten-type (first elem-types)))
       (for ([t (in-list (rest elem-types))])
         (define-values (elem elem-dims) (flatten-type t))
         (unless (and (equal? elem base) (equal? elem-dims base-dims))
           (error! stx
                   "Array elements have mismatched types: ~a vs ~a"
                   (type->string base)
                   (type->string t))))
       (tensor-of (cons (length elem-types) base-dims) base)]))
  (define (types-match? a b)
    (equal? a b))
  (define (id->sym x)
    (if (syntax? x)
        (syntax-e x)
        x))

  (let loop ([stx stx]
             [prop-dict prop-dict]
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
      [#`(! #,props ... #,body) (loop body (apply dict-set prop-dict (map syntax->datum props)) ctx)]
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
       (array-type elem-types prop-dict)]
      [#`(tensor ([#,idxs #,sizes] ...) #,body)
       (for ([size (in-list sizes)])
         (assert-scalar size
                        (loop size prop-dict ctx)
                        "Tensor dimension size must be scalar, got ~a"))
       (define idx-ctx
         (for/fold ([ctx* ctx]) ([idx (in-list idxs)])
           (context-extend ctx* (id->sym idx) (current-repr prop-dict))))
       (define body-type (loop body prop-dict idx-ctx))
       (define-values (elem elem-dims) (flatten-type body-type))
       (tensor-of (append (map syntax->datum sizes) elem-dims) elem)]
      [#`(tensor* ([#,idxs #,sizes] ...) ([#,vars* #,inits #,updates] ...) #,body)
       (for ([size (in-list sizes)])
         (assert-scalar size
                        (loop size prop-dict ctx)
                        "Tensor dimension size must be scalar, got ~a"))
       (define idx-ctx
         (for/fold ([ctx* ctx]) ([idx (in-list idxs)])
           (context-extend ctx* (id->sym idx) (current-repr prop-dict))))
       (define init-types (map (lambda (e) (loop e prop-dict idx-ctx)) inits))
       (define acc-ctx
         (for/fold ([ctx* idx-ctx])
                   ([var (in-list vars*)]
                    [ty (in-list init-types)])
           (context-extend ctx* (id->sym var) ty)))
       (for ([var (in-list vars*)]
             [init-ty (in-list init-types)]
             [update (in-list updates)])
         (define update-ty (loop update prop-dict acc-ctx))
         (unless (types-match? init-ty update-ty)
           (error! update
                   "Mismatched tensor* accumulator ~a types: init ~a, update ~a"
                   var
                   (type->string init-ty)
                   (type->string update-ty))))
       (define body-type (loop body prop-dict acc-ctx))
       (define-values (elem elem-dims) (flatten-type body-type))
       (tensor-of (append (map syntax->datum sizes) elem-dims) elem)]
      [#`(ref #,arr #,idxs ...)
       (define arr-type (loop arr prop-dict ctx))
       (define idx-types (map (lambda (i) (loop i prop-dict ctx)) idxs))
       (for ([itype (in-list idx-types)])
         (assert-scalar arr itype "Reference index must be scalar, got ~a"))
       (match arr-type
         [(? tensor-type?)
          (define dims (tensor-type-dims arr-type))
          (unless (= (length idxs) (length dims))
            (error! stx "Reference expected ~a indices, got ~a" (length dims) (length idxs)))
          (define remaining (drop dims (length idxs)))
          (define elem (tensor-type-elem arr-type))
          (if (null? remaining)
              elem
              (tensor-of remaining elem))]
         [_
          (error! stx "ref expects a tensor, got ~a" (type->string arr-type))
          (current-repr prop-dict)])]
      [#`(dim #,arr)
       (define arr-type (loop arr prop-dict ctx))
       (unless (tensor-type? arr-type)
         (error! stx "dim expects a tensor, got ~a" (type->string arr-type)))
       (current-repr prop-dict)]
      [#`(size #,arr #,dims ...)
       (define arr-type (loop arr prop-dict ctx))
       (unless (tensor-type? arr-type)
         (error! stx "size expects a tensor, got ~a" (type->string arr-type)))
       (for ([dim (in-list dims)])
         (assert-scalar dim (loop dim prop-dict ctx) "size index must be scalar, got ~a"))
       (current-repr prop-dict)]
      [#`(for ([#,idxs #,sizes] ...)
           ([#,vars* #,inits #,updates] ...)
           #,body)
       (for ([size (in-list sizes)])
         (assert-scalar size (loop size prop-dict ctx) "for dimension size must be scalar, got ~a"))
       (define idx-ctx
         (for/fold ([ctx* ctx]) ([idx (in-list idxs)])
           (context-extend ctx* (id->sym idx) (current-repr prop-dict))))
       (define init-types (map (lambda (e) (loop e prop-dict idx-ctx)) inits))
       (define loop-ctx
         (for/fold ([ctx* idx-ctx])
                   ([var (in-list vars*)]
                    [ty (in-list init-types)])
           (context-extend ctx* (id->sym var) ty)))
       (for ([var (in-list vars*)]
             [init-ty (in-list init-types)]
             [update (in-list updates)])
         (define update-ty (loop update prop-dict loop-ctx))
         (unless (types-match? init-ty update-ty)
           (error! update
                   "for accumulator ~a updates from ~a to ~a"
                   var
                   (type->string init-ty)
                   (type->string update-ty))))
       (loop body prop-dict loop-ctx)]
      [#`(for* ([#,idxs #,sizes] ...)
           ([#,vars* #,inits #,updates] ...)
           #,body)
       (for ([size (in-list sizes)])
         (assert-scalar size (loop size prop-dict ctx) "for* dimension size must be scalar, got ~a"))
       (define idx-ctx
         (for/fold ([ctx* ctx]) ([idx (in-list idxs)])
           (context-extend ctx* (id->sym idx) (current-repr prop-dict))))
       (define init-types (map (lambda (e) (loop e prop-dict idx-ctx)) inits))
       (define loop-ctx
         (for/fold ([ctx* idx-ctx])
                   ([var (in-list vars*)]
                    [ty (in-list init-types)])
           (context-extend ctx* (id->sym var) ty)))
       (for ([var (in-list vars*)]
             [init-ty (in-list init-types)]
             [update (in-list updates)])
         (define update-ty (loop update prop-dict loop-ctx))
         (unless (types-match? init-ty update-ty)
           (error! update
                   "for* accumulator ~a updates from ~a to ~a"
                   var
                   (type->string init-ty)
                   (type->string update-ty))))
       (loop body prop-dict loop-ctx)]
      [#`(while #,cond ([#,vars* #,inits #,updates] ...) #,body)
       (define init-types (map (lambda (e) (loop e prop-dict ctx)) inits))
       (define loop-ctx
         (for/fold ([ctx* ctx])
                   ([var (in-list vars*)]
                    [ty (in-list init-types)])
           (context-extend ctx* (id->sym var) ty)))
       (define cond-ty (loop cond prop-dict loop-ctx))
       (assert-bool cond cond-ty)
       (for ([var (in-list vars*)]
             [init-ty (in-list init-types)]
             [update (in-list updates)])
         (define update-ty (loop update prop-dict loop-ctx))
         (unless (types-match? init-ty update-ty)
           (error! update
                   "while accumulator ~a updates from ~a to ~a"
                   var
                   (type->string init-ty)
                   (type->string update-ty))))
       (loop body prop-dict loop-ctx)]
      [#`(while* #,cond ([#,vars* #,inits #,updates] ...) #,body)
       (define init-types (map (lambda (e) (loop e prop-dict ctx)) inits))
       (define loop-ctx
         (for/fold ([ctx* ctx])
                   ([var (in-list vars*)]
                    [ty (in-list init-types)])
           (context-extend ctx* (id->sym var) ty)))
       (define cond-ty (loop cond prop-dict loop-ctx))
       (assert-bool cond cond-ty)
       (for ([var (in-list vars*)]
             [init-ty (in-list init-types)]
             [update (in-list updates)])
         (define update-ty (loop update prop-dict loop-ctx))
         (unless (types-match? init-ty update-ty)
           (error! update
                   "while* accumulator ~a updates from ~a to ~a"
                   var
                   (type->string init-ty)
                   (type->string update-ty))))
       (loop body prop-dict loop-ctx)]
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
      (check-equal? repr rtype))

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

    ;; Tensor-aware typing
    (define vec-type (tensor-type '(3) <b64>))
    (define mat-type (tensor-type '(2 2) <b64>))
    (check-types <b64> vec-type #'(array 1 2 3))
    (define ragged-fail #f)
    (expression->type #'(array (array 1) (array 1 2))
                      (repr->prop <b64>)
                      (context '() <b64> '())
                      (lambda _ (set! ragged-fail #t)))
    (check-true ragged-fail)
    (check-types <b64> (tensor-type '(2) <b64>) #'(tensor ([i 2]) i))
    (check-types <b64>
                 <b64>
                 #'(for ([i 2])
                     ([acc 0 (+ acc i)])
                     acc))
    (check-types <b64> <b64> #'(ref A 0 1) #:env `((A . ,mat-type)))
    (check-fails <b64> #'(ref x 0) #:env `((x . ,<b64>)))))
