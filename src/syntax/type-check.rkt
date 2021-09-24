#lang racket

(require "../common.rkt" "../errors.rkt" "../interface.rkt" "syntax.rkt" "types.rkt")
(provide assert-program-typed!)

(define (assert-program-typed! stx)
  (define-values (vars props body)
    (match (syntax-e stx)
     [(list (app syntax-e 'FPCore) (app syntax-e name) (app syntax-e (list vars ...)) props ... body)
      (values vars props body)]
     [(list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) props ... body)
      (values vars props body)]))
  (define props*
    (let loop ([props props])
      (match props
       [(list) (list)]
       [(list (app syntax-e prop) value rest ...)
        (cons (cons prop (syntax->datum value)) (loop rest))])))
  (define type (get-representation (dict-ref props* ':precision 'binary64)))
  (assert-expression-type! body type #:env (for/hash ([var vars]) (values (syntax-e var) type))))

(define (assert-expression-type! stx expected-rtype #:env [env #hash()])
  (define errs
    (reap [sow]
          (define (error! stx fmt . args)
            (sow (cons stx (apply format fmt
                                  (for/list ([arg args])
                                    (if (representation? arg)
                                        (representation-name arg)
                                        arg))))))
          (define actual-rtype (expression->type stx env expected-rtype error!))
          (unless (equal? expected-rtype actual-rtype)
            (error! stx "Expected program of type ~a, got type ~a" expected-rtype actual-rtype))))
  (unless (null? errs)
    (raise-herbie-syntax-error "Program has type errors" #:locations errs)))

(define (repr-has-type? repr type)
  (and repr (equal? (representation-type repr) type)))

(define (expression->type stx env type error!)
  (match stx
    [#`,(? number?) type]
    [#`,(? constant-operator? x)
     (let/ec k
       (for/list ([name (operator-all-impls x)])
         (define rtype (operator-info name 'otype))
         (when (or (equal? rtype type) (repr-has-type? rtype 'bool))
           (k rtype)))
       (error! stx "Could not find implementation of ~a for ~a" x type))]
    [#`,(? variable? x)
     (define vtype (dict-ref env x))
     (cond
      [(repr-has-type? vtype 'bool) vtype]
      [(equal? type vtype) type]
      [else (error! stx "Expected a variable of type ~a, but got ~a" type vtype)])]
    [#`(let ((,id #,expr) ...) #,body)
     (define env2
       (for/fold ([env2 env]) ([var id] [val expr])
         (dict-set env2 var (expression->type val env type error!))))
     (expression->type body env2 type error!)]
    [#`(let* ((,id #,expr) ...) #,body)
     (define env2
       (for/fold ([env2 env]) ([var id] [val expr])
         (dict-set env2 var (expression->type val env2 type error!))))
     (expression->type body env2 type error!)]
    [#`(if #,branch #,ifstmt #,elsestmt)
     (define branch-type (expression->type branch env type error!))
     (unless (repr-has-type? branch-type 'bool)
       (error! stx "If statement has non-boolean type ~a for branch"
               branch-type))
     (define ifstmt-type (expression->type ifstmt env type error!))
     (define elsestmt-type (expression->type elsestmt env type error!))
     (unless (equal? ifstmt-type elsestmt-type)
       (error! stx "If statement has different types for if (~a) and else (~a)"
               ifstmt-type elsestmt-type))
      ifstmt-type]
    [#`(! #,props ... #,body)
     (define props* (apply hash-set* (hash) (map syntax-e props)))
     (cond
       [(hash-has-key? props* ':precision)
        (define itype (get-representation (hash-ref props* ':precision)))
        (define rtype (expression->type body env itype error!))
        (unless (equal? rtype itype)
          (error! stx "Annotation promised precision ~a, but got ~a"
                  itype rtype))
        type]
       [else
        (expression->type body env type error!)])]
    [#`(- #,arg)
     (define actual-type (expression->type arg env type error!))
     (define op* (get-parametric-operator 'neg actual-type #:fail-fast? #f))
     (if op*
         (operator-info op* 'otype)
         (begin
          (error! stx "Invalid arguments to -; expects ~a but got (- <~a>)"
                  (string-join
                   (for/list ([sig (operator-all-impls 'neg)])
                     (define atypes (operator-info sig 'itype))
                     (format "(- ~a)" (string-join
                                       (for/list ([atype atypes])
                                         (format "<~a>" atype))
                                       " ")))
                   " or ")
                  actual-type)
          #f))]
    [#`(,(and (or '+ '- '* '/) op) #,exprs ...)
     (define t #f)
     (for ([arg exprs] [i (in-naturals)])
       (define actual-type (expression->type arg env type error!))
       (when (repr-has-type? actual-type 'bool)
          (error! stx "~a does not take boolean arguments" op))
       (if (= i 0) (set! t actual-type) #f)
       (unless (equal? t actual-type)
         (error! stx "~a expects argument ~a of type ~a (not ~a)" op (+ i 1)
                 t actual-type)))
     t]
    [#`(,(and (or '< '> '<= '>= '= '!=) op) #,exprs ...)
     (define t #f)
     (for ([arg exprs] [i (in-naturals)])
       (define actual-type (expression->type arg env type error!))
       (when (repr-has-type? actual-type 'bool)
          (error! stx "~a does not take boolean arguments" op))
       (if (= i 0) (set! t actual-type) #f)
       (unless (equal? t actual-type)
         (error! stx "~a expects argument ~a of type ~a (not ~a)" op (+ i 1)
                 t actual-type)))
     (get-representation 'bool)]
    [#`(,(and (or 'and 'or) op) #,exprs ...)
     (for ([arg exprs] [i (in-naturals)])
       (define actual-type (expression->type arg env type error!))
       (unless (repr-has-type? actual-type 'bool)
          (error! stx "~a only takes boolean arguments" op)))
     (get-representation 'bool)]
    [#`(,(and (or 're 'im) op) #,arg)
     ; TODO: this special case can be removed when complex-herbie is moved to a composite type
     ; re, im : complex -> binary64
     (define atype (expression->type arg env (get-representation 'complex) error!)) 
     (unless (repr-has-type? atype 'complex)
       (error! stx "~a expects argument of type complex (not ~a)" op atype))
     (get-representation 'binary64)]
    [#`(complex #,re #,im)
     ; TODO: this special case can be removed when complex-herbie is moved to a composite type
     ; complex : binary64, binary64 -> complex
     (define b64 (get-representation 'binary64))
     (define re-type (expression->type re env b64 error!))
     (define im-type (expression->type im env b64 error!))
     (unless (and (equal? re-type b64) (equal? im-type b64))
       (error! stx "complex expects arguments of type binary64, binary64 (not ~a, ~a)"
               re-type im-type))
     (get-representation 'complex)]
    [#`(,(? operator-exists? op) #,exprs ...)
     (define actual-types (for/list ([arg exprs]) (expression->type arg env type error!)))
     (define op* (apply get-parametric-operator op actual-types #:fail-fast? #f))
     (if op*
         (operator-info op* 'otype)
         (begin
          (error! stx "Invalid arguments to ~a; expects ~a but got (~a ~a)" op
                  (string-join
                    (for/list ([sig (operator-all-impls op)])
                      (define atypes (operator-info sig 'itype))
                      (format "(~a ~a)" op (string-join
                                            (for/list ([atype atypes])
                                              (format "<~a>" atype))
                                            " ")))
                    " or ")
                  op (string-join (map (curry format "<~a>") (map representation-name actual-types)) " "))
          #f))]
    [#`(,(? (curry hash-has-key? (*functions*)) fname) #,exprs ...)
     (match-define (list vars repr _) (hash-ref (*functions*) fname))
     (define actual-types (for/list ([arg exprs]) (expression->type arg env type error!)))
     (define expected (map (const repr) vars))
     (if (andmap equal? actual-types expected)
         repr
         (begin
           (error! stx "Invalid arguments to ~a; expects (~a ~a) but got (~a ~a)" fname
                       fname fname (string-join (map (curry format "<~a>") (map representation-name expected)) " ")
                       fname (string-join (map (curry format "<~a>") (map representation-name actual-types)) " "))
           #f))]))

(module+ test
  (require rackunit)
  (require "../load-plugin.rkt")
  (load-herbie-builtins)

  (define (get-reprs-ctx env)
    (for/hash ([(k v) (in-dict env)])
      (values k (get-representation v))))

  (define (fail stx msg . args)
    (error (apply format msg args) stx))

  (define (check-types env-type rtype expr #:env [env #hash()])
    (check-equal? (expression->type expr (get-reprs-ctx env) (get-representation env-type) fail)
                  (get-representation rtype)))

  (define (check-fails type expr #:env [env #hash()])
    (check-equal?
     (let ([v #f])
       (expression->type expr (get-reprs-ctx env) (get-representation type) (lambda _ (set! v #t)))
       v)
     #t))

  (check-types 'binary64 'binary64 #'4)
  (check-types 'binary64 'binary64 #'x #:env #hash((x . binary64)))
  (check-types 'binary64 'binary64 #'(acos x) #:env #hash((x . binary64)))
  (check-fails 'binary64 #'(acos x) #:env #hash((x . bool)))
  (check-types 'binary64 'bool #'(and a b c) #:env #hash((a . bool) (b . bool) (c . bool)))
  (check-types 'binary64 'binary64 #'(if (== a 1) 1 0) #:env #hash((a . binary64)))
  (check-fails 'binary64 #'(if (== a 1) 1 0) #:env #hash((a . bool)))
  (check-types 'binary64 'bool #'(let ([a 1]) TRUE))
  (check-fails 'binary64 #'(if (== a 1) 1 TRUE) #:env #hash((a . binary64)))
  (check-types 'binary64 'binary64 #'(let ([a 1]) a) #:env #hash((a . bool))))
