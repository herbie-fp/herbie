#lang racket

(require "../common.rkt" "../errors.rkt" "../interface.rkt"
         "syntax.rkt" "sugar.rkt" "types.rkt")
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
  (define type (dict-ref props* ':precision 'binary64))
  (get-representation type)   ; load if needed
  (assert-expression-type! body type #:env (for/hash ([var vars]) (values (syntax-e var) type))))

(define (assert-expression-type! stx expected-rtype #:env [env #hash()])
  (define errs
    (reap [sow]
          (define (error! stx fmt . args)
            (sow (cons stx (apply format fmt args))))
          (define actual-rtype (expression->type stx env expected-rtype error!))
          (unless (equal? expected-rtype actual-rtype)
            (error! stx "Expected program of type ~a, got type ~a" expected-rtype actual-rtype))))
  (unless (null? errs)
    (raise-herbie-syntax-error "Program has type errors" #:locations errs)))

(define (expression->type stx env type error!)
  (match stx
    [#`,(? number?) type]
    [#`,(? constant? x)
     (if (set-member? '(TRUE FALSE) x)
         (constant-info x 'type)
         (constant-info (get-parametric-constant x type) 'type))]
    [#`,(? variable? x)
     (define etype (type-name (representation-type (get-representation type))))
     (define vtype (type-name (representation-type (get-representation (dict-ref env x)))))
     (cond
      [(equal? vtype 'bool) 'bool]
      [(equal? etype vtype) type]
      [else (error! stx "Expected a variable of type ~a, but got ~a" etype vtype)])]
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
     (unless (equal? branch-type 'bool)
       (error! stx "If statement has non-boolean type for branch ~a" branch-type))
     (define ifstmt-type (expression->type ifstmt env type error!))
     (define elsestmt-type (expression->type elsestmt env type error!))
     (unless (equal? ifstmt-type elsestmt-type)
       (error! stx "If statement has different types for if (~a) and else (~a)" ifstmt-type elsestmt-type))
      ifstmt-type]
    [#`(! #,props ... #,body)
     (define props* (apply hash-set* (hash) (map syntax-e props)))
     (cond
       [(hash-has-key? props* ':precision)
        (define itype (hash-ref props* ':precision))
        (define rtype (expression->type body env itype error!))
        (unless (equal? rtype itype)
          (error! stx "Annotation promised precision ~a, but got ~a" itype rtype))
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
                   (for/list ([sig (hash-ref parametric-operators 'neg)])
                     (match-define (list* _ _ atypes) sig)
                     (if (list? atypes)
                         (format "(- ~a)" (string-join (map (curry format "<~a>") atypes) " "))
                         (format "(- <~a> ...)" atypes)))
                   " or ")
                  actual-type)
          #f))]    
    [#`(,(and (or '+ '- '* '/) op) #,exprs ...)
     (define t #f)
     (for ([arg exprs] [i (in-naturals)])
       (define actual-type (expression->type arg env type error!))
       (if (= i 0) (set! t actual-type) #f)
       (unless (equal? t actual-type)
         (error! stx "~a expects argument ~a of type ~a (not ~a)" op (+ i 1) t actual-type)))
     t]
    [#`(,(and (or 're 'im) op) #,arg)
     ; TODO: this special case can be removed when complex-herbie is moved to a composite type
     ; re, im : complex -> binary64
     (define atype (expression->type arg env 'complex error!)) 
     (unless (equal? atype 'complex)
       (error! stx "~a expects argument of type complex (not ~a)" op atype))
     'binary64]
    [#`(complex #,re #,im)
     ; TODO: this special case can be removed when complex-herbie is moved to a composite type
     ; complex : binary64, binary64 -> complex
     (define re-type (expression->type re env 'binary64 error!))
     (define im-type (expression->type im env 'binary64 error!))
     (unless (and (equal? re-type 'binary64) (equal? im-type 'binary64))
       (error! stx "complex expects arguments of type binary64, binary64 (not ~a, ~a)" re-type im-type))
     'complex]
    [#`(,(? operator-exists? op) #,exprs ...)
     (define actual-types (for/list ([arg exprs]) (expression->type arg env type error!)))
     (define op* (apply get-parametric-operator op actual-types #:fail-fast? #f))
     (if op*
         (operator-info op* 'otype)
         (begin
          (error! stx "Invalid arguments to ~a; expects ~a but got (~a ~a)" op
                  (string-join
                    (for/list ([sig (hash-ref parametric-operators op)])
                     (match-define (list* _ _ atypes) sig)
                     (if (list? atypes)
                         (format "(~a ~a)" op (string-join (map (curry format "<~a>") atypes) " "))
                         (format "(~a <~a> ...)" op atypes)))
                    " or ")
                  op (string-join (map (curry format "<~a>") actual-types) " "))
          #f))]
    [#`(,(? (curry hash-has-key? (*functions*)) fname) #,exprs ...)
     (match-define (list vars repr _) (hash-ref (*functions*) fname))
     (define prec (representation-name repr))
     (define actual-types (for/list ([arg exprs]) (expression->type arg env type error!)))
     (define expected (map (const prec) vars))
     (if (andmap equal? actual-types expected)
         prec
         (begin
           (error! stx "Invalid arguments to ~a; expects (~a ~a) but got (~a ~a)" fname
                       fname fname (string-join (map (curry format "<~a>") expected) " ")
                       fname (string-join (map (curry format "<~a>") actual-types) " "))
           #f))]))

(module+ test
  (require rackunit)

  (define (fail stx msg . args)
    (error (apply format msg args) stx))

  (define (check-type env-type rtype expr #:env [env #hash()])
    (check-equal? (expression->type expr env env-type fail) rtype))

  (define (check-fails type expr #:env [env #hash()])
    (check-equal?
     (let ([v #f])
       (expression->type expr env type (lambda _ (set! v #t)))
       v)
     #t))

  (check-type 'binary64 'binary64 #'4)
  (check-type 'binary64'binary64 #'x #:env #hash((x . binary64)))
  (check-type 'binary64 'binary64 #'(acos x) #:env #hash((x . binary64)))
  (check-fails 'binary64 #'(acos x) #:env #hash((x . bool)))
  (check-type 'binary64 'bool #'(and a b c) #:env #hash((a . bool) (b . bool) (c . bool)))
  (check-type 'binary64 'binary64 #'(if (== a 1) 1 0) #:env #hash((a . binary64)))
  (check-fails 'binary64 #'(if (== a 1) 1 0) #:env #hash((a . bool)))
  (check-type 'binary64 'bool #'(let ([a 1]) TRUE))
  (check-fails 'binary64 #'(if (== a 1) 1 TRUE) #:env #hash((a . binary64)))
  (check-type 'binary64 'binary64 #'(let ([a 1]) a) #:env #hash((a . bool))))
