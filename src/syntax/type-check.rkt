#lang racket

(require "../common.rkt" "../errors.rkt" "syntax.rkt")
(provide assert-program-typed!)

(define (assert-program-typed! stx)
  (match-define (list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) props ... body) (syntax-e stx))
  (define props*
    (let loop ([props props])
      (match props
       [(list) (list)]
       [(list (app syntax-e prop) (app syntax-e value) rest ...)
        (cons (cons prop value) (loop rest))])))
  (define type (dict-ref props* ':precision 'binary64))
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
    [#`,(? real?) type]
    [#`,(? constant? x)
      (if (hash-has-key? parametric-constants x)
          (constant-info (get-parametric-constant x type) 'type)
          (constant-info x 'type))]
    [#`,(? variable? x) (dict-ref env x)]
    [#`(,(and (or '+ '- '* '/) op) #,exprs ...)
     (define t #f)
     (for ([arg exprs] [i (in-naturals)])
       (define actual-type (expression->type arg env type error!))
       (if (= i 0) (set! t actual-type) #f)
       (unless (equal? t actual-type)
         (error! stx "~a expects argument ~a of type ~a (not ~a)" op (+ i 1) t actual-type)))
     t]
    [#`(,(? (curry hash-has-key? parametric-operators) op) #,exprs ...)
     (define actual-types (for/list ([arg exprs]) (expression->type arg env type error!)))
     (define op* (get-parametric-operator op actual-types))
     (match (cons op* (operator-info op* 'otype))
       [(cons true-name rtype)
        (unless rtype
          (error! stx "Invalid arguments to ~a; expects ~a but got (~a ~a)" op
                  (string-join
                   (for/list ([sig (hash-ref parametric-operators op)])
                     (match sig
                       [(list _ rtype atypes ...)
                        (format "(~a ~a)" op (string-join (map (curry format "<~a>") atypes) " "))]
                       [(list* _ rtype atype)
                        (format "(~a <~a> ...)" op atype)]))
                   " or ")
                  op (string-join (map (curry format "<~a>") actual-types) " ")))
         rtype]
       [#f #f])]
    [#`(,(? operator? op) #,exprs ...)
     (define actual-types (for/list ([arg exprs]) (expression->type arg env type error!)))

     (define atypes (operator-info op 'itype))
     (unless (if (symbol? atypes)
                 (andmap (curry equal? atypes) actual-types)
                 (equal? atypes actual-types))
       (error! stx "Invalid arguments to ~a; expects ~a but got ~a"
               op
               (if (symbol? atypes)
                   (format "<~a> ..." atypes)
                   (string-join (map (curry format "<~a>") atypes) " "))
               (string-join (map (curry format "<~a>") actual-types) " ")))
     (operator-info op 'otype)]
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
      ifstmt-type]))

(module+ test
  (require rackunit)

  (define (fail stx msg . args)
    (error (apply format msg args) stx))

  (define (check-type type expr #:env [env #hash()])
    (check-equal? (expression->type expr env type fail) type))

  (define (check-fails type expr #:env [env #hash()])
    (check-equal?
     (let ([v #f])
       (expression->type expr env type (lambda _ (set! v #t)))
       v)
     #t))

  (check-type 'binary64 #'4)
  (check-type 'binary64 #'x #:env #hash((x . binary64)))
  (check-type 'binary64 #'(acos.f64 x) #:env #hash((x . binary64)))
  (check-fails 'binary64 #'(acos.f64 x) #:env #hash((x . bool)))
  (check-type 'bool #'(and a b c) #:env #hash((a . bool) (b . bool) (c . bool)))
  (check-type 'binary64 #'(if (== a 1) 1 0) #:env #hash((a . binary64)))
  (check-fails 'binary64 #'(if (== a 1) 1 0) #:env #hash((a . bool)))
  (check-fails 'binary64 #'(if (== a 1) 1 TRUE) #:env #hash((a . binary64)))
  (check-type 'bool #'(let ([a 1]) TRUE))
  (check-type 'binary64 #'(let ([a 1]) a) #:env #hash((a . bool))))
