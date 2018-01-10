#lang racket
(require "common.rkt" "syntax/syntax.rkt" "errors.rkt")
(provide assert-program-type! assert-expression-type!)

(define (get-sig fun-name num-args)
  (if (and (operation? fun-name) (hash-has-key? (operator-info fun-name 'type) num-args))
      (hash-ref (operator-info fun-name 'type) num-args)
      (if (hash-has-key? (operator-info fun-name 'type) '*)
          (hash-ref (operator-info fun-name 'type) '*)
          #f)))

(define (get-params fun-name num-args)
  (and (get-sig fun-name num-args) (car (get-sig fun-name num-args))))

(define (get-rt-type fun-name num-args)
  (and (get-sig fun-name num-args) (cadr (get-sig fun-name num-args))))

;; Unit tests
;; Rewrite expression->type so that expr is a syntax object
;; Collect errors somewhere
;; error! is a function that takes (stx format . args) and puts it somewhere
(define (assert-program-type! stx)
  (match-define (list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) props ... body) (syntax-e stx))
  (assert-expression-type! body 'real #:env (for/hash ([var vars]) (values (syntax-e var) 'real))))

(define (assert-expression-type! stx expected-rtype #:env [env #hash()])
  (define errs
    (reap [sow]
          (define (error! stx fmt . args)
            (sow (cons stx (apply format fmt args))))
          (define actual-rtype (expression->type stx env error!))
          (unless (equal? expected-rtype actual-rtype)
            (error! stx "Expected program of type ~a, got type ~a" expected-rtype actual-rtype))))
  (unless (null? errs)
    (raise-herbie-syntax-error "Program has type errors" #:locations errs)))

(define (expression->type stx env error!)
  (match stx
    [(or #`TRUE #`FALSE) 'bool]
    [#`,(? constant? x) 'real]
    [#`,(? variable? x) (dict-ref env x)]
    [#`((and (or '+ '- '* '/) op) #, exprs ...)
     (for ([arg exprs] [i (in-naturals)])
       (define actual-type (expression->type arg env error!))
       (unless (equal? actual-type 'real)
         (error! stx "~a expects argument ~a of type ~a (not ~a)" op (+ i 1) 'real actual-type)))
     'real]
    [#`(,(? operation? op) #,exprs ...)
     (match (get-params op (length exprs))
       [(list '* each-type)
        (for ([arg exprs] [i (in-naturals)])
          (define actual-type (expression->type arg env error!))
          (unless (equal? actual-type each-type)
            (error! stx "~a expects argument ~a of type ~a (not ~a)" op (+ i 1) each-type actual-type)))]
       [(? list? param-types)
        (for/and ([arg exprs] [type param-types] [i (in-naturals)])
          (define actual-type (expression->type arg env error!))
          (unless (equal? actual-type type)
            (error! stx "~a expects argument ~a of type ~a (not ~a)" op (+ i 1) type actual-type)))]
       [_ (error "Operator has no type signature" op (length exprs))])
     (get-rt-type op (length exprs))]
    [#`(let ((,id #,expr) ...) #,body)
     (define env2
       (for/fold ([env2 env]) ([var id] [val expr])
         (dict-set env2 var (expression->type val env error!))))
     (expression->type body env2 error!)]
    [#`(if #,branch #,ifstmt #,elsestmt)
     (define branch-type (expression->type branch env error!))
     (unless (equal? branch-type 'bool)
       (error! stx "If statement has non-boolean type for branch ~a" branch-type))
     (define ifstmt-type (expression->type ifstmt env error!))
     (define elsestmt-type (expression->type elsestmt env error!))
     (unless (equal? ifstmt-type elsestmt-type)
       (error! stx "If statement has different types for if (~a) and else (~a)" ifstmt-type elsestmt-type))
      
      ifstmt-type]))

(module+ test
  (require rackunit)

  (define (fail stx msg . args)
    (error (apply format msg args) stx))

  (define (check-type type expr #:env [env #hash()])
    (check-equal? (expression->type expr env fail) type))

  (define (check-fails expr #:env [env #hash()])
    (check-equal?
     (let ([v #f])
       (expression->type expr env (lambda _ (set! v #t)))
       v)
     #t))

  (check-type 'real #'4)
  (check-type 'real #'x #:env #hash((x . real)))
  (check-type 'real #'(acos x) #:env #hash((x . real)))
  (check-fails #'(acos x) #:env #hash((x . bool)))
  (check-type 'bool #'(and a b c) #:env #hash((a . bool) (b . bool) (c . bool)))
  (check-type 'real #'(if (== a 1) 1 0) #:env #hash((a . real)))
  (check-fails #'(if (== a 1) 1 0) #:env #hash((a . bool)))
  (check-fails #'(if (== a 1) 1 TRUE) #:env #hash((a . real)))
  (check-type 'bool #'(let ([a 1]) TRUE))
  (check-type 'real #'(let ([a 1]) a) #:env #hash((a . bool))))
