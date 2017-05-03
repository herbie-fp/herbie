#lang racket
(require "common.rkt" "syntax/syntax.rkt" "errors.rkt" "op-table.rkt")
(provide assert-program-type! assert-expression-type!)

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
  (match (or (syntax->list stx) (syntax-e stx))
    [(or 'TRUE 'FALSE) 'bool]
    [(? constant? x) 'real]
    [(? variable? x) (dict-ref env x)]
    [(list (app syntax-e (? operator? op)) exprs ...)
     (define sigs (get-sigs op (length exprs)))
     (unless sigs (error "Operator has no type signature" op (length exprs)))

     (define actual-types (for/list ([arg exprs]) (expression->type arg env error!)))
     (define rtype
       (for/or ([sig sigs])
         (argtypes->rtype actual-types sig)))
     (unless rtype
       (error! stx "Invalid arguments to ~a; expects ~a but got (~a ~a)" op
               (string-join
                (for/list ([sig sigs])
                  (match sig
                    [`((* ,atype) ,rtype)
                     (format "(~a <~a> ...)" op atype)]
                    [`((,atypes ...) ,rtype)
                     (format "(~a ~a)" op (string-join (map (curry format "<~a>") atypes) " "))]))
                " or ")
               op (string-join (map (curry format "<~a>") actual-types) " ")))
     rtype]
    [(list (app syntax-e 'let) (app syntax->list (list (app syntax->list (list (app syntax-e id) expr)) ...)) body)
     (define env2
       (for/fold ([env2 env]) ([var id] [val expr])
         (dict-set env2 var (expression->type val env error!))))
     (expression->type body env2 error!)]
    [(list (app syntax-e 'if) branch ifstmt elsestmt)
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
  (check-type 'real #'(let ([a 1]) a) #:env #hash((a . bool)))

  (check-type 'complex #'(complex 2 3))
  (check-type 'complex #'(+ (complex 1 2) (complex 3 4)))
  (check-fails #'(+ 2 (complex 1 2)))
  (check-type 'real #'(re (+ (complex 1 2) (complex 3 4)))))
