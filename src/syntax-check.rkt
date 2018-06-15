#lang racket

(require syntax/id-set)
(require "common.rkt" "syntax/syntax.rkt" "errors.rkt")
(provide assert-expression! assert-program!)

(define (check-expression* stx vars error!)
  (match stx
    [#`,(? constant?) (void)]
    [#`,(? variable? var)
     (unless (set-member? vars stx)
       (error! stx "Unknown variable ~a" var))]
    [#`(let ((#,vars* #,vals) ...) #,body)
     ;; These are unfolded by desugaring
     (for ([var vars*] [val vals])
       (unless (identifier? var)
         (error! var "Invalid variable name ~a" var))
       (check-expression* val vars error!))
     (check-expression* body (bound-id-set-union vars (immutable-bound-id-set vars*)) error!)]
    [#`(,(? (curry set-member? '(+ - * /))) #,args ...)
     ;; These expand associativity so we don't check the number of arguments
     (for ([arg args]) (check-expression* arg vars error!))]
    [#`(,(and (or 'sqr 'cube) f) #,args ...)
     (unless (= (length args) 1)
       (error! stx "Operator ~a given ~a arguments (expects 1)" f (length args)))
     (eprintf "Warning: the `sqr` and `cube` operators are deprecated and will be removed in later versions.\n")
     (for ([arg args]) (check-expression* arg vars error!))]
    [#`(,f #,args ...)
     (if (operator? f)
         (let ([num-args (operator-info f 'args)])
           (unless (or (set-member? num-args (length args)) (set-member? num-args '*))
             (error! stx "Operator ~a given ~a arguments (expects ~a)"
                     f (length args) (string-join (map ~a num-args) " or "))))
         (error! stx "Unknown operator ~a" f))
     (for ([arg args]) (check-expression* arg vars error!))]
    [_ (error! stx "Unknown syntax ~a" stx)]))

(define (check-property* prop error!)
  (unless (identifier? prop)
    (error! prop "Invalid property name ~a" prop))
  (define name (~a (syntax-e prop)))
  (unless (equal? (substring name 0 1) ":")
    (error! prop "Invalid property name ~a" prop)))

(define (check-properties* props vars error!)
  (define prop-dict
    (let loop ([props props] [out '()])
      (match props
        [(list (? identifier? prop-name) value rest ...)
         (check-property* prop-name error!)
         (loop rest (cons (cons (syntax-e prop-name) value) out))]
        [(list head)
         (check-property* head error!)
         (error! head "Property ~a has no value" head)
         out]
        [(list)
         out])))

  (when (dict-has-key? prop-dict ':name)
    (define name (dict-ref prop-dict ':name))
    (unless (string? (syntax-e name))
      (error! name "Invalid :name ~a; must be a string" name)))

  (when (dict-has-key? prop-dict ':description)
    (define desc (dict-ref prop-dict ':description))
    (unless (string? (syntax-e desc))
      (error! desc "Invalid :description ~a; must be a string" desc)))

  (when (dict-has-key? prop-dict ':cite)
    (define cite (dict-ref prop-dict ':cite))
    (unless (list? (syntax-e cite))
      (error! cite "Invalid :cite ~a; must be a list" cite))
    (when (list? (syntax-e cite))
      (for ([citation (syntax-e cite)] #:unless (identifier? citation))
        (error! citation "Invalid citation ~a; must be a variable name" citation))))

  (when (dict-has-key? prop-dict ':pre)
    (check-expression* (dict-ref prop-dict ':pre) vars error!))

  (when (dict-has-key? prop-dict ':herbie-target)
    (check-expression* (dict-ref prop-dict ':herbie-target) vars error!)))

(define (check-program* stx error!)
  (match stx
    [#`(FPCore #,vars #,props ... #,body)
     (unless (list? (syntax-e vars))
       (error! stx "Invalid arguments list ~a; must be a list" stx))
     (when (list? (syntax-e vars))
       (for ([var (syntax-e vars)] #:unless (identifier? var))
         (error! stx "Argument ~a is not a variable name" stx))
       (when (check-duplicate-identifier (syntax-e vars))
         (error! stx "Duplicate argument name ~a"
                 (check-duplicate-identifier (syntax-e vars)))))
     (define vars* (immutable-bound-id-set (if (list? (syntax-e vars)) (syntax-e vars) '())))
     (check-properties* props vars* error!)
     (check-expression* body vars* error!)]
    [_ (error! stx "Unknown syntax ~a" stx)]))

(define (assert-expression! stx vars)
  (define errs
    (reap [sow]
          (define (error! stx fmt . args)
            (define args* (map (λ (x) (if (syntax? x) (syntax->datum x) x)) args))
            (sow (cons stx (apply format fmt args*))))
          (check-expression* stx vars error!)))
  (unless (null? errs)
    (raise-herbie-syntax-error "Invalid expression" #:locations errs)))

(define (assert-program! stx)
  (define errs
    (reap [sow]
          (define (error! stx fmt . args)
            (define args* (map (λ (x) (if (syntax? x) (syntax->datum x) x)) args))
            (sow (cons stx (apply format fmt args*))))
          (check-program* stx error!)))
  (unless (null? errs)
    (raise-herbie-syntax-error "Invalid program" #:locations errs)))
