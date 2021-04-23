#lang racket

(require syntax/id-set)
(require "../common.rkt" "../errors.rkt" "../interface.rkt" "syntax.rkt")
(provide assert-program!)

(define (check-expression* stx vars error!)
  (match stx
    [#`,(? constant?) (void)]
    [#`,(? variable? var)
     (unless (set-member? vars stx)
       (error! stx "Unknown variable ~a" var))]
    [#`(let* ((#,vars* #,vals) ...) #,body)
     (define bindings
       (for/fold ([vars vars]) ([var vars*] [val vals])
         (unless (identifier? var)
           (error! var "Invalid variable name ~a" var))
         (check-expression* val vars error!)
         (bound-id-set-union vars (immutable-bound-id-set (list var)))))
     (check-expression* body bindings error!)]
    [#`(let ((#,vars* #,vals) ...) #,body)
     ;; These are unfolded by desugaring
     (for ([var vars*] [val vals])
       (unless (identifier? var)
         (error! var "Invalid variable name ~a" var))
       (check-expression* val vars error!))
     (check-expression* body (bound-id-set-union vars (immutable-bound-id-set vars*)) error!)]
    [#`(let #,varlist #,body)
     (error! stx "Invalid `let` expression variable list ~a" (syntax->datum varlist))
     (check-expression* body vars error!)]
    [#`(let #,args ...)
     (error! stx "Invalid `let` expression with ~a arguments (expects 2)" (length args))
     (unless (null? args) (check-expression* (last args) vars error!))]
    [#`(if #,cond #,ift #,iff)
     (check-expression* cond vars error!)
     (check-expression* ift vars error!)
     (check-expression* iff vars error!)]
    [#`(if #,args ...)
     (error! stx "Invalid `if` expression with ~a arguments (expects 3)" (length args))
     (unless (null? args) (check-expression* (last args) vars error!))]
    [#`(! #,props ... #,body)
     (check-properties* props '() body)
     (check-expression* body vars error!)]
    [#`(,(? (curry set-member? '(+ - * /))) #,args ...)
     ;; These expand associativity so we don't check the number of arguments
     (for ([arg args]) (check-expression* arg vars error!))]
    [#`(#,f-syntax #,args ...)
     (define f (syntax->datum f-syntax))
     (if (hash-has-key? parametric-operators f)
         (let ([arity (get-operator-arity f)]) ;; variary is (#f . #f)
           (unless (or (not arity) (= arity (length args)))
             (error! stx "Operator ~a given ~a arguments (expects ~a)"
                     f (length args) arity)))
         (error! stx "Unknown operator ~a" f))
     (for ([arg args]) (check-expression* arg vars error!))]
    [_ (error! stx "Unknown syntax ~a" (syntax->datum stx))]))

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

  (when (dict-has-key? prop-dict ':precision)
    (define prec (dict-ref prop-dict ':precision))
    (define known-repr? (generate-repr (syntax-e* prec)))
    (unless known-repr?
      (error! prec "Unknown :precision ~a" prec)))

  (when (dict-has-key? prop-dict ':cite)
    (define cite (dict-ref prop-dict ':cite))
    (if (list? (syntax-e cite))
        (for ([citation (syntax-e cite)] #:unless (identifier? citation))
          (error! citation "Invalid citation ~a; must be a variable name" citation))
        (error! cite "Invalid :cite ~a; must be a list" cite)))   

  (when (dict-has-key? prop-dict ':pre)
    (check-expression* (dict-ref prop-dict ':pre) vars error!))

  (when (dict-has-key? prop-dict ':herbie-target)
    (check-expression* (dict-ref prop-dict ':herbie-target) vars error!))
    
  (when (dict-has-key? prop-dict ':herbie-conversions)
    (define conversions (dict-ref prop-dict ':herbie-conversions))
    (if (list? (syntax-e conversions))
        (for ([conv (syntax-e* conversions)])
          (unless (and (list? conv) (= (length conv) 2))
            (error! conversions "Invalid conversion ~a; Valid example: (binary64 binary32)" conv))
          (define known-repr? (and (generate-repr (first conv)) (generate-repr (second conv))))
          (unless known-repr?
            (error! conversions "Unknown precision in conversion ~a" conv)))
        (error! conversions "Invalid :herbie-conversions ~a; must be a list" conversions))))

(define (check-program* stx error!)
  (define-values (vars props body)
    (match (syntax-e stx)
     [(list (app syntax-e 'FPCore) (app syntax-e name) (app syntax-e (list vars ...)) props ... body)
      (unless (symbol? name)
        (error! stx "FPCore identifier must be a symbol: ~a" name))
      (values vars props body)]
     [(list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) props ... body)
      (values vars props body)]
     [(list (app syntax-e 'FPCore) something ...)
      (error! stx "FPCore not in a valid format" stx)]
     [_ (error! stx "Not an FPCore: ~a" stx)]))
  (unless (list? vars)
    (error! stx "Invalid arguments list ~a; must be a list" stx))
  (define vars* (filter identifier? vars))
  (when (list? vars)
    (for ([var vars] #:unless (identifier? var))
      (error! stx "Argument ~a is not a variable name" var))
   (when (check-duplicate-identifier vars*)
      (error! stx "Duplicate argument name ~a" (check-duplicate-identifier vars*))))
  (check-properties* props (immutable-bound-id-set vars*) error!)
  (check-expression* body (immutable-bound-id-set vars*) error!))

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
