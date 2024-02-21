#lang racket

(require syntax/id-set)
(require "../common.rkt" "../errors.rkt" "types.rkt" "syntax.rkt")
(provide assert-program!)

(define (check-expression* stx vars error! deprecated-ops)
  (let loop ([stx stx] [vars vars])
    (match stx
     [#`,(? number?) (void)]
     [#`,(? constant-operator?) (void)]
     [#`,(? variable? var)
      (unless (set-member? vars stx)
        (error! stx "Unknown variable ~a" var))]
     [#`(let* ((#,vars* #,vals) ...) #,body)
      (define bindings
        (for/fold ([vars vars]) ([var vars*] [val vals])
          (unless (identifier? var)
            (error! var "Invalid variable name ~a" var))
          (loop val vars)
          (bound-id-set-union vars (immutable-bound-id-set (list var)))))
      (loop body bindings)]
     [#`(let ((#,vars* #,vals) ...) #,body)
      ;; These are unfolded by desugaring
      (for ([var vars*] [val vals])
        (unless (identifier? var)
          (error! var "Invalid variable name ~a" var))
        (loop val vars))
      (loop body (bound-id-set-union vars (immutable-bound-id-set vars*)))]
     [#`(let #,varlist #,body)
      (error! stx "Invalid `let` expression variable list ~a" (syntax->datum varlist))
      (loop body vars)]
     [#`(let #,args ...)
      (error! stx "Invalid `let` expression with ~a arguments (expects 2)" (length args))
      (unless (null? args) (loop (last args) vars))]
     [#`(if #,cond #,ift #,iff)
      (loop cond vars)
      (loop ift vars)
      (loop iff vars)]
     [#`(if #,args ...)
      (error! stx "Invalid `if` expression with ~a arguments (expects 3)" (length args))
      (unless (null? args) (loop (last args) vars))]
     [#`(! #,props ... #,body)
      (check-properties* props '() error! deprecated-ops)
      (loop body vars)]
     [#`(,(? (curry set-member? '(+ * and or))) #,args ...)
      ;; Variary (minimum 0 arguments)
      (for ([arg args]) (loop arg vars))]
     [#`(,(? (curry set-member? '(- / = != < > <= >=))))
      ;; Variary (minimum 1 arg)
      (error! stx "Variary operator expects at least one argument")]
     [#`(,(? (curry set-member? '(- / = != < > <= >=))) #,args ...)
      ;; Variary (minimum 1 arg)
      (for ([arg args]) (loop arg vars))]
     [#`(,(? (curry set-member? '(+ - * / and or = != < > <= >=))) #,args ...)
      ;; These expand by associativity so we don't check the number of arguments
      (for ([arg args]) (loop arg vars))]
     [#`(#,f-syntax #,args ...)
      (define f (syntax->datum f-syntax))
      (cond
       [(operator-exists? f)
        (define arity (length (operator-info f 'itype)))
        (unless (= arity (length args))
          (error! stx "Operator ~a given ~a arguments (expects ~a)"
                  f (length args) arity))
        (when (operator-deprecated? f)
          (set-add! deprecated-ops f))]
       [(hash-has-key? (*functions*) f)
        (match-define (list vars _ _) (hash-ref (*functions*) f))
        (unless (= (length vars) (length args))
          (error! stx "Function ~a given ~a arguments (expects ~a)"
                  f (length args) (length vars)))]
       [else
        (error! stx "Unknown operator ~a" f)])
      (for ([arg args]) (loop arg vars))]
     [_ (error! stx "Unknown syntax ~a" (syntax->datum stx))])))

(define (check-property* prop error!)
  (unless (identifier? prop)
    (error! prop "Invalid property name ~a" prop))
  (define name (~a (syntax-e prop)))
  (unless (equal? (substring name 0 1) ":")
    (error! prop "Invalid property name ~a" prop)))

(define (check-properties* props vars error! deprecated-ops)
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

  ; TODO : not a string anymore so needs to be changed
  ; Change to data: symbol, number, string, (data*)
  ;(when (dict-has-key? prop-dict ':description)
    ;(define desc (dict-ref prop-dict ':description))
    ;(unless (string? (syntax-e desc))
    ; (error! desc "Invalid :description ~a; must be a string" desc))
    ;)

  (when (dict-has-key? prop-dict ':precision)
    (define prec (dict-ref prop-dict ':precision))
    (define known-repr? (get-representation (syntax->datum prec)))
    (unless known-repr?
      (error! prec "Unknown :precision ~a" prec)))

  (when (dict-has-key? prop-dict ':cite)
    (define cite (dict-ref prop-dict ':cite))
    (if (list? (syntax-e cite))
        (for ([citation (syntax-e cite)] #:unless (identifier? citation))
          (error! citation "Invalid citation ~a; must be a variable name" citation))
        (error! cite "Invalid :cite ~a; must be a list" cite)))   

  (when (dict-has-key? prop-dict ':pre)
    (check-expression* (dict-ref prop-dict ':pre) vars error! deprecated-ops))

  (when (dict-has-key? prop-dict ':alt)
    (check-expression* (dict-ref prop-dict ':alt) vars error! deprecated-ops))
  
  (void))

(define (check-program* stx vars props body error!)
  (unless (list? vars)
    (error! stx "Invalid arguments list ~a; must be a list" stx))
  (define vars* (filter identifier? vars))
  (when (list? vars)
    (for ([var vars] #:unless (identifier? var))
      (error! stx "Argument ~a is not a variable name" var))
   (when (check-duplicate-identifier vars*)
      (error! stx "Duplicate argument name ~a" (check-duplicate-identifier vars*))))
  (define deprecated-ops (mutable-set))
  (check-properties* props (immutable-bound-id-set vars*) error! deprecated-ops)
  (check-expression* body (immutable-bound-id-set vars*) error! deprecated-ops)
  (for ([op (in-set deprecated-ops)])
    (define message
      (format (syntax->error-format-string stx)
        (format "operator `~a` is deprecated." op)))
    (warn 'deprecated #:url "faq.html#deprecated-ops" message)))

(define (check-fpcore* stx error!)
  (match stx
   [#`(FPCore #,name (#,vars ...) #,props ... #,body)
    (unless (identifier? name)
      (error! stx "FPCore identifier must be a symbol: ~a" name))
    (check-program* stx vars props body error!)]
   [#`(FPCore (#,vars ...) #,props ... #,body)
    (check-program* stx vars props body error!)]
   [#`(FPCore #,something ...)
    (error! stx "FPCore not in a valid format: ~a" stx)]
   [_
    (error! stx "Not an FPCore: ~a" stx)]))

(define (assert-program! stx)
  (define errs
    (reap [sow]
          (define (error! stx fmt . args)
            (define args* (map (λ (x) (if (syntax? x) (syntax->datum x) x)) args))
            (sow (cons stx (apply format fmt args*))))
          (check-fpcore* stx error!)))
  (unless (null? errs)
    (raise-herbie-syntax-error "Invalid program" #:locations errs)))

;; testing FPCore format
(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-builtins)

  (define (get-errs stx)
    (reap [sow]
          (define (error! stx fmt . args)
            (define args* (map (λ (x) (if (syntax? x) (syntax->datum x) x)) args))
            (sow (cons stx (apply format fmt args*))))
          (check-fpcore* stx error!)))

  (check-pred (compose not null?) (get-errs #'a))
  (check-pred (compose not null?) (get-errs #'(FPCore)))
  (check-pred (compose not null?) (get-errs #'(FPCore foo 1)))
  (check-pred (compose not null?) (get-errs #'(FPCore foo x 1)))
  (check-pred (compose not null?) (get-errs #'(FPCore foo foo2 (x) x)))

  (check-pred null? (get-errs #'(FPCore (x) x)))
  (check-pred null? (get-errs #'(FPCore (x) :precision binary64 x)))
  (check-pred null? (get-errs #'(FPCore foo (x) x)))
  (check-pred null? (get-errs #'(FPCore foo (x) :precision binary64 x)))
)
