#lang racket

(require math/bigfloat math/flonum syntax/id-set)
(require "common.rkt" "syntax/syntax.rkt" "errors.rkt")

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables ->flonum ->bf
         replace-leaves location-hash
         location-do location-get location-parent location-sibling
         eval-prog
         compile expression-cost program-cost
         free-variables unused-variables replace-expression
         assert-expression! assert-program!
         eval-exact eval-const-expr
         desugar-program expr->prog expr?)

(define expr? (or/c list? symbol? number?))

(define location? (listof natural-number/c))

;; Programs are just lambda expressions

(define/contract (program-body prog)
  (-> expr? expr?)
  (match-define (list (or 'lambda 'λ) (list vars ...) body) prog)
  body)

(define/contract (program-variables prog)
  (-> expr? (listof symbol?))
  (match-define (list (or 'lambda 'λ) (list vars ...) body) prog)
  vars)

;; Converting constants

(define (->flonum x)
  (define convert
    ((flag 'precision 'double) real->double-flonum real->single-flonum))

  (cond
   [(real? x) (convert x)]
   [(bigfloat? x) (convert (bigfloat->flonum x))]
   [(complex? x)
    (if (= (imag-part x) 0)
        (->flonum (real-part x))
        +nan.0)]
   [(constant? x)
    (convert ((constant-info x 'fl)))]
   [else x]))

(define (->bf x)
  (cond
   [(real? x) (bf x)]
   [(bigfloat? x) x]
   [(complex? x)
    (if (= (imag-part x) 0) (->bf (real-part x)) +nan.bf)]
   [(constant? x) ((constant-info x 'bf))]
   [else x]))

(define/contract (location-hash prog)
  (-> expr? (hash/c expr? (listof location?)))
  (define hash (make-hash))
  (define (save expr loc)
    (hash-update! hash expr (curry cons loc) '()))

  (let loop ([expr prog] [loc '()])
    (match expr
      [(list (or 'lambda 'λ) (list vars ...) body)
       (loop body (cons 2 loc))]
      [(? constant?) (save expr (reverse loc))]
      [(? variable?) (save expr (reverse loc))]
      [(list op args ...)
       (save expr (reverse loc))
       (for ([idx (in-naturals 1)] [arg args])
         (loop arg (cons idx loc)))]))

  hash)

(define/contract (replace-leaves prog #:constant [constant identity]
                                 #:variable [variable identity] #:symbol [symbol-table identity])
  (->* (expr?)
       (#:constant (-> constant? any/c) #:variable (-> variable? any/c) #:symbol (-> operator? any/c))
       any/c)

  ; Inlined for speed
  (define (inductor prog)
    (match prog
      [(list (or 'lambda 'λ) (list vars ...) body)
       `(λ ,vars ,(inductor body))]
      [(? constant?) (constant prog)]
      [(? variable?) (variable prog)]
      [(list 'if cond ift iff)
       `(if ,(inductor cond) ,(inductor ift) ,(inductor iff))]
      [(list op args ...)
       (cons (symbol-table op) (map inductor args))]
      [_ (error (format "Invalid program ~a" prog))]))

  (inductor prog))

(define (free-variables prog)
  (match prog
    [(? constant?) '()]
    [(? variable?) (list prog)]
    [(list (or 'lambda 'λ) vars body)
           (remove* vars (free-variables body))]
    [`(,op ,args ...)
     (remove-duplicates (append-map free-variables args))]))

(define (unused-variables prog)
  (remove* (free-variables (program-body prog))
           (program-variables prog)))

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

(define/contract (location-do loc prog f)
  (-> location? expr? (-> expr? expr?) expr?)
  (cond
   [(null? loc)
    (f prog)]
   [(not (pair? prog))
    (error "Bad location: cannot enter " prog "any further.")]
   [#t
    ; Inlined loop for speed
    (let loop ([idx (car loc)] [lst prog])
      (if (= idx 0)
          (cons (location-do (cdr loc) (car lst) f) (cdr lst))
          (cons (car lst) (loop (- idx 1) (cdr lst)))))]))

(define (location-get loc prog)
  ; Clever continuation usage to early-return
  (let/ec return
    (location-do loc prog return)))

(define (location-parent loc)
  (reverse (cdr (reverse loc))))

(define (location-sibling loc)
  (if (<= (length loc) 1)
      #f
      (let ([loc* (reverse loc)])
        (cond
         [(= (car loc*) 1)
          (reverse (cons 2 (cdr loc*)))]
         [(= (car loc*) 2)
          (reverse (cons 1 (cdr loc*)))]
         [else
          #f]))))

(define (eval-prog prog mode)
  (let* ([real->precision (if (equal? mode 'bf) ->bf ->flonum)]
         [op->precision (λ (op) (operator-info op mode))] ; TODO change use of mode
         [body* (replace-leaves (program-body prog) #:constant real->precision #:symbol op->precision)]
         [prog-opt `(λ ,(program-variables prog) ,(compile body*))]
         [fn (eval prog-opt common-eval-ns)])
    (lambda (pts)
      (->flonum (apply fn (map real->precision pts))))))

;; Does the same thing as the above with mode 'bf, but doesn't convert
;; the results back to floats.
(define (eval-exact prog)
  (let* ([body* (replace-leaves (program-body prog) #:constant ->bf #:symbol (curryr operator-info 'bf))]
         [prog-opt `(lambda ,(program-variables prog) ,(compile body*))]
         [fn (eval prog-opt common-eval-ns)])
    (lambda (pts)
      (apply fn (map ->bf pts)))))

(define (eval-const-expr expr)
  (let* ([expr_bf (replace-leaves expr #:constant ->bf #:symbol (curryr operator-info 'bf))])
    (->flonum (eval expr_bf common-eval-ns))))

;; To compute the cost of a program, we could use the tree as a
;; whole, but this is inaccurate if the program has many common
;; subexpressions.  So, we compile the program to a register machine
;; and use that to estimate the cost.

(define (compile expr)
  (define assignments '())
  (define compilations (make-hash))

  ;; TODO : use one of Racket's memoization libraries
  (define (compile-one expr)
    (hash-ref!
     compilations expr
     (λ ()
       (let ([expr* (if (list? expr)
			(let ([fn (car expr)] [children (cdr expr)])
			  (cons fn (map compile-one children)))
			expr)]
	     [register (gensym "r")])
	 (set! assignments (cons (list register expr*) assignments))
	 register))))

  (let ([reg (compile-one expr)])
    `(let* ,(reverse assignments) ,reg)))

(define (program-cost prog)
  (expression-cost (program-body prog)))

(define (expression-cost expr)
  (for/sum ([step (second (compile expr))])
    (if (list? (second step))
        (operator-info (caadr step) 'cost)
        1)))

(define (replace-expression haystack needle needle*)
  (match haystack
   [(== needle) needle*]
   [(list (or 'lambda 'λ) (list vars ...) body)
    `(λ ,vars ,(replace-expression body needle needle*))]
   [(list op args ...)
    (cons op (map (curryr replace-expression needle needle*) args))]
   [x x]))

(module+ test
  (require rackunit)
  (check-equal? (replace-expression '(λ (x) (- x (sin x))) 'x 1)
                '(λ (x) (- 1 (sin 1)))))

(define (unfold-let expr)
	(match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define bindings (map cons vars vals))
		 (unfold-let (replace-vars bindings body))]
		[`(,head ,args ...)
			(cons head (map unfold-let args))]
		[x x]))

(define (expand-associativity expr)
  (match expr
    [(list (? (curryr member '(+ - * /)) op) a ..2 b)
     (list op
           (expand-associativity (cons op a))
           (expand-associativity b))]
    [(list op a ...)
     (cons op (map expand-associativity a))]
    [_
     expr]))

(define (replace-vars dict expr)
  (cond
    [(dict-has-key? dict expr) (dict-ref dict expr)]
    [(list? expr)
     (cons (replace-vars dict (car expr)) (map (curry replace-vars dict) (cdr expr)))]
    [#t expr]))

(define ((replace-var var val) expr)
  (cond
   [(eq? expr var) val]
   [(list? expr)
    (cons (car expr) (map (replace-var var val) (cdr expr)))]
   [#t
    expr]))

(define (desugar-program prog)
  (expand-associativity (unfold-let prog)))

(define (expr->prog expr)
  `(lambda ,(free-variables expr) ,expr))
