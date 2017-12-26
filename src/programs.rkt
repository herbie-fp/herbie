#lang racket

(require math/bigfloat math/flonum syntax/id-set)
(require "common.rkt" "syntax/syntax.rkt" "errors.rkt")

(provide (all-from-out "syntax/syntax.rkt")
         location-induct program-induct expression-induct location-hash
         location-do location-get location-parent location-sibling
         eval-prog replace-subexpr
         compile expression-cost program-cost
         free-variables unused-variables replace-expression
         assert-expression! assert-program!
         eval-exact eval-const-expr
         desugar-program expr->prog expr?)

(define expr? (or/c list? symbol? number?))

(define (location-induct
	 prog
	 #:toplevel [toplevel (λ (expr location) expr)] #:constant [constant (λ (c location) c)]
	 #:variable [variable (λ (x location) x)] #:primitive [primitive (λ (list location) list)]
	 #:symbol [symbol-table (λ (sym location) sym)] #:predicate [predicate (λ (pred loc) pred)])

  (define (inductor prog location)
    (cond
     [(constant? prog)
      (constant prog (reverse location))]
     [(variable? prog)
      (variable prog (reverse location))]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog) (cons 2 location))])
	(toplevel `(λ ,(program-variables prog) ,body*) (reverse location)))]
     [(and (list? prog) (memq (car prog) predicates))
      (predicate (cons (symbol-table (car prog) (reverse (cons 0 location)))
                       (for/list ([idx (in-naturals)] [arg prog] #:when (> idx 0))
                         (inductor arg (cons idx location))))
		 (reverse location))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog) (reverse (cons 0 location)))
                       (for/list ([idx (in-naturals)] [arg prog] #:when (> idx 0))
                         (inductor arg (cons idx location))))
		 (reverse location))]
     [else (error (format "Invalid program ~a" prog))]))
  (inductor prog '()))

(define (location-hash prog)
  (define expr->locs (make-hash))

  (define (save expr loc)
    (hash-update! expr->locs expr (curry cons loc) '())
    expr)

  (location-induct prog #:constant save #:variable save #:primitive save)
  expr->locs)

(define (expression-induct
	 expr vars
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity] #:predicate [predicate identity])
  (program-body (program-induct
		 `(λ ,vars ,expr)
		 #:toplevel toplevel #:constant constant
		 #:variable variable #:primitive primitive
		 #:symbol symbol-table #:predicate predicate)))

(define (program-induct
         prog
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity] #:predicate [predicate identity])

  ; Inlined for speed
  (define (inductor prog)
    (cond
     [(constant? prog) (constant prog)]
     [(variable? prog) (variable prog)]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog))])
	(toplevel `(λ ,(program-variables prog) ,body*)))]
     [(and (list? prog) (memq (car prog) predicates))
      (predicate (cons (symbol-table (car prog))
		       (map inductor (cdr prog))))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog))
		       (map inductor (cdr prog))))]
     [else (error (format "Invalid program ~a" prog))]))

  (inductor prog))

(define (free-variables prog)
  (match prog
         [(? constant?) '()]
         [(? variable?) (list prog)]
         [`(lambda ,vars ,body)
           (remove* vars (free-variables body))]
         [`(,op ,args ...) ; TODO what if op unbound?
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
     (if (hash-has-key? (*operations*) f)
         (let ([num-args (list-ref (hash-ref (*operations*) f) mode:args)])
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

(define (replace-expression program from to)
  (cond
   [(equal? program from)
    to]
   [(list? program)
    (for/list ([subexpr program])
      (replace-expression subexpr from to))]
   [else
    program]))

(define (location-do loc prog f)
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
  (let* ([real->precision (if (equal? mode mode:bf) ->bf ->flonum)]
         [op->precision (λ (op) (list-ref (hash-ref (*operations*) op) mode))]
         [prog* (program-induct prog #:constant real->precision #:symbol op->precision)]
         [prog-opt `(λ ,(program-variables prog*) ,(compile (program-body prog*)))]
         [fn (eval prog-opt common-eval-ns)])
    (lambda (pts)
      (->flonum (apply fn (map real->precision pts))))))

;; Does the same thing as the above with mode:bf, but doesn't convert
;; the results back to floats.
(define (eval-exact prog)
  (let* ([prog* (program-induct prog #:constant ->bf #:symbol real-op->bigfloat-op)]
         [prog-opt `(lambda ,(program-variables prog*) ,(compile (program-body prog*)))]
         [fn (eval prog-opt common-eval-ns)])
    (lambda (pts)
      (apply fn (map ->bf pts)))))

(define (eval-const-expr expr)
  (let* ([expr_bf (expression-induct expr '() #:constant ->bf #:symbol real-op->bigfloat-op)])
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
        (let ([fn (caadr step)])
          (list-ref (hash-ref (*operations*) fn) mode:cost))
        1)))

(define (replace-subexpr prog needle needle*)
  `(λ ,(program-variables prog)
     ,(replace-expr-subexpr (program-body prog) needle needle*)))

(define (replace-expr-subexpr haystack needle needle*)
  (cond [(equal? haystack needle) needle*]
	[(list? haystack)
	 (cons (car haystack) (map (curryr replace-expr-subexpr needle* needle)
				   (cdr haystack)))]
	[#t haystack]))

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
