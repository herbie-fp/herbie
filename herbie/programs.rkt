#lang racket

(require math/bigfloat)
(require math/flonum)
(require "common.rkt")
(require "syntax.rkt")

(provide (all-from-out "syntax.rkt")
         location-induct program-induct expression-induct location-hash
         location-do location-get location-parent location-sibling
         eval-prog replace-subexpr
         compile expression-cost program-cost
         free-variables unused-variables replace-expression valid-program?
         eval-exact eval-const-expr
         desugar-program)

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
		 (reverse location))]))
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
		       (map inductor (cdr prog))))]))

  (inductor prog))

;; (define (free-variables prog [bound constants])
;;   (filter (λ (v) (not (member v bound)))
;;           (match prog
;;             [(? constant?) '()]
;;             [(? variable?) (list prog)]
;;             [`(λ ,vars ,body)
;;              (free-variables body (append vars constants))]
;;             [`(lambda ,vars ,body)
;;              (free-variables body (append vars constants))]
;;             [`(,f ,args ...)
;;              (remove-duplicates (append-map (curryr free-variables bound) args))])))

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

(define (valid-program? prog)
  (define (valid-expression? expr vars)
    (match expr
      [(? constant?) #t]
      [(? variable?) (member expr vars)]
      [`(,f ,args ...)
       (and (andmap (curryr valid-expression? vars) args)
            (hash-has-key? (*operations*) f)
            (member (length args) (list-ref (hash-ref (*operations*) f) mode:args)))]
      [_ #f]))
  (match prog
    [(list (or 'λ 'lambda) vars body)
     (valid-expression? body vars)]
    [_
     #f]))

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
    [`(let* ,vars ,body)
     (let loop ([vars vars] [body body])
       (if (null? vars)
           body
           (let ([var (caar vars)] [val (cadar vars)])
             (loop (map (replace-var var val) (cdr vars))
                   ((replace-var var val) body)))))]
    [`(,head ,args ...)
     (cons head (map unfold-let args))]
    [x
     x]))

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

(define ((replace-var var val) expr)
  (cond
   [(eq? expr var) val]
   [(list? expr)
    (cons (car expr) (map (replace-var var val) (cdr expr)))]
   [#t
    expr]))

(define (desugar-program prog)
  (expand-associativity (unfold-let prog)))
