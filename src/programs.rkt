#lang racket

(require math/bigfloat math/flonum)
(require "common.rkt" "syntax/syntax.rkt" "errors.rkt")

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables ->flonum ->bf
         replace-leaves location-hash
         location-do location-get location-parent location-sibling
         eval-prog
         compile expression-cost program-cost
         free-variables unused-variables replace-expression
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
    (if (flag-set? 'precision 'double) real->double-flonum real->single-flonum))

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

(define/contract (replace-expression haystack needle needle*)
  (-> expr? expr? expr? expr?)
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
    [(list '/ a)
     (list '/ 1 (expand-associativity a))]
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
