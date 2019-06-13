#lang racket

(require math/bigfloat math/flonum)
(require "common.rkt" "syntax/types.rkt" "syntax/syntax.rkt" "syntax/optional.rkt")
(require "errors.rkt" "type-check.rkt" "biginterval.rkt" "float.rkt" "interface.rkt")

(module+ test (require rackunit))

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables ->flonum ->bf
         expr-supports?
         location-hash
         location? expr?
         location-do location-get
         eval-prog eval-const-expr
         compile expression-cost program-cost
         free-variables replace-expression
         desugar-program resugar-program)

(define expr? (or/c list? symbol? value?))

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

(define (free-variables prog)
  (match prog
    [(? constant?) '()]
    [(? variable?) (list prog)]
    [`(,op ,args ...)
     (remove-duplicates (append-map free-variables args))]))

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

(define (eval-prog prog mode)
  (define real->precision (match mode ['bf ->bf] ['fl ->flonum] ['ival mk-ival] ['nonffi identity])) ; Keep exact numbers exact
  (define precision->real (match mode ['bf identity] ['fl ->flonum] ['ival identity] ['nonffi identity]))

  (define body*
    (let inductor ([prog (program-body prog)])
      (match prog
        [(? value?) (real->precision prog)]
        [(? constant?) (list (constant-info prog mode))]
        [(? variable?) prog]
        #;[(list 'if cond ift iff)
         `(if ,(inductor cond) ,(inductor ift) ,(inductor iff))]
        [(list op args ...)
         (cons (operator-info op mode) (map inductor args))]
        [_ (error (format "Invalid program ~a" prog))])))
  (define fn (common-eval `(λ ,(program-variables prog) ,(compile body*))))
  (lambda (pts)
    (precision->real (apply fn (map real->precision pts)))))

(define (eval-const-expr expr)
  ((eval-prog `(λ () ,expr) 'nonffi) '()))

(module+ test
  (check-equal? (eval-const-expr '(+ 1 1)) 2)
  (check-equal? (eval-const-expr 'PI) pi)
  (check-equal? (eval-const-expr '(exp 2)) (exp 2)))

(module+ test
  (define tests
    #hash([(λ (a b c) (/ (- (sqrt (- (* b b) (* a c))) b) a))
           . (-1.918792216976527e-259 8.469572834134629e-97 -7.41524568576933e-282)
           ])) ;(2.4174342574957107e-18 -1.4150052601637869e-40 -1.1686799408259549e+57)

  (define (in-interval? iv pt)
    (match-define (ival lo hi err? err) iv)
    (and (bf<= lo pt) (bf<= pt hi)))

  (define-binary-check (check-in-interval? in-interval? interval point))

  (for ([(e p) (in-hash tests)])
    (parameterize ([bf-precision 4000])
      (define iv ((eval-prog e 'ival) p))
      (define val ((eval-prog e 'bf) p))
      (check bf<= (ival-lo iv) (ival-hi iv))
      (check-in-interval? iv val))))

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
  (check-equal? (replace-expression '(λ (x) (- x (sin x))) 'x 1)
                '(λ (x) (- 1 (sin 1))))

  (check-equal?
   (replace-expression
    '(/ (cos (* 2 x)) (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
    'cos
    '(/ 1 cos))
   '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))


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

(define (expand-parametric expr ctx)
  (define precision (if (and (list? ctx) (not (empty? ctx))) (cdr (first ctx)) 'real))
  (define-values (expr* type)
    (let loop ([expr expr])
      ;; Run after unfold-let, so no need to track lets
      (match expr
        [(list (? (curry hash-has-key? parametric-operators) op) args ...)
         (define sigs (hash-ref parametric-operators op))
         (define-values (args* actual-types)
           (for/lists (args* actual-types) ([arg args])
             (loop arg)))
         (match-define (cons op* rtype)
           (for/or ([sig sigs])
             (match-define (list* true-name rtype atypes) sig)
             (and
              (if (symbol? atypes)
                  (andmap (curry equal? atypes) actual-types)
                  (if (set-member? variary-operators op)
                      (and (andmap (λ (x) (eq? (car actual-types) x)) actual-types)
                           (eq? (car actual-types) (car atypes)))
                      (equal? atypes actual-types)))
              (cons true-name rtype))))
         (values (cons op* args*) rtype)]
        [(list 'if cond ift iff)
         (define-values (cond* _a) (loop cond))
         (define-values (ift* rtype) (loop ift))
         (define-values (iff* _b) (loop iff))
         (values (list 'if cond* ift* iff*) rtype)]
        [(list op args ...)
         (define-values (args* _) (for/lists (args* _) ([arg args]) (loop arg)))
         (values (cons op args*) (second (first (first (hash-values (operator-info op 'type))))))]
        [(? real?) (values (fl->repr expr (get-representation (match precision ['real (if (flag-set? 'precision 'double) 'binary64 'binary32)] [x x]))) precision)]
        [(? complex?) (values expr 'complex)]
        [(? value?) (values expr (representation-name (infer-representation expr)))]
        [(? constant?) (values expr (constant-info expr 'type))]
        [(? variable?) (values expr (dict-ref ctx expr))])))
  expr*)

(define (expand-parametric-reverse expr)
  (define expr*
    (let loop ([expr expr])
      ;; Run after unfold-let, so no need to track lets
      (match expr
        [(list (? (curry hash-has-key? parametric-operators-reverse) op) args ...)
         (define args* (for/list ([arg args]) (loop arg)))
         (define op* (hash-ref parametric-operators-reverse op))
         (cons op* args*)]
        [(list 'if cond ift iff)
         (list 'if (loop cond) (loop ift) (loop iff))]
        [(list op args ...)
         (cons op (for/list ([arg args]) (loop arg)))]
        [(? (conjoin complex? (negate real?))) expr]
        [(? value?) (repr->fl expr (infer-representation expr))]
        [(? constant?) expr]
        [(? variable?) expr])))
  expr*)

(define (desugar-program prog ctx)
  (expand-parametric (expand-associativity (unfold-let prog)) ctx))

(define (resugar-program prog)
  (expand-parametric-reverse prog))

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

(define (expr-supports? expr field)
  (let loop ([expr expr])
    (match expr
      [(list op args ...)
       (and (operator-info op field) (andmap loop args))]
      [(? variable?) true]
      [(? constant?) (or (not (symbol? expr)) (constant-info expr field))])))
