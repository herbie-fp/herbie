#lang racket

(require math/bigfloat math/flonum)
(require "common.rkt" "syntax/syntax.rkt" "errors.rkt" "bigcomplex.rkt" "type-check.rkt"
         "biginterval.rkt" "syntax/softposit.rkt")

(module+ test (require rackunit))

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables ->flonum ->bf
         location-hash
         location? expr?
         location-do location-get
         eval-prog eval-const-expr
         compile expression-cost program-cost
         free-variables replace-expression
         desugar-program resugar-program)

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

(define/contract (->flonum x)
  (-> any/c (or/c flonum? complex? boolean?
                  posit8? posit16? posit32?
                  quire8? quire16? quire32?))
  (define convert
    (if (flag-set? 'precision 'double)
        real->double-flonum
        real->single-flonum))
  (cond
   [(real? x) (convert x)]
   [(bigfloat? x) (convert (bigfloat->flonum x))]
   [(bigcomplex? x)
    (make-rectangular (->flonum (bigcomplex-re x))
                      (->flonum (bigcomplex-im x)))]
   [(or (posit8? x) (posit16? x) (posit32? x)) x]
   [(or (quire8? x) (quire16? x) (quire32? x)) x]
   [(big-posit8? x) (double->posit8 (bigfloat->flonum (big-posit8-v x)))]
   [(big-posit16? x) (double->posit16 (bigfloat->flonum (big-posit16-v x)))]
   [(big-posit32? x) (double->posit32 (bigfloat->flonum (big-posit32-v x)))]
   [(big-quire8? x) (double->quire8 (bigfloat->flonum (big-quire8-v x)))]
   [(big-quire16? x) (double->quire16 (bigfloat->flonum (big-quire16-v x)))]
   [(big-quire32? x) (double->quire32 (bigfloat->flonum (big-quire32-v x)))]
   [(and (symbol? x) (constant? x))
    (->flonum ((constant-info x 'fl)))]
   [else x]))

(define (->bf x)
  (cond
   [(real? x) (bf x)]
   [(bigfloat? x) x]
   [(complex? x)
    (bigcomplex (->bf (real-part x)) (->bf (imag-part x)))]
   [(posit8? x)
    (big-posit8 (bf (posit8->double x)))]
   [(posit16? x)
    (big-posit16 (bf (posit16->double x)))]
   [(posit32? x)
    (big-posit32 (bf (posit32->double x)))]
   [(quire8? x)
    (big-quire8 (bf (posit8->double (quire8->posit8 x))))]
   [(quire16? x)
    (big-quire16 (bf (posit16->double (quire16->posit16 x))))]
   [(quire32? x)
    (big-quire32 (bf (posit32->double (quire32->posit32 x))))]
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

(define (free-variables prog)
  (match prog
    [(? constant?) '()]
    [(? variable?) (list prog)]
    [(list (or 'lambda 'λ) vars body)
           (remove* vars (free-variables body))]
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
        [(? real?) (real->precision prog)]
        [(? constant?) ((constant-info prog mode))]
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
        [(? real?)
         ;; cast to the correct type
         (match precision
           ['posit8 (values (list 'real->posit8 expr) 'posit8)]
           ['posit16 (values (list 'real->posit16 expr) 'posit16)]
           ['posit32 (values (list 'real->posit32 expr) 'posit32)]
           [_ (values expr 'real)])]
        [(? boolean?) (values expr 'bool)]
        [(? complex?) (values expr 'complex)]
        [(? posit8?) (values expr 'posit8)]
        [(? posit16?) (values expr 'posit16)]
        [(? posit32?) (values expr 'posit32)]
        [(? constant?) (values expr (constant-info expr 'type))]
        [(? variable?) (values expr (dict-ref ctx expr))])))
  expr*)

(define (expand-parametric-reverse expr)
  (define expr*
    (let loop ([expr expr])
      ;; Run after unfold-let, so no need to track lets
      (match expr
        [(list (? (curry hash-has-key? parametric-operators-reverse) op) args ...)
         (define args*
           (for/list ([arg args])
             (loop arg)))
         (define op* (hash-ref parametric-operators-reverse op))
         (cons op* args*)]
        [(list 'if cond ift iff)
         (define cond* (loop cond))
         (define ift* (loop ift))
         (define iff* (loop iff))
         (list 'if cond* ift* iff*)]
        [(list 'real->posit8 num) num]
        [(list 'real->posit16 num) num]
        [(list 'real->posit32 num) num]
        [(list op args ...)
         (define args* (for/list ([arg args]) (loop arg)))
         (cons op args*)]
        [(? real?) expr]
        [(? boolean?) expr]
        [(? complex?) expr]
        [(? posit8?) expr]
        [(? posit16?) expr]
        [(? posit32?) expr]
        [(? constant?) expr]
        [(? variable?) expr])))
  expr*)

(define (desugar-program prog ctx)
  (expand-parametric (expand-associativity (unfold-let prog)) ctx))

(define (resugar-program prog)
  (expand-parametric-reverse (expand-associativity (unfold-let prog))))

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
