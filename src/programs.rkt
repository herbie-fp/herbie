#lang racket

(require "common.rkt" "syntax/types.rkt" "syntax/syntax.rkt" "biginterval.rkt"
         "float.rkt" "interface.rkt")

(module+ test (require rackunit))

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables ->flonum ->bf
         type-of
         expr-supports?
         location-hash
         location? expr?
         location-do location-get
         batch-eval-progs eval-prog eval-const-expr eval-application
         free-variables replace-expression
         desugar-program resugar-program)

(define expr? (or/c list? symbol? value?))

(define location? (listof natural-number/c))

;; Programs are just lambda expressions

(define/contract (program-body prog)
  (-> expr? expr?)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  body)

(define/contract (program-variables prog)
  (-> expr? (listof symbol?))
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  vars)


;; `env` is in an indeterminate state, where it's a mapping from
;; variables to *either* types or reprs.
(define (type-of expr env)
  ;; Fast version does not recurse into functions applications
  (match expr
    [(? real?) 'real]
    [(? complex?) 'complex]
    ;; TODO(interface): Once we update the syntax checker to FPCore 1.1
    ;; standards, this will have to have more information passed in
    [(? value?) (representation-type (*output-repr*))]
    [(? constant?) (constant-info expr 'type)]
    [(? variable?)
     (match (dict-ref env expr)
       [(? symbol? t) t]
       [(? representation? r) (representation-type r)])]
    [(list 'if cond ift iff)
     (type-of ift env)]
    [(list op args ...)
     ;; Assumes single return type for any function
     (second (first (first (hash-values (operator-info op 'type)))))]))


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

(define (eval-prog prog mode repr)
  (batch-eval-progs (list prog) mode repr))

(define (batch-eval-progs progs mode repr)
  ; Keep exact numbers exact
  ;; TODO(interface): Right now, real->precision and precision->real are
  ;; mixed up for bf and fl because there is a mismatch between the fpbench
  ;; input format for how we specify complex numbers (which is the format
  ;; the interface will ultimately use), and the 1.3 herbie input format
  ;; (which has no way of specifying complex numbers as input.) Once types
  ;; and representations are cleanly distinguished, we can get rid of the
  ;; additional check to see if the repr is complex.
  (define real->precision (match mode
    ['bf (λ (repr x) (->bf x repr))]
    ['fl (λ (repr x) (->flonum x repr))]
    ['ival (λ (repr x) (mk-ival (->bf x repr)))]
    ['nonffi (λ (repr x) x)]))
  (define precision->real (match mode
    ['bf identity]
    ['fl (curryr ->flonum repr)]
    ['ival identity]
    ['nonffi identity]))

  (define (munge prog)
    (let inductor ([prog (program-body prog)])
      (match prog
        [(? value?) (real->precision repr prog)]
        [(? constant?) (list (constant-info prog mode))]
        [(? variable?) prog]
        [(list op args ...)
         (cons (operator-info op mode) (map inductor args))]
        [_ (error (format "Invalid program ~a" prog))])))

  (define vars (program-variables (first progs)))

  (define fn
    `(λ ,vars
       (let (,@(for/list ([var (in-list vars)])
                 (define repr (dict-ref (*var-reprs*) var))
                 `[,var (,(curry real->precision repr) ,var)]))
         (values
          ,@(for/list ([prog (in-list progs)])
              `(,precision->real ,(compile (munge prog))))))))
  (common-eval fn))

(define (eval-const-expr expr)
  ;; When we are in nonffi mode, we don't use repr, so pass in #f
  ((eval-prog `(λ () ,expr) 'nonffi #f)))

(define (eval-application op . args)
  (if (and (not (null? args)) (andmap (conjoin number? exact?) args))
      (with-handlers ([exn:fail:contract:divide-by-zero? (const #f)])
        (define res (eval-const-expr (cons op args)))
        (define type-info (operator-info op 'type))
        (match-define (list (list _ type))
                      (if (hash-has-key? type-info (length args))
                          (hash-ref type-info (length args))
                          (hash-ref type-info '*)))
        (and ((value-of type) res)
             (exact-value? type res)
             (val-to-type type res)))
      false))

(module+ test
  (define repr (get-representation 'binary64))
  (check-equal? (eval-application '+ 1 1) 2)
  (check-equal? (eval-application 'exp 2) #f)) ; Not exact

(module+ test
  (*var-reprs* (map (curryr cons (get-representation 'binary64)) '(a b c)))
  (require math/bigfloat)
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
      ;; When we are in ival mode, we don't use repr, so pass in #f
      (define iv (apply (eval-prog e 'ival (get-representation 'binary64)) p))
      (define val (apply (eval-prog e 'bf (get-representation 'binary64)) p))
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
     (replace-vars bindings (unfold-let body))]
    [`(let* () ,body)
     (unfold-let body)]
    [`(let* ([,var ,val] ,rest ...) ,body)
     (replace-vars (list (cons var val)) (unfold-let `(let* ,rest ,body)))]
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

;; TODO(interface): This needs to be changed once the syntax checker is updated
;; and supports multiple precisions
(define (expand-parametric expr prec var-precs)
  (define-values (expr* type)
    (let loop ([expr expr])
      ;; Run after unfold-let, so no need to track lets
      (match expr
        [(list (? (curry hash-has-key? parametric-operators) op) args ...)
         (define sigs (hash-ref parametric-operators op))
         (define-values (args* actual-types)
           (for/lists (args* actual-types) ([arg args])
             ;; TODO(interface): Right now we check if the actual-type is binary64
             ;; or binary32 because we don't have a distinction between them (both
             ;; are included in real). Once the operator code is fixed, this check
             ;; can be removed.
             (define-values (arg* actual-type) (loop arg))
             (if (set-member? '(binary64 binary32) actual-type)
               (values arg* 'real)
               (values arg* actual-type))))
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
         (values (cons op args*)
                 (second (first (first(hash-values (operator-info op 'type))))))]
        [(? real?) (values
                     (fl->repr expr (get-representation (match prec
                        ['real (if (flag-set? 'precision 'double) 'binary64 'binary32)]
                        [x x])))
                     prec)]
        [(? complex?) (values expr 'complex)]
        [(? value?) (values expr prec)]
        [(? constant?) (values expr (constant-info expr 'type))]
        [(? variable?) (values expr (dict-ref var-precs expr))])))
  expr*)

;; TODO(interface): This needs to be changed once the syntax checker is updated
;; and supports multiple precisions
(define (expand-parametric-reverse expr repr)
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
        [(? value?) (string->number (value->string expr repr))]
        [(? constant?) expr]
        [(? variable?) expr])))
  expr*)

(define (desugar-program prog prec var-precs)
  (expand-parametric (expand-associativity (unfold-let prog)) prec var-precs))

(define (resugar-program prog prec)
  (define repr (get-representation prec))
  (expand-parametric-reverse prog repr))

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
