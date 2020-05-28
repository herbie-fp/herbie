#lang racket

(require math/bigfloat rival)
(require "common.rkt" "syntax/types.rkt" "syntax/syntax.rkt" "float.rkt" "interface.rkt")

(module+ test (require rackunit))

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables ->flonum ->bf
         type-of
         expr-supports?
         location-hash
         location? expr?
         location-do location-get
         batch-eval-progs eval-prog eval-application
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
    ;; TODO(interface): Once we update the syntax checker to FPCore 1.1
    ;; standards, this will have to have more information passed in
    [(? value?) (type-name (representation-type (*output-repr*)))]
    [(? constant?) (constant-info expr 'type)]
    [(? variable?)
     (match (dict-ref env expr)
       [(? symbol? t) t]
       [(? representation? r) (type-name (representation-type r))])]
    [(list 'if cond ift iff)
     (type-of ift env)]
    [(list op args ...)
     (operator-info op 'otype)]))


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
  (define f (batch-eval-progs (list prog) mode repr))
  (λ args (vector-ref (apply f args) 0)))

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
  
  (define vars (program-variables (first progs)))
  (define var-reprs (map (curry dict-ref (*var-reprs*)) vars))

  (define exprs '())
  (define exprhash
    (make-hash
     (for/list ([var vars] [i (in-naturals)])
       (cons var i))))

  (define (munge prog repr)
    (define expr
      (match prog
        [(? value?) (list (const (real->precision repr prog)))]
        [(? constant?) (list (constant-info prog mode))]
        [(? variable?) prog]
        [`(if ,c ,t ,f)
         (list (operator-info 'if mode)
               (munge c (get-representation 'bool))
               (munge t repr)
               (munge f repr))]
        [(list op args ...)
         (define atypes
           (match (operator-info op 'itype)
             [(? list? as) as]
             [(? type-name? a) (map (const a) args)]))
         (unless (= (length atypes) (length args))
           (raise-argument-error 'eval-prog "expr?" prog))
         (cons (operator-info op mode)
               (for/list ([arg args] [atype atypes])
                 (munge arg (get-representation* atype))))]
        [_ (raise-argument-error 'eval-prog "expr?" prog)]))

    (hash-ref! exprhash expr
               (λ ()
                 (define n (+ (length exprs) (length vars)))
                 (set! exprs (cons expr exprs))
                 n)))
  
  (define names
    (for/list ([prog progs])
      (munge (program-body prog) repr)))
  (define l1 (length vars))
  (define lt (+ (length exprs) l1))
  (define exprvec (list->vector (reverse exprs)))
  (λ args
    (define v (make-vector lt))
    (for ([arg (in-list args)] [n (in-naturals)] [var (in-list vars)] [repr (in-list var-reprs)])
      (vector-set! v n (real->precision repr arg)))
    (for ([expr (in-vector exprvec)] [n (in-naturals l1)])
      (define tl
        (for/list ([arg (in-list (cdr expr))])
          (vector-ref v arg)))
      (vector-set! v n (apply (car expr) tl)))
    (for/vector ([n (in-list names)])
      (precision->real (vector-ref v n)))))

(module+ test
  (*var-reprs* (map (curryr cons (get-representation 'binary64)) '(a b c)))
  (require math/bigfloat)
  (define tests
    #hash([(λ (a b c) (/ (- (sqrt (- (* b b) (* a c))) b) a))
           . (-1.918792216976527e-259 8.469572834134629e-97 -7.41524568576933e-282)
           ])) ;(2.4174342574957107e-18 -1.4150052601637869e-40 -1.1686799408259549e+57)

  (define-simple-check (check-in-interval? iv pt)
    (match-define (ival lo hi) iv)
    (and (bf<= lo pt) (bf<= pt hi)))

  (for ([(e p) (in-hash tests)])
    (parameterize ([bf-precision 4000])
      ;; When we are in ival mode, we don't use repr, so pass in #f
      (define iv (apply (eval-prog e 'ival (get-representation 'binary64)) p))
      (define val (apply (eval-prog e 'bf (get-representation 'binary64)) p))
      (check-in-interval? iv val))))

(define (eval-application op . args)
  (if (and (not (null? args)) (andmap (conjoin number? exact?) args))
      (with-handlers ([exn:fail:contract:divide-by-zero? (const #f)])
        (define fn (operator-info op 'nonffi))
        (define res (apply fn args))
        (define rtype (operator-info op 'otype))
        (and ((value-of rtype) res)
             (exact-value? rtype res)
             (value->code rtype res)))
      false))

(module+ test
  (define repr (get-representation 'binary64))
  (check-equal? (eval-application '+ 1 1) 2)
  (check-equal? (eval-application 'exp 2) #f)) ; Not exact

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
    [(list (and (or '+ '- '* '/) op) a ..2 b)
     (list op
           (expand-associativity (cons op a))
           (expand-associativity b))]
    [(list (or '+ '*) a) (expand-associativity a)]
    [(list '- a) (list '- (expand-associativity a))]
    [(list '/ a) (list '/ 1 (expand-associativity a))]
    [(list (or '+ '-)) 0]
    [(list (or '* '/)) 1]
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
         ;; Match guaranteed to succeed because we ran type-check first
         (match-define (cons op* rtype) (get-parametric-operator op actual-types))
         (values (cons op* args*) rtype)]
        [(list 'if cond ift iff)
         (define-values (cond* _a) (loop cond))
         (define-values (ift* rtype) (loop ift))
         (define-values (iff* _b) (loop iff))
         (values (list 'if cond* ift* iff*) rtype)]
        [(list op args ...)
         (define-values (args* _) (for/lists (args* _) ([arg args]) (loop arg)))
         (values (cons op args*) (operator-info op 'otype))]
        [(? real?) (values (fl->repr expr (get-representation* prec)) prec)]
        [(? value?) (values expr prec)]
        [(? constant?) (values expr (constant-info expr 'type))]
        [(? variable?) (values expr (dict-ref var-precs expr))])))
  expr*)

;; TODO(interface): This needs to be changed once the syntax checker is updated
;; and supports multiple precisions
(define (expand-parametric-reverse expr repr)
  (define ->bf (representation-repr->bf repr))
  (match expr
    [(list op args ...)
     (define op* (hash-ref parametric-operators-reverse op op))
     (define atypes
       (match (operator-info op 'itype)
         [(? list? as) as]
         [(? type-name? a) (map (const a) args)]))
     (define args*
       (for/list ([arg args] [type atypes])
         (expand-parametric-reverse arg (get-representation* type))))
     (cons op args*)]
    [(? (conjoin complex? (negate real?)))
     `(complex ,(real-part expr) ,(imag-part expr))]
    [(? value?)
     (match (bigfloat->flonum (->bf expr))
       [-inf.0 '(- INFINITY)] ; not '(neg INFINITY) because this is post-resugaring
       [+inf.0 'INFINITY]
       [+nan.0 'NAN]
       [x x])]
    [(? constant?) expr]
    [(? variable?) expr]))

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

(define (expr-supports? expr field)
  (let loop ([expr expr])
    (match expr
      [(list op args ...)
       (and (operator-info op field) (andmap loop args))]
      [(? variable?) true]
      [(? constant?) (or (not (symbol? expr)) (constant-info expr field))])))
