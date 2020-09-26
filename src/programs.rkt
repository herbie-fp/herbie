#lang racket

(require math/bigfloat rival)
(require "syntax/types.rkt" "syntax/syntax.rkt" "float.rkt" "interface.rkt")

(module+ test (require rackunit))

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables
         type-of repr-of
         expr-supports?
         location-hash
         location? expr?
         location-do location-get location-do-multiple
         batch-eval-progs eval-prog eval-application
         free-variables replace-expression
         desugar-program resugar-program
         unary-op-with-repr binary-op-with-repr)

(define expr? (or/c list? symbol? value? real?))

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

;; Returns type name
;; Fast version does not recurse into functions applications
;; TODO(interface): Once we update the syntax checker to FPCore 1.1
;; standards, this will have to have more information passed in
(define (type-of expr repr env)
  (match expr
   [(? real?) 'real]
   [(? value?) (type-name (representation-type repr))]
   [(? constant?) 
    (type-name (representation-type (get-representation (constant-info expr 'type))))]
   [(? variable?) (type-name (representation-type (dict-ref env expr)))]
   [(list 'if cond ift iff) (type-of ift env)]
   [(list op args ...) ; repr-name -> repr -> type
    (type-name (representation-type (get-representation (operator-info op 'otype))))]))

;; Returns repr name
;; Fast version does not recurse into functions applications
(define (repr-of expr repr env)
  (match expr
   [(? real?) (representation-name repr)]
   [(? value?) (representation-name repr)]
   [(? constant?) (constant-info expr 'type)]
   [(? variable?) (representation-name (dict-ref env expr))]
   [(list 'if cond ift iff) (repr-of ift repr env)]
   [(list 'd subexpr var) (repr-of subexpr repr env)]
   [(list 'subst subexpr var val) (repr-of subexpr repr env)] ;; we assume subst isn't changing reprs
   [(list 'try-/ a sub1 sub2 hist) (repr-of a repr env)]
   [(list 'took-substitution hist var value) (repr-of value repr env)]
   [(list 'took-derivative hist var) (repr-of var repr env)]
   [(list 'lim num denom cnum cdenom var val) (repr-of num repr env)]
   [(list op args ...) (operator-info op 'otype)]))

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

(define (location-do-multiple loc prog f)
  (define val (location-get loc prog))
  (define variants (f val))
  (for/list ([v variants])
    (location-do loc prog (lambda (x) v))))

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
    ['fl (λ (repr x) (real->repr x repr))]
    ['ival (λ (repr x) (if (ival? x) x (mk-ival (->bf x repr))))]
    ['nonffi (λ (repr x) x)]))
  
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
        [(? real?) (list (const (real->precision repr prog)))]
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
             [(? representation-name? a) (map (const a) args)] ; must be first
             [(? list? as) as]))
         (unless (= (length atypes) (length args))
           (raise-argument-error 'eval-prog "expr?" prog))
         (cons (operator-info op mode)
               (for/list ([arg args] [type atypes])
                 (munge arg (get-representation type))))]
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
      (vector-ref v n))))

(module+ test
  (*var-reprs* (map (curryr cons (get-representation 'binary64)) '(a b c)))
  (require math/bigfloat)
  (define tests
    #hash([(λ (a b c) (/.f64 (-.f64 (sqrt.f64 (-.f64 (*.f64 b b) (*.f64 a c))) b) a))
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

(define (exact-value? type val)
  (match type
    [(or 'real 'complex) (exact? val)]
    ['boolean true]
    [_ false]))

(define (value->code type val)
  (match type
    ['real val]
    ['complex (list 'complex (real-part val) (imag-part val))]
    ['boolean (if val 'TRUE 'FALSE)]))

(define (eval-application op . args)
  (if (and (not (null? args)) (andmap (conjoin number? exact?) args))
      (with-handlers ([exn:fail:contract:divide-by-zero? (const #f)])
        (define fn (operator-info op 'nonffi))
        (define res (apply fn args))
        (define repr (get-representation (operator-info op 'otype)))
        (define rtype (type-name (representation-type repr)))
        (and ((value-of rtype) res)
             (exact-value? rtype res)
             (value->code rtype res)))
      false))

(module+ test
  (define repr (get-representation 'binary64))
  (check-equal? (eval-application '+.f64 1 1) 2)
  (check-equal? (eval-application 'exp.f64 2) #f)) ; Not exact

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


(define (unary-op-with-repr op child)
  (define repr (repr-of child (*output-repr*) (*var-reprs*)))
  (get-parametric-operator op repr))

(define (binary-op-with-repr op child)
  (define repr (repr-of child (*output-repr*) (*var-reprs*)))
  (get-parametric-operator op repr repr))

(define (unfold-let expr)
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define bindings (map cons vars (map unfold-let vals)))
     (replace-vars bindings (unfold-let body))]
    [`(let* () ,body)
     (unfold-let body)]
    [`(let* ([,var ,val] ,rest ...) ,body)
     (replace-vars (list (cons var (unfold-let val))) (unfold-let `(let* ,rest ,body)))]
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
(define (expand-parametric expr repr var-reprs full?)
  (define-values (expr* prec)
    (let loop ([expr expr] [prec (representation-name repr)]) ; easier to work with repr names
      ;; Run after unfold-let, so no need to track lets
      (match expr
        [(list 'if cond ift iff)
         (define-values (cond* _a) (loop cond prec))
         (define-values (ift* rtype) (loop ift prec))
         (define-values (iff* _b) (loop iff prec))
         (values (list 'if cond* ift* iff*) rtype)]
        [(list 'd term var)
         (define-values (term* rtype) (loop term prec))
         (define-values (var* _b) (loop var prec))
         (values (list 'd term* var*) rtype)]
        [(list 'subst term var value)
         (define-values (term* rtype) (loop term prec))
         (define-values (var* _b) (loop var prec))
         (define-values (value* _c) (loop value prec))
         (values (list 'subst term* var* value*) rtype)]
        [(list 'lim numerator denominator subst1 subst2 var value)
         (define-values (num* rtype) (loop numerator prec))
         (define-values (denom* _a) (loop denominator prec))
         (define-values (subst1* _b) (loop subst1 prec))
         (define-values (subst2* _c) (loop subst2 prec))
         (define-values (var* _d) (loop var prec))
         (define-values (value* _e) (loop value prec))
         (values (list 'lim num* denom* subst1* subst2* var* value*) rtype)]
        [(list 'try-/ original subst1 subst2 hist)
         (define-values (original* rtype) (loop original prec))
         (define-values (subst1* _b) (loop subst1 prec))
         (define-values (subst2* _c) (loop subst2 prec))
         (define-values (hist* _d) (loop hist prec))
         (values (list 'try-/ original* subst1* subst2* hist*) rtype)]
        [(list 'took-substitution hist var value)
         (define-values (hist* _a) (loop hist prec))
         (define-values (var* _b) (loop var prec))
         (define-values (value* rtype) (loop value prec))
         (values (list 'took-substitution hist* var* value*) rtype)]
        [(list 'took-derivative hist var)
         (define-values (hist* _a) (loop hist prec))
         (define-values (var* rtype) (loop var prec))
         (values (list 'took-derivative hist* var*) rtype)]
        [(list '! props ... body)
         (define props* (apply hash-set* (hash) props))
         (cond
           [(hash-has-key? props* ':precision)
            (define-values (body* _) (loop body (hash-ref props* ':precision)))
            (values body* prec)]
           [else (loop body prec)])]
        [(list (or 'neg '-) arg) ; unary minus
         (define-values (arg* atype) (loop arg prec))
         (define op* (get-parametric-operator '- atype))
         (values (list op* arg*) (operator-info op* 'otype))]
        [(list (? repr-conv? op) body) ; conversion (e.g. posit16->f64)
         (define-values (iprec oprec)
           (let ([split (string-split (symbol->string op) "->")])
             (values (first split) (last split))))
         (define-values (body* rtype) (loop body iprec))
         (values (list op body*) oprec)]
        [(list (and (or 're 'im) op) arg)
         ; TODO: this special case can be removed when complex-herbie is moved to a composite type
         (define-values (arg* atype) (loop arg 'complex))
         (values (list op arg*) 'binary64)]
        [(list 'complex re im)
         ; TODO: this special case can be removed when complex-herbie is moved to a composite type
         (define-values (re* re-type) (loop re 'binary64))
         (define-values (im* im-type) (loop im 'binary64))
         (values (list 'complex re* im*) 'complex)]
        [(list op args ...)
         (define-values (args* atypes)
           (for/lists (args* atypes) ([arg args])
             (loop arg prec)))
         ;; Match guaranteed to succeed because we ran type-check first
         (define op* (apply get-parametric-operator op atypes))
         (values (cons op* args*) (operator-info op* 'otype))]
        [(? real?) 
         (values
           (match expr
             [(or +inf.0 -inf.0 +nan.0) expr]
             [(? exact?) expr]
             [_ (inexact->exact expr)])
           prec)]
        [(? boolean?) (values expr 'bool)]
        [(? constant?) 
         (define prec* (if (set-member? '(TRUE FALSE) expr) 'bool prec))
         (define constant* (get-parametric-constant expr prec*))
         (values constant* (constant-info constant* 'type))]
        [(? variable?) 
         (values 
           expr 
           (if (equal? (representation-name (dict-ref var-reprs expr)) 'bool) 
               'bool prec))])))
  expr*)

;; TODO(interface): This needs to be changed once the syntax checker is updated
;; and supports multiple precisions
(define (expand-parametric-reverse expr repr full?)
  (match expr
    [(list 'if cond ift iff)
     (define cond* (expand-parametric-reverse cond repr full?))
     (define ift* (expand-parametric-reverse ift repr full?))
     (define iff* (expand-parametric-reverse iff repr full?))
     (list 'if cond* ift* iff*)]
    [(list (? repr-conv? op) body) ; conversion (e.g. posit16->f64)
     (define iprec (first (string-split (symbol->string op) "->")))
     (define repr* (get-representation (string->symbol iprec)))
     (define body* (expand-parametric-reverse body repr* full?))
     (if full? 
        `(cast (! :precision ,(string->symbol iprec) ,body*))
        `(,op ,body*))]
    [(list op args ...)
     (define op* (hash-ref parametric-operators-reverse op op))
     (define atypes
       (match (operator-info op 'itype)
         [(? representation-name? a) (map (const a) args)] ; some repr names are lists
         [(? list? as) as]))   
     (define args*
       (for/list ([arg args] [type atypes])
         (expand-parametric-reverse arg (get-representation type) full?)))
     (if (and (not full?) (equal? op* '-) (= (length args) 1))
         (cons 'neg args*) ; if only unparameterizing, leave 'neg' alone
         (cons op* args*))]
    [(? (conjoin complex? (negate real?)))
     `(complex ,(real-part expr) ,(imag-part expr))]
    [(? real?)
     (if full?
         (match expr
           [-inf.0 '(- INFINITY)] ; not '(neg INFINITY) because this is post-resugaring
           [+inf.0 'INFINITY]
           [+nan.0 'NAN]
           [x
            (if (set-member? '(binary64 binary32) (representation-name repr))
                 (exact->inexact x) ; convert to flonum if binary64 or binary32
                 x)])
         expr)]
    [(? constant?) (hash-ref parametric-constants-reverse expr expr)]
    [(? variable?) expr]))

(define (desugar-program prog repr var-reprs #:full [full? #t])
  (if full?
      (expand-parametric (expand-associativity (unfold-let prog)) repr var-reprs full?)
      (expand-parametric prog repr var-reprs full?)))

(define (resugar-program prog repr #:full [full? #t])
  (match prog
    [(list 'FPCore (list vars ...) body) `(FPCore ,vars ,(expand-parametric-reverse body repr full?))]
    [(list (or 'λ 'lambda) (list vars ...) body) `(λ ,vars ,(expand-parametric-reverse body repr full?))]
    [(? expr?) (expand-parametric-reverse prog repr full?)]))

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