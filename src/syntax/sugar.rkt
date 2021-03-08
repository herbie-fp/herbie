#lang racket

(require "types.rkt" "syntax.rkt" "../interface.rkt")
(provide desugar-program resugar-program)

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
        [(list '! props ... body)
         (define props* (apply hash-set* (hash) props))
         (cond
           [(hash-has-key? props* ':precision)
            ; need to insert a cast
            (loop (cons 'cast expr) prec)]
           [else (loop body prec)])]
        [(list 'cast (list '! ':precision iprec iexpr))
         (define conv (get-repr-conv iprec prec))
         (define-values (iexpr* _) (loop iexpr iprec))
         (values (list conv iexpr*) prec)]
        [(list (or 'neg '-) arg) ; unary minus
         (define-values (arg* atype) (loop arg prec))
         (define op* (get-parametric-operator '- atype))
         (values (list op* arg*) (operator-info op* 'otype))]
        [(list (? repr-conv? op) body) ; conversion (e.g. posit16->f64)
         (define iprec (first (operator-info op 'itype)))
         (define oprec (operator-info op 'otype))
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
         (define vprec (representation-name (dict-ref var-reprs expr)))
         (cond
          [(equal? vprec 'bool) (values expr 'bool)]
          [(equal? vprec prec) (values expr prec)]
          [else
           (define conv (get-repr-conv vprec prec))
           (values (list conv expr) prec)])])))
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
     (define iprec (first (operator-info op 'itype)))
     (define oprec (operator-info op 'otype))
     (define repr* (get-representation iprec))
     (define body* (expand-parametric-reverse body repr* full?))
     (cond
      [(not full?) `(,op ,body*)]
      [(list? body*) `(cast (! :precision ,iprec ,body*))]
      [else body*])] ; constants and variables should not have casts and precision changes
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
    [_ (expand-parametric-reverse prog repr full?)]))

(define (replace-vars dict expr)
  (cond
    [(dict-has-key? dict expr) (dict-ref dict expr)]
    [(list? expr)
     (cons (replace-vars dict (car expr)) (map (curry replace-vars dict) (cdr expr)))]
    [#t expr]))

