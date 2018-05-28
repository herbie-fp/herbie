#lang racket

(require "fpcore-common.rkt" math/flonum math/bigfloat math/special-functions math/base)
(provide
 (struct-out evaluator) racket-double-evaluator racket-single-evaluator
 fpcore? expr? context/c eval-expr* eval-expr racket-run-fpcore
 read-fpcore)

(struct evaluator (real constant function))

(define/contract (fpcore? thing)
  contract?
  (match thing
    [`(FPCore (,(? symbol?) ...) ,props ... ,(? expr?))
     (define-values (rest props*) (parse-properties props))
     (null? rest)]
    [_ false]))

(define-by-match expr?
  (? number?)
  (? constant?)
  (? symbol?)
  (list (? operator?) (? expr?) ...)
  `(if ,(? expr?) ,(? expr?) ,(? expr?))
  `(let ([,(? symbol?) ,(? expr?)] ...) ,(? expr?))
  `(while ,(? expr?) ([,(? symbol?) ,(? expr?) ,(? expr?)] ...) ,(? expr?)))

(define type? (symbols 'boolean 'real))

(define/match (operator-type op args)
  [((or '- 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt
        'cbrt 'sin 'cos 'tan 'asin 'acos 'atan 'sinh 'cosh 'tanh
        'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 'ceil 'floor
        'trunc 'round 'nearbyint)
    (list 'real))
   'real]
  [((or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
    (list 'real 'real))
   'real]
  [('fma (list 'real 'real 'real)) 'real]
  [((or '< '> '<= '>= '== '!=) (list 'real ...)) 'boolean]
  [((or 'isfinite 'isinf 'isnan 'isnormal 'signbit) 'real) 'boolean]
  [((or 'and 'or) (list 'boolean ...)) 'boolean]
  [('not (list 'boolean)) 'boolean] 
  [(_ _) #f])

(define/contract (check-expr stx ctx)
  (-> syntax? (dictof symbol? type?) (cons/c expr? type?))

  (match (syntax-e stx)
    [(? number? val)
     (cons val 'real)]
    [(? constant? val)
     (cons val (match val [(or 'TRUE 'FALSE) 'boolean] [_ 'real]))]
    [(? symbol? var)
     (unless (dict-has-key? ctx var)
       (raise-syntax-error #f "Undefined variable" stx))
     (cons var (dict-ref ctx var))]
    [(list (app syntax-e 'if) test ift iff)
     (define test* (check-expr test ctx))
     (unless (equal? (cdr test*) 'boolean)
       (raise-syntax-error #f "Conditional test must return a boolean" stx test))
     (define ift* (check-expr ift ctx))
     (define iff* (check-expr iff ctx))
     (unless (equal? (cdr ift*) (cdr iff*))
       (raise-syntax-error #f "Conditional branches must have same type" stx))
     (cons `(if ,(car test*) ,(car ift*) ,(car iff*)) (cdr ift*))]
    [(cons (app syntax-e 'if) _)
     (raise-syntax-error #f "Invalid conditional statement" stx)]
    [(list (app syntax-e 'let) (app syntax-e (list (app syntax-e (list vars vals)) ...)) body)
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by let binding" stx var))
         (syntax-e var)))
     (define vals* (map (curryr check-expr ctx) vals))
     (define ctx* (apply dict-set* ctx (append-map list vars* (map cdr vals*))))
     (define body* (check-expr body ctx*))
     (cons `(let (,@(map list vars* (map car vals*))) ,(car body*)) (cdr body*))]
    [(cons (app syntax-e 'let) _)
     (raise-syntax-error #f "Invalid let bindings" stx)]
    [(list (app syntax-e 'while) test (app syntax-e (list (app syntax-e (list vars inits updates)) ...)) body)
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by while loop" stx var))
         (syntax-e var)))
     (define inits* (map (curryr check-expr ctx) inits))
     (define ctx* (apply dict-set* ctx (append-map list vars* (map cdr inits*))))
     (define test* (check-expr test ctx*))
     (unless (equal? (cdr test*) 'boolean)
       (raise-syntax-error #f "While loop conditions must return a boolean" test))
     (define updates* (map (curryr check-expr ctx*) updates))
     (for ([var vars] [init inits*] [update updates*])
       (unless (equal? (cdr init) (cdr update))
         (raise-syntax-error #f "Initialization and update must have the same type in while loop" stx var)))
     (define body* (check-expr body ctx*))
     (cons `(while ,(car test*) (,@(map list vars* (map car inits*) (map car updates*))) ,(car body*)) (cdr body*))]
    [(cons (app syntax-e 'while) _)
     (raise-syntax-error #f "Invalid while loop" stx)]
    [(list op args ...)
     (unless (set-member? operators (syntax-e op))
       (raise-syntax-error #f "Unknown operator" op))
     (define children (map (curryr check-expr ctx) args))
     (define rtype (operator-type (syntax-e op) (map cdr children)))
     (unless rtype
       (raise-syntax-error #f (format "Invalid types for operator ~a" op) stx))
     (cons (list* (syntax-e op) (map car children)) rtype)]))

(define/contract (check-fpcore stx)
  (-> syntax? fpcore?)
  (match (syntax-e stx)
    [(list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) properties ... body)
     (define ctx
       (for/hash ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "FPCore parameters must be variables" stx var))
         (values (syntax-e var) 'real)))

     (define properties*
       (let loop ([properties properties])
         (match properties
           [(list) (list)]
           [(list prop) (raise-syntax-error #f "Property with no value" prop)]
           [(list (app syntax-e (? property? prop)) value rest ...)
            (cons (cons prop value) (loop rest))]
           [(list prop _ ...) (raise-syntax-error #f "Invalid property" prop)])))

     (when (dict-has-key? properties* ':pre)
       (define pre (dict-ref properties* ':pre))
       (define pre* (check-expr pre ctx))
       (unless (equal? (cdr pre*) 'boolean)
         (raise-syntax-error #f "FPCore precondition must return a boolean" pre)))

     (define body* (check-expr body ctx))
     (unless (equal? (cdr body*) 'real)
       (raise-syntax-error #f "FPCore benchmark must return a real number" body))
     
     `(FPCore (,@(dict-keys ctx))
              ,@(apply append
                       (for/list ([(prop val) (in-dict properties*)])
                         (list prop (syntax->datum val))))
              ,(car body*))]))

(define/contract (read-fpcore name p)
  (-> any/c input-port? (or/c fpcore? eof-object?))
  (parameterize ([read-decimal-as-inexact #f])
    (define stx (read-syntax name p))
    (if (eof-object? stx) stx (check-fpcore stx))))

(define/contract context/c contract? (dictof symbol? any/c))

(define/contract ((eval-expr* evaltor rec) expr ctx)
  (-> evaluator? (-> expr? context/c any/c) (-> expr? context/c any/c))
  (match expr
    [(? number?) ((evaluator-real evaltor) expr)]
    [(? constant?) ((evaluator-constant evaltor) expr)]
    [(? symbol?) (dict-ref ctx expr)]
    [`(if ,test ,ift ,iff)
     (if (rec test ctx) (rec ift ctx) (rec iff ctx))]
    [`(let ([,vars ,vals] ...) ,body)
     (define vals* (for/list ([val vals]) (rec val ctx)))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (rec body ctx*)]
    [`(while ,test ([,vars ,inits ,updates] ...) ,res)
     (define vals* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (if (rec test ctx*)
         (let ([inits* (for/list ([update updates]) (rec update ctx*))])
           (rec
            `(while ,test
               ,(for/list ([var vars] [init inits] [update updates])
                  (list var (rec update ctx*) update))
               ,res)
            ctx))
         (rec res ctx*))]
    [(list (? operator? op) args ...)
     (apply ((evaluator-function evaltor) op)
            (map (curryr rec ctx) args))]))

(define/contract ((eval-expr evaltor) expr ctx)
  (-> evaluator? (-> expr? context/c any/c))
  (let eval ([expr expr] [ctx ctx])
    ((eval-expr* evaltor eval) expr ctx)))

(define-syntax-rule (table-fn [var val] ...)
  (match-lambda
   [`var val] ...
   [unsupported-value
    (error 'eval-expr "Unimplemented operation ~a"
           unsupported-value)]))

(define (my!= #:cmp [cmp =] . args) (not (check-duplicates args cmp)))
(define (my= #:cmp [cmp =] . args)
  (match args ['() true] [(cons hd tl) (andmap (curry cmp hd) tl)]))

(define/contract racket-double-evaluator evaluator?
  (evaluator
   real->double-flonum
   (table-fn
    [E		2.71828182845904523540]
    [LOG2E	1.44269504088896340740]
    [LOG10E	0.43429448190325182765]
    [LN2	0.69314718055994530942]
    [LN10	2.30258509299404568402]
    [PI		3.14159265358979323846]
    [PI_2	1.57079632679489661923]
    [PI_4	0.78539816339744830962]
    [1_PI	0.31830988618379067154]
    [2_PI	0.63661977236758134308]
    [2_SQRTPI	1.12837916709551257390]
    [SQRT2	1.41421356237309504880]
    [SQRT1_2	0.70710678118654752440]
    [NAN	+nan.0]
    [INFINITY	+inf.0]
    [TRUE #t] [FALSE #f])
   (table-fn
    [+ +] [- -] [* *] [/ /] [fabs abs]
    [exp exp] [exp2 (λ (x) (expt 2.0 x))]
    [log log] [log10 (λ (x) (/ (log x) (log 10.0)))]
    [log2 (λ (x) (/ (log x) (log 2.0)))]
    [pow expt] [sqrt sqrt]
    [hypot flhypot] [sin sin] [cos cos] [tan tan] [asin asin]
    [acos acos] [atan atan] [atan2 atan] [sinh sinh] [cosh cosh]
    [tanh tanh] [asinh asinh] [acosh acosh] [atanh atanh]
    [erf erf] [erfc erfc] [tgamma gamma] [lgamma log-gamma]
    [ceil ceiling] [floor floor] [trunc truncate] [round round]
    [fmax max] [fmin min]
    [fdim (λ (x y) (abs (- x y)))]
    [< <] [> >] [<= <=] [>= >=] [== my=] [!= my!=]
    [and (λ (x y) (and x y))] [or (λ (x y) (or x y))] [not not]
    [isnan nan?] [isinf infinite?]
    [isfinite (λ (x) (not (or (nan? x) (infinite? x))))]
    ; TODO: Currently unsupported
    ;[fma '?] [expm1 '?] [log1p '?] [isnormal '?] [signbit '?]
    ;[fmod '?] [remainder '?] [copysign '?] [nearbyint '?]
    )))

(define/contract racket-single-evaluator evaluator?
  (struct-copy evaluator racket-double-evaluator
               [real real->single-flonum]
               [constant (λ (x) (real->single-flonum ((evaluator-constant racket-double-evaluator) x)))]))

(define/contract (racket-run-fpcore prog vals)
  (-> fpcore? (listof real?) real?)
  (match-define `(FPCore (,vars ...) ,props* ... ,body) prog)
  (define-values (_ props) (parse-properties props*))
  (define evaltor
    (match (dict-ref props ':precision 'binary64)
      ['binary64 racket-double-evaluator]
      ['binary32 racket-single-evaluator]))
  ((eval-expr evaltor) body (map cons vars (map (evaluator-real evaltor) vals))))

(module+ main
  (command-line
   #:program "fpcore.rkt"
   #:args args
   (port-count-lines! (current-input-port))
   (let ([vals (map (compose real->double-flonum string->number) args)])
     (for ([prog (in-port (curry read-fpcore "stdin"))])
       (printf "~a\n" (racket-run-fpcore prog vals))))))
