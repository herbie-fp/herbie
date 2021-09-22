#lang racket

(require "types.rkt" "syntax.rkt" "../interface.rkt")
(provide desugar-program resugar-program)
(module+ test (require rackunit))

;; preprocessing
(define (expand expr)
  (match expr
   ;; Constants are zero-ary functions
   [(? constant-operator?) (list expr)]
   ;; unfold let
   [(list let* (list (list var val) rest ...) body)
    (replace-vars (list (cons var (expand val))) (expand `(let* ,rest ,body)))]
   [(list 'let (list (list vars vals) ...) body)
    (replace-vars (map cons vars (map expand vals)) (expand body))]
   [(list (or 'let 'let*) (list) body)
    (expand body)]
   ;; expand arithmetic associativity
   [(list (and (or '+ '- '* '/ 'and 'or) op) a ..2 b)
     (list op (expand (cons op a)) (expand b))]
   [(list (or '+ '* 'and 'or) a) (expand a)]
   [(list '- a) (list '- (expand a))]
   [(list '/ a) (list '/ 1 (expand a))]
   [(list (or '+ '-)) 0]
   [(list (or '* '/)) 1]
   ['(and) 'TRUE]
   ['(or) 'FALSE]
   ;; expand comparison associativity
   [(list (and (or '< '<= '> '>= '=) op) as ...)
    (define as* (map expand as))
    (define out
      (for/fold ([out #f]) ([term as*] [next (cdr as*)])
        (if out
            (list 'and out (list op term next))
            (list op term next))))
    (or out 'TRUE)]
   [(list '!= as ...)
    (define as* (map expand as))
    (define out
      (for/fold ([out #f])
          ([term as*] [i (in-naturals)]
           #:when true
           [term2 as*] [j (in-naturals)]
           #:when (< i j))
        (if out
            (list 'and out (list '!= term term2))
            (list '!= term term2))))
    (or out 'TRUE)]
   [(list (or 'and 'or) a) (expand a)]
   ['(and) 'TRUE]
   ['(or) 'FALSE]
   ;; inline functions
   [(list (? (curry hash-has-key? (*functions*)) fname) args ...)
    (match-define (list vars _ body) (hash-ref (*functions*) fname))
    (replace-vars (map cons vars args) (expand body))]
   ;; unchanged
   [(list op args ...) (cons op (map expand args))]
   [_ expr]))

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
            (loop (list 'cast expr) prec)]
           [else (loop body prec)])]
        [(list 'cast (list '! ':precision iprec iexpr))
         (cond
          [(equal? prec iprec)  ; ignore
           (loop iexpr prec)]
          [else
           (define conv (get-repr-conv iprec prec))
           (define-values (iexpr* _) (loop iexpr iprec))
           (values (list conv iexpr*) prec)])]
        [(list 'cast body)    ; no-op cast (ignore)
         (loop body prec)]
        [(list (or 'neg '-) arg) ; unary minus
         (define-values (arg* atype) (loop arg prec))
         (define op* (get-parametric-operator 'neg atype))
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
        [(or (? constant-operator? x) (list x)) ; constant
         (let/ec k
           (for/list ([name (operator-all-impls x)])
             (define rtype (operator-info name 'otype))
             (when (or (equal? rtype prec) (equal? rtype 'bool))
               (k (list name) rtype)))
           (error 'sugar "Could not find constant implementation for ~a at ~a" x prec))]
        [(list op args ...)
         (define-values (args* atypes)
           (for/lists (args* atypes) ([arg args])
             (loop arg prec)))
         ;; Match guaranteed to succeed because we ran type-check first
         (define op* (apply get-parametric-operator op atypes))
         (values (cons op* args*) (operator-info op* 'otype))]
        [(? number?) 
         (values
           (match expr
             [(or +inf.0 -inf.0 +nan.0) expr]
             [(? exact?) expr]
             [_ (inexact->exact expr)])
           prec)]
        [(? boolean?) (values expr 'bool)]
        [(? variable?)
         (define vprec (representation-name (dict-ref var-reprs expr)))
         (cond
          [(equal? vprec 'bool) (values expr 'bool)]
          [(equal? vprec prec) (values expr prec)]
          [else
           (define conv (get-repr-conv vprec prec))
           (unless conv (error 'expand-parametric "Conversion does not exist: ~a -> ~a\n" vprec prec))
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
     (define repr* (get-representation (first (operator-info op 'itype))))
     (define body* (expand-parametric-reverse body repr* full?))
     (cond
      [(not full?) `(,op ,body*)]
      [(list? body*) `(cast (! :precision ,(representation-name repr*) ,body*))]
      [else body*])] ; constants and variables should not have casts and precision changes
    [(list op)
     (define op* (impl->operator op))
     (if full? op* (list op*))]
    [(list op args ...)
     (define op* (impl->operator op))
     (define args*
       (for/list ([arg args] [repr (map get-representation (operator-info op 'itype))])
         (expand-parametric-reverse arg repr full?)))
     (if (and full? (equal? op* 'neg) (= (length args) 1)) ; if only unparameterizing, leave 'neg' alone
         (cons '- args*)
         (cons op* args*))]
    [(? (conjoin complex? (negate real?)))
     `(complex ,(real-part expr) ,(imag-part expr))]
    [(? number?)
     (if full?
         (match expr
           [-inf.0 (if full? '(- INFINITY) '(neg INFINITY))] ; not '(neg INFINITY) because this is post-resugaring
           [+inf.0 'INFINITY]
           [+nan.0 'NAN]
           [x
            (if (set-member? '(binary64 binary32) (representation-name repr))
                 (exact->inexact x) ; convert to flonum if binary64 or binary32
                 x)])
         expr)]
    [(? variable?) expr]))

(define (desugar-program prog repr var-reprs #:full [full? #t])
  (if full?
      (expand-parametric (expand prog) repr var-reprs full?)
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

#;(module+ test
  (define repr (get-representation 'binary64))

  ;; inlining

  ;; Test classic quadp and quadm examples
  (register-function! 'discr (list 'a 'b 'c) repr `(sqrt (- (* b b) (* 4 a c))))
  (define quadp `(/ (+ (- y) (discr x y z)) (* 2 x)))
  (define quadm `(/ (- (- y) (discr x y z)) (* 2 x)))
  (check-equal? (desugar-program quadp repr (map (curryr cons repr) (list 'x 'y 'z)))
                '(/.f64 (+.f64 (neg.f64 y) (sqrt.f64 (-.f64 (*.f64 y y) (*.f64 (*.f64 4 x) z)))) (*.f64 2 x)))
  (check-equal? (desugar-program quadm repr (map (curryr cons repr) (list 'x 'y 'z)))
                '(/.f64 (-.f64 (neg.f64 y) (sqrt.f64 (-.f64 (*.f64 y y) (*.f64 (*.f64 4 x) z)))) (*.f64 2 x)))

  ;; x^5 = x^3 * x^2
  (register-function! 'sqr (list 'x) repr '(* x x))
  (register-function! 'cube (list 'x) repr '(* x x x))
  (define fifth '(* (cube a) (sqr a)))
  (check-equal? (desugar-program fifth repr (list (cons 'a repr)))
                '(*.f64 (*.f64 (*.f64 a a) a) (*.f64 a a)))

  ;; casting edge cases
  (check-equal? (desugar-program `(cast x) repr `((x . ,repr)))
                'x)
  (check-equal? (desugar-program `(cast (! :precision binary64 x)) repr `((x . ,repr)))
                'x)
)
