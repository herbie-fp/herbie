#lang racket

(require "../errors.rkt" "types.rkt" "syntax.rkt")
(provide desugar-program resugar-program)

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
    (let loop ([expr expr] [repr repr]) ; easier to work with repr names
      ;; Run after unfold-let, so no need to track lets
      (match expr
        [(list 'if cond ift iff)
         (define-values (cond* _a) (loop cond repr))
         (define-values (ift* rtype) (loop ift repr))
         (define-values (iff* _b) (loop iff repr))
         (values (list 'if cond* ift* iff*) rtype)]
        [(list '! props ... body)
         (define props* (apply hash-set* (hash) props))
         (cond
           [(hash-has-key? props* ':precision)
            ; need to insert a cast
            (loop (list 'cast expr) repr)]
           [else (loop body repr)])]
        [(list 'cast (list '! ':precision iprec iexpr))
         (define irepr (get-representation iprec))
         (cond
          [(equal? repr irepr)  ; ignore
           (loop iexpr repr)]
          [else
           (define conv (get-repr-conv irepr repr))
           (define-values (iexpr* _) (loop iexpr irepr))
           (values (list conv iexpr*) repr)])]
        [(list 'cast body)    ; no-op cast (ignore)
         (loop body repr)]
        [(list (or 'neg '-) arg) ; unary minus
         (define-values (arg* atype) (loop arg repr))
         (define op* (get-parametric-operator 'neg atype))
         (values (list op* arg*) (operator-info op* 'otype))]
        [(list (? repr-conv? op) body) ; conversion (e.g. posit16->f64)
         (define irepr (first (operator-info op 'itype)))
         (define orepr (operator-info op 'otype))
         (define-values (body* rtype) (loop body irepr))
         (values (list op body*) orepr)]
        [(or (? constant-operator? x) (list x)) ; constant
         (let/ec k
           (for/list ([name (operator-all-impls x)])
             (define rtype (operator-info name 'otype))
             (when (or (equal? rtype repr) (equal? (representation-type rtype) 'bool))
               (k (list name) rtype)))
           (raise-herbie-missing-error "Could not find constant implementation for ~a at ~a"
                                        x (representation-name repr)))]
        [(list op args ...)
         (define-values (args* atypes)
           (for/lists (args* atypes) ([arg args])
             (loop arg repr)))
         ;; Match guaranteed to succeed because we ran type-check first
         (define op* (apply get-parametric-operator op atypes))
         (values (cons op* args*) (operator-info op* 'otype))]
        [(? number?) 
         (values
           (match expr
             [(or +inf.0 -inf.0 +nan.0) expr]
             [(? exact?) expr]
             [_ (inexact->exact expr)])
           repr)]
        [(? boolean?) (values expr (get-representation 'bool))]
        [(? variable?)
         (define vrepr (dict-ref var-reprs expr))
         (cond
          [(equal? (representation-type vrepr) 'bool) (values expr vrepr)]
          [(equal? vrepr repr) (values expr repr)]
          [else
           (define conv (get-repr-conv vrepr repr))
           (unless conv
             (raise-herbie-missing-error "Conversion does not exist: ~a -> ~a"
                (representation-name vrepr) (representation-name repr)))
           (values (list conv expr) repr)])]))) 
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
     (define repr* (first (operator-info op 'itype)))
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
       (for/list ([arg args] [repr (operator-info op 'itype)])
         (expand-parametric-reverse arg repr full?)))
     (if (and full? (equal? op* 'neg) (= (length args) 1)) ; if only unparameterizing, leave 'neg' alone
         (cons '- args*)
         (cons op* args*))]
    [(? number?)
     (if full?
         (match expr
           [-inf.0 (if full? '(- INFINITY) '(neg INFINITY))] ; not '(neg INFINITY) because this is post-resugaring
           [+inf.0 'INFINITY]
           [+nan.0 'NAN]
           [x
            ;; TODO: Why is this here?
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
