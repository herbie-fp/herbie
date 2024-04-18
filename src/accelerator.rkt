#lang racket

(require "common.rkt"
         "compiler.rkt"
         "ground-truth.rkt"
         "core/matcher.rkt"
         "syntax/types.rkt"
         (submod "syntax/syntax.rkt" internals)
         (submod "syntax/rules.rkt" internals))

(provide accelerator?
         accelerator-info
         register-accelerator-operator!
         register-accelerator-impl!
         define-accelerator-operator
         define-accelerator-impl
         expand-accelerators)

(module+ internals
  (provide register-accelerator-operator!
           register-accelerator-impl!
           define-accelerator-operator
           define-accelerator-impl))

(struct accelerator-operator (body vars itypes otype))

(define accelerator-operators (make-hasheq))

(define (accelerator? op)
  (hash-has-key? accelerator-operators op))

(define/contract (accelerator-info op field)
  (-> symbol? (or/c 'itype 'otype 'body 'vars) any/c)
  (unless (hash-has-key? accelerator-operators op)
    (error 'accelerator-info "Unknown accelerator ~a" op))
  (define accessor
    (match field
      ['itype accelerator-operator-itypes]
      ['otype accelerator-operator-otype]
      ['body accelerator-operator-body]
      ['vars accelerator-operator-vars]))
  (accessor (hash-ref accelerator-operators op)))

(define (register-accelerator-operator! name itypes otype form [ival-impl #f])
  (match-define (list 'lambda (list vars ...) body) form)
  (define ruleset-name (sym-append name '- 'accelerator))
  (define define-name (sym-append name '- 'define))
  (define undefine-name (sym-append name '- 'undefine))
  (define info (accelerator-operator body vars itypes otype))
  (define impl-fn (if ival-impl ival-impl (compile-spec body vars)))
  (hash-set! accelerator-operators name info)
  (register-operator! name itypes otype (list (cons 'ival impl-fn)))
  ;; TODO: Are the groups right now?
  (register-ruleset*! ruleset-name
                     '(numerics simplify)
                     (map cons vars itypes)
                     `((,define-name ,body (,name ,@vars))
                       (,undefine-name (,name ,@vars) ,body))))

(define-syntax define-accelerator-operator
  (syntax-rules ()
    [(_ name (itypes ...) otype (lambda (variables ...) body))
     (register-accelerator-operator! 'name
                                     (list 'itypes ...) 'otype
                                     (list 'lambda (list 'variables ...) 'body))]
    [(_ name (itypes ...) otype (lambda (variables ...) body) ival-impl)
     (register-accelerator-operator! 'name
                                     (list 'itypes ...) 'otype
                                     (list 'lambda (list 'variables ...) 'body) ival-impl)]))

(define (register-accelerator-impl! operator name
                                    itypes otype
                                    [implementation #f])
  (match-define (accelerator-operator body vars _ _) (dict-ref accelerator-operators operator))
  (register-operator-impl!
   operator name
   itypes otype
   (list
    (cons 'fl
          (or 
           implementation
           (compose first (eval-progs-real 
                           (list body)
                           (list (context vars otype itypes)))))))))

(define-syntax define-accelerator-impl
  (syntax-rules ()
    [(_ operator name (itypes ...) otype)
     (register-accelerator-impl! 'operator 'name
                                 (list (get-representation 'itypes) ...)
                                 (get-representation 'otype))]
    [(_ operator name (itypes ...) otype implementation)
     (register-accelerator-impl! 'operator 'name
                                 (list (get-representation 'itypes) ...)
                                 (get-representation 'otype)
                                 implementation)]))

(define (expand-accelerators rules expression)
  (define undefine-rules
    (filter
     (compose
      (curry set-member? (map (curryr sym-append '- 'undefine) (dict-keys accelerator-operators)))
      rule-name)
     rules))
  ;; Apply the first rule that matches top down. We do this because we can only
  ;; be sure we have a real match if the term does not occur in the syntactic
  ;; scope of any other syntactic extensions.
  ;;
  ;; See https://dl.acm.org/doi/10.1145/319838.319859
  (let rewrite ([expression* expression])
    (match (or
            (let ([expression** (ormap (curryr rule-apply expression*) undefine-rules)])
              (and expression** (car expression**)))
            expression*)
      [(list operator operands ...) (cons operator (map rewrite operands))]
      [_ expression*])))

(define-accelerator-operator expm1 (real) real (lambda (x) (- (exp x) 1)))
(define-accelerator-operator log1p (real) real (lambda (x) (log (+ 1 x))))
(define-accelerator-operator hypot (real real) real (lambda (x y) (sqrt (+ (* x x) (* y y)))))
(define-accelerator-operator fma (real real real) real (lambda (x y z) (+ (* x y) z)))
(define-accelerator-operator erfc (real) real (lambda (x) (- 1 (erf x))))

; Specialized numerical functions
(define-ruleset* special-numerical-reduce (numerics simplify)
  #:type ([x real] [y real] [z real])
  ;; TODO: Remove these comments entirely, keeping them for now to make it
  ;; easier to track what's been deleted without having to tab back and forth.
  ;;
  ;; [expm1-def   (- (exp x) 1)              (expm1 x)]
  ;; [log1p-def   (log (+ 1 x))              (log1p x)]
  [log1p-expm1 (log1p (expm1 x))          x]
  ;; [expm1-log1p (expm1 (log1p x))          x]
  ;; [hypot-def   (sqrt (+ (* x x) (* y y))) (hypot x y)]
  [hypot-1-def (sqrt (+ 1 (* y y)))       (hypot 1 y)]
  ;; [fma-def     (+ (* x y) z)              (fma x y z)]
  [fma-neg     (- (* x y) z)              (fma x y (neg z))])
  ;; [fma-udef    (fma x y z)                (+ (* x y) z)])

(define-ruleset* special-numerical-expand (numerics)
  #:type ([x real] [y real])
  ;; [expm1-udef    (expm1 x)      (- (exp x) 1)]
  ;; [log1p-udef    (log1p x)      (log (+ 1 x))]
  [log1p-expm1-u x              (log1p (expm1 x))]
  [expm1-log1p-u x              (expm1 (log1p x))])
  ;; [hypot-udef    (hypot x y)    (sqrt (+ (* x x) (* y y)))])

(define-ruleset* erf-rules (special simplify)
  #:type ([x real])
  [erf-odd          (erf (neg x))        (neg (erf x))])
  ;; [erf-erfc         (erfc x)             (- 1 (erf x))]
  ;; [erfc-erf         (erf x)              (- 1 (erfc x))])

(define-ruleset* numerics-papers (numerics)
  #:type ([a real] [b real] [c real] [d real])
  ;  "Further Analysis of Kahan's Algorithm for
  ;   the Accurate Computation of 2x2 Determinants"
  ;  Jeannerod et al., Mathematics of Computation, 2013
  ;
  ;  a * b - c * d  ===> fma(a, b, -(d * c)) + fma(-d, c, d * c)
  [prod-diff    (- (* a b) (* c d))
                (+ (fma a b (neg (* d c)))
                   (fma (neg d) c (* d c)))])
