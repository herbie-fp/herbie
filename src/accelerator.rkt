#lang racket

(require "common.rkt"
         "compiler.rkt"
         "ground-truth.rkt"
         (submod "syntax/syntax.rkt" internals)
         (submod "syntax/rules.rkt" internals)
         "syntax/types.rkt")

(provide accelerator?
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

;;
;;  Accelerator operators
;;

;; An accelerator operator
;; Each operator is just a composition of existing operators
;;  `itypes`: input type of accelerator
;;  `otype`: output type of accelerator
;;  `spec`: definition as an S-expr
;;  `apply`: procedure that matches on its definition and replaces it
;;  `undo`: procedure that replaces an accelerator with its definition
(struct accelerator-operator (itypes otype spec apply undo))

;; Accelerators known to Herbie at runtime
(define accelerator-operators (make-hasheq))

;; Is the operator an accelerator?
(define (accelerator? x)
  (hash-has-key? accelerator-operators x))

;; Adds an accelerator to the table.
(define (register-accelerator-operator! name itypes otype spec)
  (match-define (list (or 'λ 'lambda) (list vars ...) body) spec)
  (for/fold ([ids '()]) ([var (in-list vars)])
    (if (member var ids)
        (error 'register-accelerator-operator!
               "duplicate variable for ~a: ~a"
               name var)
        (cons var ids))) 
  (unless (= (length vars) (length itypes))
    (error 'register-accelerator-operator!
           "implementation does not have expected arity: ~a ~a"
           (length vars)
           (length itypes)))
  (define ruleset-name (sym-append name '- 'accelerator))
  (define define-name (sym-append name '- 'define))
  (define undefine-name (sym-append name '- 'undefine))
  (define ival-fn (compile-spec body vars))
  (hash-set! accelerator-operators name
             (accelerator-operator itypes otype spec #f #f))
  (register-operator! name itypes otype (list (cons 'ival ival-fn)))
  (register-ruleset*! ruleset-name
                      (list 'numerics 'simplify)
                      (map cons vars itypes)
                      (list (list define-name body (cons name vars))
                            (list undefine-name (cons name vars) body))))

(define-syntax define-accelerator-operator
  (syntax-rules ()
    [(_ name (itypes ...) otype impl)
     (register-accelerator-operator! 'name
                                     (list 'itypes ...)
                                     'otype
                                     'impl)]))

(define (register-accelerator-impl! operator name
                                    itypes otype
                                    [implementation #f])
  (match-define (accelerator-operator _ _ spec _ _)
    (dict-ref accelerator-operators operator))
  (match-define (list (or 'λ 'lambda) (list vars ...) body) spec)
  (define ctx (context vars otype itypes))
  (define impl-fn
    (or implementation
        (let ([fn (eval-progs-real (list body) ctx)])
          (λ args (first (apply fn args))))))
  (register-operator-impl! operator
                           name
                           itypes
                           otype
                           (list (cons 'fl impl-fn))))

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

(define (expand-accelerators expr
                             #:accelerators [accelerators (dict-keys accelerator-operators)])
  (void))

; (define (expand-accelerators rules expression)
;   (define undefine-rules
;     (filter
;      (compose
;       (curry set-member? (map (curryr sym-append '- 'undefine) (dict-keys accelerator-operators)))
;       rule-name)
;      rules))
;   ;; Apply the first rule that matches top down. We do this because we can only
;   ;; be sure we have a real match if the term does not occur in the syntactic
;   ;; scope of any other syntactic extensions.
;   ;;
;   ;; See https://dl.acm.org/doi/10.1145/319838.319859
;   (let rewrite ([expression* expression])
;     (match (or
;             (let ([expression** (ormap (curryr rule-apply expression*) undefine-rules)])
;               (and expression** (car expression**)))
;             expression*)
;       [(list operator operands ...) (cons operator (map rewrite operands))]
;       [_ expression*])))

; ;; TODO: Temporarily copied to avoid cycles. Is there a way to avoid this?

; (define (merge-bindings binding1 binding2)
;   (and binding1
;        binding2
;        (let/ec quit
;          (for/fold ([binding binding1]) ([(k v) (in-dict binding2)])
;            (dict-update binding k (λ (x) (if (equal? x v) v (quit #f))) v)))))

; (define (pattern-match pattern expr)
;   (match pattern
;    [(? number?)
;     (and (equal? pattern expr) '())]
;    [(? variable?)
;     (list (cons pattern expr))]
;    [(list phead _ ...)
;     (and (list? expr)
;          (equal? (car expr) phead)
;          (= (length expr) (length pattern))
;          (for/fold ([bindings '()])
;              ([pat (cdr pattern)] [subterm (cdr expr)])
;            (merge-bindings bindings (pattern-match pat subterm))))]))

; (define (pattern-substitute pattern bindings)
;   ; pattern binding -> expr
;   (match pattern
;    [(? number?) pattern]
;    [(? variable?)
;     (dict-ref bindings pattern)]
;    [(list phead pargs ...)
;     (cons phead (map (curryr pattern-substitute bindings) pargs))]))

; (define (rule-apply rule expr)
;   (let ([bindings (pattern-match (rule-input rule) expr)])
;     (if bindings
;         (cons (pattern-substitute (rule-output rule) bindings) bindings)
;         #f)))

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
