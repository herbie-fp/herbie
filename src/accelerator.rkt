#lang racket

(require "common.rkt"
         "compiler.rkt"
         "ground-truth.rkt"
         (submod "syntax/syntax.rkt" internals)
         (submod "syntax/rules.rkt" internals)
         "syntax/types.rkt"
         "core/matcher.rkt")

(provide accelerator?
         accelerators
         expand-accelerators)

(module+ internals
  (provide register-accelerator-operator!
           register-accelerator-impl!
           define-accelerator
           define-accelerator-impl))

;;
;;  Accelerator operators
;;

;; An accelerator operator
;; Each operator is just a composition of existing operators
;;  `itypes`: input type of accelerator
;;  `otype`: output type of accelerator
;;  `spec`: definition as an S-expr
;;  `lhs`: term representing the accelerator
;;  `rhs`: term representing the definition of an accelerator
(struct accelerator-operator (itypes otype spec lhs rhs))

;; Accelerators known to Herbie at runtime
(define accelerator-operators (make-hasheq))

;; Is the operator an accelerator?
(define (accelerator? x)
  (hash-has-key? accelerator-operators x))

;; The list of accelerators as a list.
(define (accelerators)
  (hash-keys accelerator-operators))

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
  (define info (accelerator-operator itypes otype spec `(,name ,@vars) body))
  (hash-set! accelerator-operators name info)
  (register-operator! name itypes otype (list (cons 'ival ival-fn)))
  (register-ruleset*! ruleset-name
                      (list 'numerics 'simplify)
                      (map cons vars itypes)
                      (list (list define-name body (cons name vars))
                            (list undefine-name (cons name vars) body))))

(define-syntax define-accelerator
  (syntax-rules ()
    [(_ (name itypes ...) otype impl)
     (register-accelerator-operator! 'name
                                     (list 'itypes ...)
                                     'otype
                                     'impl)]))

(define (register-accelerator-impl! accelerator
                                    name
                                    itypes
                                    otype
                                    #:impl [impl #f])
  (unless (accelerator? accelerator)
    (error 'register-accelerator-impl "must be an accelerator ~a" accelerator))
  (match-define (accelerator-operator _ _ spec _ _)
    (hash-ref accelerator-operators accelerator))
  (match-define (list (or 'λ 'lambda) (list vars ...) body) spec)
  (define ctx (context vars otype itypes))
  (define impl-fn
    (or impl
        (let ([fn (eval-progs-real (list body) ctx)])
          (λ args (first (apply fn args))))))
  (register-operator-impl! accelerator
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
                                 #:impl implementation)]))

(define (expand-accelerators expr #:accelerators [accelerators (accelerators)])
  (define (expand expr)
    (for/fold ([expr expr]) ([accel (in-list accelerators)])
      (define info (hash-ref accelerator-operators accel))
      (define lhs (accelerator-operator-lhs info))
      (define rhs (accelerator-operator-rhs info))
      (define bindings (pattern-match lhs expr))
      (if bindings (pattern-substitute rhs bindings) expr)))
  (let loop ([expr expr])
    (match (expand expr)
      [(list 'if cond iff ift) `(if ,(loop cond) ,(loop ift) ,(loop iff))]
      [(list op args ...) `(,op ,@(map loop args))]
      [expr expr])))


(define-accelerator (expm1 real) real (lambda (x) (- (exp x) 1)))
(define-accelerator (log1p real) real (lambda (x) (log (+ 1 x))))
(define-accelerator (hypot real real) real (lambda (x y) (sqrt (+ (* x x) (* y y)))))
(define-accelerator (fma real real real) real (lambda (x y z) (+ (* x y) z)))
(define-accelerator (erfc real) real (lambda (x) (- 1 (erf x))))

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
