#lang racket

(require "common.rkt"
         "accelerator.rkt" (submod "accelerator.rkt" internals)
         "ground-truth.rkt"
         (submod "syntax/syntax.rkt" internals)
         (submod "syntax/rules.rkt" internals)
         "syntax/types.rkt"
         "core/matcher.rkt")

(provide register-accelerator!
         register-accelerator-impl!
         define-accelerator
         define-accelerator-impl)

;; Adds an accelerator to the table.
(define (register-accelerator! name itypes otype spec)
  (match-define (list (or 'λ 'lambda) (list vars ...) body) spec)
  (for/fold ([ids '()]) ([var (in-list vars)])
    (if (member var ids)
        (error 'register-accelerator!
               "duplicate variable for ~a: ~a"
               name var)
        (cons var ids))) 
  (unless (= (length vars) (length itypes))
    (error 'register-accelerator!
           "implementation does not have expected arity: ~a ~a"
           (length vars)
           (length itypes)))
  (define ruleset-name (sym-append name '-accelerator))
  (define define-name (sym-append name '-define))
  (define undefine-name (sym-append name '-undefine))
  (define info (accelerator name itypes otype spec))
  (hash-set! accelerators name info)
  (register-operator! name itypes otype (list (cons 'ival #f)))
  (register-ruleset*! ruleset-name
                      (list 'numerics 'simplify)
                      (map cons vars itypes)
                      (list (list define-name body (cons name vars))
                            (list undefine-name (cons name vars) body))))

(define-syntax define-accelerator
  (syntax-rules ()
    [(_ (name itypes ...) otype impl)
     (register-accelerator! 'name (list 'itypes ...) 'otype 'impl)]))

(define (register-accelerator-impl! op
                                    name
                                    itypes
                                    otype
                                    #:impl [impl #f])
  (unless (accelerator-exists? op)
    (error 'register-accelerator-impl "must be an accelerator ~a" op))
  (match-define (accelerator _ _ _ spec) (hash-ref accelerators op))
  (match-define (list (or 'λ 'lambda) (list vars ...) body) spec)
  (define ctx (context vars otype itypes))
  (define impl-fn
    (or impl
        (let ([fn (eval-progs-real (list body) (list ctx))])
          (λ args (first (apply fn args))))))
  (register-operator-impl! op
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


(define-accelerator (expm1 real) real (lambda (x) (- (exp x) 1)))
(define-accelerator (log1p real) real (lambda (x) (log (+ 1 x))))
(define-accelerator (hypot real real) real (lambda (x y) (sqrt (+ (* x x) (* y y)))))
(define-accelerator (fma real real real) real (lambda (x y z) (+ (* x y) z)))
(define-accelerator (erfc real) real (lambda (x) (- 1 (erf x))))

(define-accelerator (fmsub real real real) real (lambda (x y z) (- (* x y) z)))
(define-accelerator (fnmadd real real real) real (lambda (x y z) (+ (neg (* x y)) z)))
(define-accelerator (fnmsub real real real) real (lambda (x y z) (- (neg (* x y)) z)))

; Specialized numerical functions
(define-ruleset* special-numerical-reduce (numerics simplify)
  #:type ([x real] [y real] [z real])
  [log1p-expm1 (log1p (expm1 x))          x]
  [hypot-1-def (sqrt (+ 1 (* y y)))       (hypot 1 y)]
  [fmm-def     (- (* x y) z)              (fma x y (neg z))]
  [fmm-undef   (fma x y (neg z))          (- (* x y) z)])

(define-ruleset* special-numerical-expand (numerics)
  #:type ([x real] [y real])
  [log1p-expm1-u x              (log1p (expm1 x))]
  [expm1-log1p-u x              (expm1 (log1p x))])

(define-ruleset* erf-rules (special simplify)
  #:type ([x real])
  [erf-odd          (erf (neg x))        (neg (erf x))])

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
