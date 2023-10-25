#lang racket

(require
 "./programs.rkt"
 (submod "syntax/syntax.rkt" internals) "./syntax/types.rkt")

;; Conceptually, accelerators are strictly implementations of operator ASTs (compositions of real
;; operators): we're describing fast versions of e.g. reciprocal square root, not introducing
;; fundamentally new trigonometric functions. However, as an implementation detail, the current
;; structure of Herbie basically requires us to add accelerators to the operator table as "new"
;; operators. We maintain a separate accelerator operator table to keep the difference between
;; accelerator and non-accelerator operators straight, since accelerators also touch operator
;; implementations, rewrite rules, and taylor rewrite rules.
(struct accelerator-operator (variables definition ival))

(define accelerator-operators (make-hasheq))

(define (register-accelerator-operator! name variables definition)
  (define ctx (context variables 'real (make-list (length variables) 'real)))
  (define ival-implementation (compile-prog definition 'ival ctx))
  (hash-set! accelerator-operators
             name
             (accelerator-operator variables definition))
  (register-operator!
   name
   (make-list (length variables) 'real) 'real
   (list
    (cons 'ival ival-implementation))))
