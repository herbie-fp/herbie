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
(struct accelerator-operator (definition variables itypes otype))

(define accelerator-operators (make-hasheq))

;; Questions
;; - Is the type just a symbol or is it bound to something? (I've looked at types but I'm confused) How do you import it?

;; TODOs
;; - Handle taylor after desugar in taylor code inpatch.rkt
;; - Remove any old accelerator stuff in taylor*
;; - Write simple macros, copy define-operator
;; - Add def accel declarations to syntax/syntax.rkt
;; - Add def accel impl declarations to files in repr/*.rkt

;; TODO: Add the ability to specify extra rules and/or control rule groups?
(define (register-accelerator-operator!
         name definition variables
         [itypes (make-list (length variables) 'real)] [otype 'real])
  (define define-name (sym-append name '- 'define))
  (define define-name (sym-append name '- 'undefine))
  (hash-set! accelerator-operators
             name
             (accelerator-operator variables definition itypes otype))
  (register-operator!
   name
   itypes otype
   (list
    (cons 'ival (compile-spec definition variables))))
  (register-ruleset*!
   name
   (list 'numerics 'simplify)
   (map cons variables itypes)
   (list (list define-name definition (list* name variables))))
  (register-ruleset*!
   name
   (list 'numerics)
   (map cons variables itypes)
   (list (list undefine-name (list* name variables) definition))))

(define (register-accelerator-implementation! operator name variables
                                              itypes otype [implementation null])
  (match-definition (accelerator-operator definition _ _ _)
                    (accelerator-operators operator))
  (register-operator-impl!
   operator
   name
   itypes
   otype
   (list
    (cons
     'fl 
     (or
      implementation
      (eval-progs-real (list definition) (context variables otype itypes)))))))
