#lang racket

(require
 "common.rkt"
 "compiler.rkt"
 "ground-truth.rkt"
 "core/matcher.rkt"
 (submod "syntax/syntax.rkt" internals)
 submod "syntax/rules.rkt" internals)
 "syntax/types.rkt")

(struct accelerator-operator (body variables itypes otypes))

(define accelerator-operators (make-hasheq))

(define (register-accelerator-operator!
         name body variables
         [itypes (make-list (length variables) 'real)] [otype 'real])
  (define define-name (sym-append name '- 'define))
  (define undefine-name (sym-append name '- 'undefine))
  (hash-set! accelerator-operators
             name
             (accelerator-operator body variables itypes otype))
  (register-operator! name
                      itypes otype
                      (list ('ival . (compile-spec body variables))))
  ;; TODO: Are the groups right now?
  (register-ruleset*! name
                      (list 'numerics 'simplify)
                      (map cons variables itypes)
                      (list (list define-name body (list* name variables))))
  (register-ruleset*! name
                      (list 'numerics 'simplify)
                      (map cons variables itypes)
                      (list (list undefine-name (list* name variables) body))))

(define (register-accelerator-implementation! operator name
                                              itypes otype
                                              [implementation #f])
  (match-define (accelerator-operator body variables _ _) (dict-ref accelerator-operators operator))
  (register-operator-impl!
   operator name
   itypes otype
   (list
    ('fl . (or 
            implementation
            (compose first (eval-progs-real 
                            (list body)
                            (list (context variables otype itypes)))))))))

;; This is macro expansion!
;; TODO: Hygiene issues?
;; TODO: Is there a way to do this with just racket?
;; TODO: Test
(define (expand-accelerators rules expression)
  (define undefine-rules
    (filter
     (compose
      (curry set-member? (map sym-append '- 'undefine) (dict-keys accelerator-operators))
      rule-name)
     rules))
  ;; Apply the first rule that matches top down. We can only be sure we have
  ;; a real match if the term does not occur in the syntactic scope of any
  ;; other syntactic extensions.
  (let rewrite ([expression* expression])
    (match (or
            (ormap (curryr rule-apply expression*) undefine-rules)
            expression*)
      [(list operator operands ...) (cons operator (map rewrite operands))]
      [_ expression*])))
