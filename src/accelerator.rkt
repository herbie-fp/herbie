#lang racket

(require
 "common.rkt"
 "compiler.rkt"
 "ground-truth.rkt"
 "core/matcher.rkt"
 (submod "syntax/syntax.rkt" internals)
 (submod "syntax/rules.rkt" internals)
 "syntax/types.rkt")

(provide (struct-out accelerator-operator) accelerator-operators
         register-accelerator-operator! register-accelerator-implementation! remove-accelerators)

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
;; - When does module code get run? 

;; TODOs
;; - Handle taylor after desugar in taylor code in patch.rkt
;; - Remove any old accelerator stuff in taylor*
;; - Write simple macros, copy define-operator
;; - Add def accel declarations to syntax/syntax.rkt
;; - Add def accel impl declarations to files in repr/*.rkt

(define (register-accelerator-operator!
         name definition variables
         [itypes (make-list (length variables) 'real)] [otype 'real])
  (define define-name (sym-append name '- 'define))
  (define undefine-name (sym-append name '- 'undefine))
  (hash-set! accelerator-operators
             name
             (accelerator-operator definition variables itypes otype))
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
   ;; TODO: Add 'simplify group here too
   (list 'numerics)
   (map cons variables itypes)
   (list (list undefine-name (list* name variables) definition))))

(define (register-accelerator-implementation! operator name
                                              itypes otype
                                              [implementation #f])
  (match-define (accelerator-operator definition variables _ _)
                    (dict-ref accelerator-operators operator))
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
      (compose
       first
       (eval-progs-real
        (list definition)
        (list (context variables otype itypes)))))))))

(define (remove-accelerators rules expression)
  (match expression
    [(list operator operands ...)
     (define expression* (cons operator (map (curry remove-accelerators rules) operands)))
     ;; No reason to get this list each recursion
     (define undefine-rules
       (filter
        (lambda (rule)
          (set-member?
           (map
            (curryr sym-append '- 'undefine)
            (dict-keys accelerator-operators))
           (rule-name rule)))
        rules))
     ;; Apply the first rule that matches, there will only be one
     (or
      (let ([match (ormap (curryr rule-apply expression*) undefine-rules)])
        (and match (car match)))
      expression*)]
    [_ expression]))

(register-accelerator-operator! 'reciprocal '(/ 1 x) '(x))
(register-accelerator-implementation! 'reciprocal 'reciprocal.f64 (list (get-representation 'binary64)) (get-representation 'binary64)) 
(register-accelerator-implementation! 'reciprocal 'reciprocal.f32 (list (get-representation 'binary32)) (get-representation 'binary32)) 

(register-accelerator-operator! 'reciprocal-sqrt '(/ 1 (sqrt x)) '(x))
(register-accelerator-implementation! 'reciprocal-sqrt 'reciprocal-sqrt.f64 (list (get-representation 'binary64)) (get-representation 'binary64)) 
(register-accelerator-implementation! 'reciprocal-sqrt 'reciprocal-sqrt.f32 (list (get-representation 'binary32)) (get-representation 'binary32)) 

;; 1. Test reciprocal, make sure it is used a lot
;; 2. Test reciprocal square root, same
;; 3. FFI actual implementation, check that it doesn't get used as much
;; 4. Modify cost to make reciprocal cheap, check that it now gets used
