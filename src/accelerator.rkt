#lang racket

(require
 math/bigfloat

 "compiler.rkt"
 "ground-truth.rkt"
 (submod "syntax/syntax.rkt" internals)
 (submod "syntax/rules.rkt" internals)
 "syntax/types.rkt")

(provide
 register-accelerator-operator!
 register-accelerator-implementation!
 expand-accelerators)

(module+ internals
  (provide register-accelerator-operator!
           register-accelerator-implementation!))

;; This might have to stay, it's from common.rkt
(define (sym-append . args)
  (string->symbol (apply string-append (map ~a args))))

(struct accelerator-operator (body variables itypes otypes))

(define accelerator-operators (make-hasheq))

(define (register-accelerator-operator!
         name body variables
         [itypes (make-list (length variables) 'real)] [otype 'real])
  (define ruleset-name (sym-append name '- 'accelerator))
  (define define-name (sym-append name '- 'define))
  (define undefine-name (sym-append name '- 'undefine))
  (hash-set! accelerator-operators
             name
             (accelerator-operator body variables itypes otype))
  (register-operator! name
                      itypes otype
                      (list (cons 'ival (compile-spec body variables))))
  ;; TODO: Are the groups right now?
  (register-ruleset*! ruleset-name
                     '(numerics simplify)
                     (map cons variables itypes)
                     `((,define-name ,body (,name ,@variables))
                       (,undefine-name (,name ,@variables) ,body))))

(define (register-accelerator-implementation! operator name
                                              itypes otype
                                              [implementation #f])
  (match-define (accelerator-operator body variables _ _) (dict-ref accelerator-operators operator))
  (register-operator-impl!
   operator name
   itypes otype
   (list
    (cons 'fl
          (or 
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
      (curry set-member? (map (curryr sym-append '- 'undefine) (dict-keys accelerator-operators)))
      rule-name)
     rules))
  ;; Apply the first rule that matches top down. We can only be sure we have
  ;; a real match if the term does not occur in the syntactic scope of any
  ;; other syntactic extensions.
  (let rewrite ([expression* expression])
    (match (or
            (let ([expression** (ormap (curryr rule-apply expression*) undefine-rules)])
              (and expression** (car expression**)))
            expression*)
      [(list operator operands ...) (cons operator (map rewrite operands))]
      [_ expression*])))

;; Temporarily copied to avoid cycles

(define (merge-bindings binding1 binding2)
  (and binding1
       binding2
       (let/ec quit
         (for/fold ([binding binding1]) ([(k v) (in-dict binding2)])
           (dict-update binding k (λ (x) (if (equal? x v) v (quit #f))) v)))))

(define (pattern-match pattern expr)
  (match pattern
   [(? number?)
    (and (equal? pattern expr) '())]
   [(? variable?)
    (list (cons pattern expr))]
   [(list phead _ ...)
    (and (list? expr)
         (equal? (car expr) phead)
         (= (length expr) (length pattern))
         (for/fold ([bindings '()])
             ([pat (cdr pattern)] [subterm (cdr expr)])
           (merge-bindings bindings (pattern-match pat subterm))))]))

(define (pattern-substitute pattern bindings)
  ; pattern binding -> expr
  (match pattern
   [(? number?) pattern]
   [(? variable?)
    (dict-ref bindings pattern)]
   [(list phead pargs ...)
    (cons phead (map (curryr pattern-substitute bindings) pargs))]))

(define (rule-apply rule expr)
  (let ([bindings (pattern-match (rule-input rule) expr)])
    (if bindings
        (cons (pattern-substitute (rule-output rule) bindings) bindings)
        #f)))

(register-accelerator-operator! 'expm1 '(- (exp x) 1) '(x))
(register-accelerator-operator! 'log1p '(log (+ 1 x)) '(x))
(register-accelerator-operator! 'hypot '(sqrt (+ (* x x) (* y y))) '(x y))
(register-accelerator-operator! 'fma '(+ (* x y) z) '(x y z))
(register-accelerator-operator! 'erfc '(- 1 (erf x)) '(x))
