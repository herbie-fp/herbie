#lang racket

(require "config.rkt" "syntax/syntax.rkt" "syntax/rules.rkt" "core/matcher.rkt" "programs.rkt")
(provide get-expander get-evaluator)

(define (evaluation-rule? rule)
  (and (list? (rule-input rule))
       (null? (free-variables (rule-input rule)))))

(define (definition-rule? rule)
  (and (list? (rule-input rule))
       (andmap herbie-variable? (cdr (rule-input rule)))))

(define (all-ops expr good?)
  (match expr
    [(? constant-or-num?) #t]
    [(? herbie-variable?) #t]
    [(list f args ...)
     (and (good? f) (andmap (curryr all-ops good?) args))]))

(define expanders (make-hash))
(define evaluators (make-hash))

(define (make-expander primitives)
  (define known-functions (make-hash))
  (for ([op primitives])
    (dict-set! known-functions op #f))
  (define known-function? (curry dict-has-key? known-functions))

  (define definition-rules (filter definition-rule? (*rules*)))

  (let loop ()
    (define continue? #f)
    (for ([rule definition-rules])
      (define op (car (rule-input rule)))
      (when (and (not (known-function? op))
                 (all-ops (rule-output rule) known-function?))
        (dict-set! known-functions op rule)
        (set! continue? #t))))

  (define (simplify expr)
    (and (list? expr)
         (dict-ref known-functions (car expr) #f)
         (rule-rewrite (dict-ref known-functions (car expr)) expr)))

  (λ (expr)
    (let loop ([expr expr])
      (let ([expr* (simplify expr)])
        (if expr* (loop expr*) expr)))))

(define (make-evaluator)
  (define evaluation-rules
    (for/hash ([rule (*rules*)] #:when (evaluation-rule? rule))
      (values (rule-input rule) (rule-output rule))))

  (λ (expr) (dict-ref evaluation-rules expr expr)))

(define (get-expander primitives)
  (hash-ref! expanders (cons primitives (*flags*)) (λ () (make-expander primitives))))

(define (get-evaluator)
  (hash-ref! evaluators (*flags*) (λ () (make-evaluator))))
