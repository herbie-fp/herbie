#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt")

(provide expr?
         expr-contains?
         expr<?
         all-subexpressions
         ops-in-expr
         impl-prog?
         repr-of
         location-do
         location-get
         free-variables
         replace-expression
         replace-vars)

;; Programs are just lisp lists plus atoms

(define expr? (or/c list? symbol? boolean? real? literal?))

;; Returns repr name
;; Fast version does not recurse into functions applications
(define (repr-of expr ctx)
  (match expr
    [(? literal?) (get-representation (literal-precision expr))]
    [(? variable?) (context-lookup ctx expr)]
    [(list 'if cond ift iff) (repr-of ift ctx)]
    [(list op args ...) (impl-info op 'otype)]))

(define (expr-contains? expr pred)
  (let loop ([expr expr])
    (match expr
      [(list elems ...) (ormap loop elems)]
      [term (pred term)])))

(define (all-subexpressions expr #:reverse? [reverse? #f])
  (define subexprs
    (reap [sow]
          (let loop ([expr expr])
            (sow expr)
            (match expr
              [(? number?) (void)]
              [(? literal?) (void)]
              [(? variable?) (void)]
              [`(if ,c ,t ,f)
               (loop c)
               (loop t)
               (loop f)]
              [(list _ args ...)
               (for ([arg args])
                 (loop arg))]))))
  (remove-duplicates (if reverse? (reverse subexprs) subexprs)))

(define (ops-in-expr expr)
  (remove-duplicates (filter-map (lambda (e) (and (pair? e) (first e))) (all-subexpressions expr))))

;; Returns `#t` if program is a program of operator implementations.
(define (impl-prog? expr)
  (let/ec return
          (let loop ([expr expr])
            (match expr
              [(? literal?) (void)]
              [(? number?) (return #f)]
              [(? symbol?) (void)]
              [(list 'if cond ift iff)
               (loop cond)
               (loop ift)
               (loop iff)]
              [(list (? impl-exists?) args ...) (for-each loop args)]
              [(list _ ...) (return #f)]))
          #t))

;; Total order on expressions

(define (expr-cmp a b)
  (match* (a b)
    [((? list?) (? list?))
     (define len-a (length a))
     (define len-b (length b))
     (cond
       [(< len-a len-b) -1]
       [(> len-a len-b) 1]
       [else
        (let loop ([a a]
                   [b b])
          (if (null? a)
              0
              (let ([cmp (expr-cmp (car a) (car b))])
                (if (zero? cmp) (loop (cdr a) (cdr b)) cmp))))])]
    [((? list?) _) 1]
    [(_ (? list?)) -1]
    [((? symbol?) (? symbol?))
     (cond
       [(symbol<? a b) -1]
       [(symbol=? a b) 0]
       [else 1])]
    [((? symbol?) _) 1]
    [(_ (? symbol?)) -1]
    ;; Need both cases because `reduce` uses plain numbers
    [((or (? literal? (app literal-value a)) (? number? a)) (or (? literal? (app literal-value b))
                                                                (? number? b)))
     (cond
       [(< a b) -1]
       [(= a b) 0]
       [else 1])]))

(define (expr<? a b)
  (< (expr-cmp a b) 0))

;; Converting constants

(define (free-variables prog)
  (match prog
    [(? literal?) '()]
    [(? number?) '()]
    [(? variable?) (list prog)]
    [`(,op ,args ...) (remove-duplicates (append-map free-variables args))]))

(define (replace-vars dict expr)
  (cond
    [(dict-has-key? dict expr) (dict-ref dict expr)]
    [(list? expr) (cons (replace-vars dict (car expr)) (map (curry replace-vars dict) (cdr expr)))]
    [#t expr]))

(define location? (listof natural-number/c))

(define/contract (location-do loc prog f)
  (-> location? expr? (-> expr? expr?) expr?)
  (cond
    [(null? loc) (f prog)]
    [(not (pair? prog)) (error "Bad location: cannot enter " prog "any further.")]
    [#t
     ; Inlined loop for speed
     (let loop ([idx (car loc)]
                [lst prog])
       (if (= idx 0)
           (cons (location-do (cdr loc) (car lst) f) (cdr lst))
           (cons (car lst) (loop (- idx 1) (cdr lst)))))]))

(define/contract (location-get loc prog)
  (-> location? expr? expr?)
  ; Clever continuation usage to early-return
  (let/ec return (location-do loc prog return)))

(define/contract (replace-expression haystack needle needle*)
  (-> expr? expr? expr? expr?)
  (match haystack
    [(== needle) needle*]
    [(list op args ...) (cons op (map (curryr replace-expression needle needle*) args))]
    [x x]))

(module+ test
  (require rackunit)
  (check-equal? (replace-expression '(- x (sin x)) 'x 1) '(- 1 (sin 1)))

  (check-equal? (replace-expression '(/ (cos (* 2 x))
                                        (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
                                    'cos
                                    '(/ 1 cos))
                '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))
