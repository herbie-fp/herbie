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
         type-of
         repr-of
         location-do
         location-get
         eval-application
         free-variables
         replace-expression
         replace-vars)

(define expr? (or/c list? symbol? boolean? real? literal?))

;; Programs are just lambda expressions

;; Returns type name
;; Fast version does not recurse into functions applications
(define (type-of expr ctx)
  (match expr
    [(? number?) 'real]
    [(? variable?) (representation-type (context-lookup ctx expr))]
    [(list 'if cond ift iff) (type-of ift ctx)]
    [(? literal?) 'real]
    [(list op args ...) (representation-type (impl-info op 'otype))]))

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
        (let loop ([a a] [b b])
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
     (let loop ([idx (car loc)] [lst prog])
       (if (= idx 0)
           (cons (location-do (cdr loc) (car lst) f) (cdr lst))
           (cons (car lst) (loop (- idx 1) (cdr lst)))))]))

(define/contract (location-get loc prog)
  (-> location? expr? expr?)
  ; Clever continuation usage to early-return
  (let/ec return (location-do loc prog return)))

;; This is a transcription of egg-herbie/src/math.rs, lines 97-149
(define (eval-application op . args)
  (define exact-value? (conjoin number? exact?))
  (match (cons op args)
    [(list '+ (? exact-value? as) ...) (apply + as)]
    [(list '- (? exact-value? as) ...) (apply - as)]
    [(list '* (? exact-value? as) ...) (apply * as)]
    [(list '/ (? exact-value? num) (? exact-value? den)) (and (not (zero? den)) (/ num den))]
    [(list 'neg (? exact-value? arg)) (- arg)]
    [(list 'pow (? exact-value? a) (? exact-value? b))
     (cond
       [(and (zero? b) (not (zero? a))) 1]
       [(and (zero? a) (positive? b)) 0]
       [(and (not (zero? a)) (integer? b)) (expt a b)]
       [else #f])]
    [(list 'sqrt (? exact-value? a))
     (let ([s1 (sqrt (numerator a))] [s2 (sqrt (denominator a))])
       (and (real? s1) (real? s2) (exact? s1) (exact? s2) (/ s1 s2)))]
    [(list 'cbrt (? exact-value? a))
     (define inexact-num (inexact->exact (expt (numerator a) 1/3)))
     (define inexact-den (inexact->exact (expt (denominator a) 1/3)))
     (and (real? inexact-num)
          (real? inexact-den)
          (= (expt inexact-num 3) (numerator a))
          (= (expt inexact-den 3) (denominator a))
          (/ inexact-num inexact-den))]
    [(list 'fabs (? exact-value? a)) (abs a)]
    [(list 'floor (? exact-value? a)) (floor a)]
    [(list 'ceil (? exact-value? a)) (ceiling a)]
    [(list 'round (? exact-value? a)) (round a)]
    ;; Added
    [(list 'exp 0) 1]
    [(list 'log 1) 0]
    [_ #f]))

(module+ test
  (require rackunit)

  (check-equal? (eval-application '+ 1 1) 2)
  (check-equal? (eval-application '+) 0)
  (check-equal? (eval-application '/ 1 0) #f) ; Not valid
  (check-equal? (eval-application 'cbrt 1) 1)
  (check-equal? (eval-application 'log 1) 0)
  (check-equal? (eval-application 'exp 2) #f)) ; Not exact

(define/contract (replace-expression haystack needle needle*)
  (-> expr? expr? expr? expr?)
  (match haystack
    [(== needle) needle*]
    [(list op args ...) (cons op (map (curryr replace-expression needle needle*) args))]
    [x x]))

(module+ test
  (check-equal? (replace-expression '(- x (sin x)) 'x 1) '(- 1 (sin 1)))

  (check-equal? (replace-expression '(/ (cos (* 2 x))
                                        (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
                                    'cos
                                    '(/ 1 cos))
                '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))