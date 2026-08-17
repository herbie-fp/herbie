#lang racket

(require "../utils/common.rkt"
         "../syntax/syntax.rkt"
         "../syntax/platform.rkt"
         "../syntax/types.rkt"
         "../syntax/block.rkt")

(provide expr?
         expr<?
         all-subexpressions
         ops-in-expr
         spec-prog?
         impl-prog?
         node-is-impl?
         repr-of
         block-repr-of
         get-locations
         free-variables
         replace-expression
         block-replace-expression!
         block-replace-subexpr
         replace-vars)

;; Programs are just lisp lists plus atoms

(define expr? (or/c list? symbol? boolean? real? literal? approx?))

(define (node-is-impl? node)
  (match node
    [(? number?) #f]
    [(list (? operator-exists? op) args ...) #f]
    [_ #t]))

;; Returns repr name
;; Fast version does not recurse into functions applications
(define (repr-of expr ctx)
  (match expr
    [(literal val precision) (get-representation precision)]
    [(? symbol?) (context-lookup ctx expr)]
    [(approx _ impl) (repr-of impl ctx)]
    [(list op args ...) (impl-info op 'otype)]))

(define (block-repr-of v)
  (define block (val-block v))
  (define var-reprs (map cons (block-vars block) (block-var-reprs block)))
  (let loop ([v v])
    (match (val-def v)
      [(literal val precision) (get-representation precision)]
      [(? symbol? node) (dict-ref var-reprs node)]
      [(approx _ impl) (loop impl)]
      [(list op args ...) (impl-info op 'otype)])))

(define (all-subexpressions expr #:reverse? [reverse? #f])
  (define subexprs
    (reap [sow]
          (let loop ([expr expr])
            (sow expr)
            (match expr
              [(? number?) (void)]
              [(? literal?) (void)]
              [(? symbol?) (void)]
              [(approx _ impl) (loop impl)]
              [`(if ,c ,t ,f)
               (loop c)
               (loop t)
               (loop f)]
              [(list _ args ...)
               (for ([arg args])
                 (loop arg))]))))
  (remove-duplicates (if reverse?
                         (reverse subexprs)
                         subexprs)))

(define (ops-in-expr expr)
  (remove-duplicates (filter-map (lambda (e) (and (pair? e) (first e))) (all-subexpressions expr))))

;; Is the expression in LSpec (real expressions)?
(define (spec-prog? expr)
  (match expr
    [(? symbol?) #t]
    [(? number?) #t]
    [(list 'if cond ift iff) (and (spec-prog? cond) (spec-prog? ift) (spec-prog? iff))]
    [(list (? operator-exists?) args ...) (andmap spec-prog? args)]
    [_ #f]))

;; Is the expression in LImpl (floating-point implementations)?
(define (impl-prog? expr)
  (match expr
    [(? symbol?) #t]
    [(? literal?) #t]
    [(approx spec impl) (and (spec-prog? spec) (impl-prog? impl))]
    [(list (? impl-exists?) args ...) (andmap impl-prog? args)]
    [_ #f]))

;; Total order on expressions

(define (expr-cmp a b)
  (match* (a b)
    [((? val?) (? val?)) (expr-cmp (val-def a) (val-def b))]
    [((? val?) _) (expr-cmp (val-def a) b)]
    [(_ (? val?)) (expr-cmp a (val-def b))]
    [((? list?) (? list?))
     (define len-a (length a))
     (define len-b (length b))
     (cond
       [(< len-a len-b) -1]
       [(> len-a len-b) 1]
       [else
        (let loop ([a a]
                   [b b])
          (cond
            [(null? a) 0]
            [else
             (define cmp (expr-cmp (car a) (car b)))
             (if (zero? cmp)
                 (loop (cdr a) (cdr b))
                 cmp)]))])]
    [((? list?) _) 1]
    [(_ (? list?)) -1]
    [((? approx?) (? approx?))
     (define cmp-spec (expr-cmp (approx-spec a) (approx-spec b)))
     (if (zero? cmp-spec)
         (expr-cmp (approx-impl a) (approx-impl b))
         cmp-spec)]
    [((? approx?) _) 1]
    [(_ (? approx?)) -1]
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
  (negative? (expr-cmp a b)))

;; Converting constants

(define (free-variables prog)
  (match prog
    [(? literal?) '()]
    [(? number?) '()]
    [(? symbol?) (list prog)]
    [(approx _ impl) (free-variables impl)]
    [(list _ args ...) (remove-duplicates (append-map free-variables args))]))

(define (replace-vars dict expr)
  (let loop ([expr expr])
    (match expr
      [(? literal?) expr]
      [(? number?) expr]
      [(? symbol?) (dict-ref dict expr expr)]
      [(approx impl spec) (approx (loop impl) (loop spec))]
      [(list op args ...) (cons op (map loop args))])))

(define (get-locations expr subexpr)
  (reap [sow]
        (let loop ([expr expr]
                   [loc '()])
          (match expr
            [(== subexpr) (sow (reverse loc))]
            [(? literal?) (void)]
            [(? symbol?) (void)]
            [(approx _ impl) (loop impl (cons 2 loc))]
            [(list _ args ...)
             (for ([arg (in-list args)]
                   [i (in-naturals 1)])
               (loop arg (cons i loc)))]))))

(define/contract (replace-expression expr from to)
  (-> expr? expr? expr? expr?)
  (let loop ([expr expr])
    (match expr
      [(== from) to]
      [(? number?) expr]
      [(? literal?) expr]
      [(? symbol?) expr]
      [(approx spec impl) (approx (loop spec) (loop impl))]
      [(list op args ...) (cons op (map loop args))])))

(define (block-replace-expression! block from to)
  (define from* (val-def (block-add! block from))) ;; a hack on how not to use val-def for "from"
  (define (f node)
    (match node
      [(== from*) to]
      [(? number?) node]
      [(? literal?) node]
      [(? symbol?) node]
      [(approx spec impl) (approx spec impl)]
      [(list op args ...) (cons op args)]))
  (block-recurse block
                 (λ (v recurse)
                   (define node (val-def v))
                   (define node* (f node))
                   (let loop ([node* node*])
                     (match node*
                       [(? val? v) (recurse v)]
                       [_ (block-push! block (expr-recurse node* (compose val-idx loop)))])))))

;; Replace all occurrences of `from` with `to` in expression `expr`, returning a new val
;; Only recurses into impl parts, not specs
(define (block-replace-subexpr block expr from to [can-refer #f])
  (define cache (make-hasheq))
  (define from-idx (val-idx from))
  (let loop ([v expr])
    (define idx (val-idx v))
    (cond
      [(< idx from-idx) v]
      [(= idx from-idx) to]
      [(and can-refer (not (set-member? can-refer idx))) v]
      [else
       (hash-ref! cache
                  idx
                  (lambda ()
                    (match (val-def v)
                      [(approx spec impl)
                       (define impl* (loop impl))
                       (if (= (val-idx impl*) (val-idx impl))
                           v
                           (block-push! block (approx spec (val-idx impl*))))]
                      [node
                       (define unchanged? #t)
                       (define node*
                         (expr-recurse node
                                       (lambda (arg)
                                         (define arg* (loop arg))
                                         (unless (= (val-idx arg*) (val-idx arg))
                                           (set! unchanged? #f))
                                         (val-idx arg*))))
                       (if unchanged?
                           v
                           (block-push! block node*))])))])))

(module+ test
  (require rackunit)
  (check-equal? (replace-expression '(- x (sin x)) 'x 1) '(- 1 (sin 1)))

  (check-equal? (replace-expression '(/ (cos (* 2 x))
                                        (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
                                    'cos
                                    '(/ 1 cos))
                '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))
