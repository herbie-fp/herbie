#lang racket

(require "../utils/common.rkt"
         "../syntax/syntax.rkt"
         "../syntax/platform.rkt"
         "../syntax/types.rkt"
         "batch.rkt")

(provide expr?
         expr<?
         all-subexpressions
         ops-in-expr
         spec-prog?
         impl-prog?
         node-is-impl?
         repr-of
         batch-reprs
         location-set
         location-get
         get-locations
         free-variables
         replace-expression
         batch-replace-expression
         replace-vars
         batch-get-locations
         batch-location-set)

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
    [(hole precision spec) (get-representation precision)]
    [(list op args ...) (impl-info op 'otype)]))

(define (batch-reprs batch ctx)
  (batch-map batch
             (lambda (get-repr node)
               (match node
                 [(literal val precision) (get-representation precision)]
                 [(? symbol?) (context-lookup ctx node)]
                 [(approx _ impl) (get-repr impl)]
                 [(hole precision spec) (get-representation precision)]
                 [(list op args ...) (impl-info op 'otype)]))))

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
    [((? batchref?) (? batchref?))
     (cond
       [(batchref<? a b) -1]
       [(batchref>? a b) 1]
       [else 0])]
    [((? batchref?) _) 1]
    [(_ (? batchref?)) -1]
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
    [((? hole?) (? hole?))
     (define cmp-spec (expr-cmp (hole-spec a) (hole-spec b)))
     (if (zero? cmp-spec)
         (expr-cmp (hole-precision a) (hole-precision b))
         cmp-spec)]
    [((? hole?) _) 1]
    [(_ (? hole?)) -1]
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

(define location? (listof natural-number/c))

(define (location-do loc prog f)
  (match* (prog loc)
    [(_ (? null?)) (f prog)]
    [((approx spec impl) (cons 1 rest)) (approx (location-do rest spec f) impl)]
    [((approx spec impl) (cons 2 rest)) (approx spec (location-do rest impl f))]
    [((hole prec spec) (cons 1 rest)) (hole prec (location-do rest spec f))]
    [((? list?) (cons idx rest)) (list-set prog idx (location-do rest (list-ref prog idx) f))]))

(define/contract (location-set loc prog prog*)
  (-> location? expr? (or/c expr? batchref?) expr?)
  (location-do loc prog (const prog*)))

(define (batch-location-set b e1 loc e2)
  (match loc
    ; If empty loc then we're replacing e1 by e2
    ['() e2]
    ; Otherwise, go one step and recurse
    [(cons idx rest)
     (define node (deref e1))
     (define child (location-get (list idx) node))
     (define child* (batch-location-set b child rest e2))
     (define node* (location-set (list idx) node child*))
     (batch-push! b (expr-recurse node* batchref-idx))]))

(define/contract (location-get loc prog)
  (-> location? expr? (or/c expr? batchref?))
  ; Clever continuation usage to early-return
  (let/ec return
    (location-do loc prog return)))

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

(define (batch-get-locations brf sub-brf)
  (reap [sow]
        (let loop ([brf brf]
                   [loc '()])
          (if (batchref<? brf sub-brf)
              (void)
              (match (deref brf)
                [(== (deref sub-brf)) (sow (reverse loc))]
                [(? literal?) (void)]
                [(? symbol?) (void)]
                [(approx _ impl) (loop impl (cons 2 loc))]
                [(list _ args ...)
                 (for ([arg (in-list args)]
                       [i (in-naturals 1)])
                   (loop arg (cons i loc)))])))))

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

(define (batch-replace-expression batch from to)
  (batch-apply! batch
                (λ (node)
                  (match node
                    [(== from) to]
                    [(? number?) node]
                    [(? literal?) node]
                    [(? symbol?) node]
                    [(approx spec impl) (approx spec impl)]
                    [(list op args ...) (cons op args)]))))

(module+ test
  (require rackunit)
  (check-equal? (replace-expression '(- x (sin x)) 'x 1) '(- 1 (sin 1)))

  (check-equal? (replace-expression '(/ (cos (* 2 x))
                                        (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
                                    'cos
                                    '(/ 1 cos))
                '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))
