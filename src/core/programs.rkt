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
         repr-of
         repr-of-node
         location-set
         location-get
         get-locations
         free-variables
         replace-expression
         replace-vars
         batch-get-locations
         batch-location-set)

;; Programs are just lisp lists plus atoms

(define expr? (or/c list? symbol? boolean? real? literal? approx?))

;; Returns repr name
;; Fast version does not recurse into functions applications
(define (repr-of expr ctx)
  (match expr
    [(literal val precision) (get-representation precision)]
    [(? symbol?) (context-lookup ctx expr)]
    [(approx _ impl) (repr-of impl ctx)]
    [(hole precision spec) (get-representation precision)]
    [(list 'if cond ift iff) (repr-of ift ctx)]
    [(list op args ...) (impl-info op 'otype)]))

; Index inside (batch-nodes batch) -> type
(define (repr-of-node batch idx ctx)
  (define node (vector-ref (batch-nodes batch) idx))
  (match node
    [(literal val precision) (get-representation precision)]
    [(? symbol?) (context-lookup ctx node)]
    [(approx _ impl) (repr-of-node batch impl ctx)]
    [(hole precision spec) (get-representation precision)]
    [(list 'if cond ift iff) (repr-of-node batch ift ctx)]
    [(list op args ...) (impl-info op 'otype)]))

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
    [(list 'if cond ift iff) (and (impl-prog? cond) (impl-prog? ift) (impl-prog? iff))]
    [(list (? impl-exists?) args ...) (andmap impl-prog? args)]
    [_ #f]))

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

(define location? (listof natural-number/c))

(define (location-do loc prog f)
  (match* (prog loc)
    [(_ (? null?)) (f prog)]
    [((approx spec impl) (cons 1 rest)) (approx (location-do rest spec f) impl)]
    [((approx spec impl) (cons idx rest)) (approx spec (location-do rest impl f))]
    [((hole prec spec) (cons 1 rest)) (hole prec (location-do rest spec f))]
    [((? list?) (cons idx rest)) (list-set prog idx (location-do rest (list-ref prog idx) f))]))

(define/contract (location-set loc prog prog*)
  (-> location? expr? expr? expr?)
  (location-do loc prog (const prog*)))

(define/contract (batch-location-set loc0 mutable-batch cache full-batchref sub-batchref)
  (-> location? mutable-batch? hash? batchref? batchref? batchref?)
  (match-define (batchref sub-batch sub-idx) sub-batchref)
  (match-define (batchref full-batch full-idx) full-batchref)
  (unless (equal? sub-batch full-batch)
    (error 'batch-location-set "Function assumes that batches are equal"))

  (define (get-node idx)
    (hash-ref cache idx (λ () (vector-ref (batch-nodes full-batch) idx))))

  (define (push-node node)
    (define idx (mutable-batch-push! mutable-batch node))
    (hash-set! cache idx node)
    idx)

  #;(define (mutable-batch-munge-batchref! b-ref)
      (define b (batchref-batch b-ref))
      (define nodes (batch-nodes b))
      (define node-refs (batchref-all-subnodes b-ref #:reverse? #t #:include-spec? #t))
      (for/last ([idx (in-list node-refs)])
        (push-node (vector-ref nodes idx))))

  (define idx*
    (let loop ([loc0 loc0]
               [idx full-idx])
      (let ([node (get-node idx)])
        (match* (node loc0)
          [(_ (? null?))
           sub-idx
           #;(if (equal? full-batch sub-batch)
                 sub-idx ; sub-nodes are already in full-batch - no need to munge
                 (mutable-batch-munge-batchref! mutable-batch sub-batchref))]
          [((approx spec impl) (cons 1 rest)) (push-node (approx (loop rest spec) impl))]
          [((approx spec impl) (cons 2 rest)) (push-node (approx spec (loop rest impl)))]
          [((hole prec spec) (cons 1 rest)) (push-node (hole prec (loop rest spec)))]
          [((list op args ...) (cons i rest))
           (define args*
             (for/list ([arg (in-list args)]
                        [n (in-naturals 1)])
               (if (equal? n i)
                   (loop rest arg)
                   arg)))
           (push-node (cons op args*))]))))
  (batchref full-batch idx*))

(define/contract (location-get loc prog)
  (-> location? expr? expr?)
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

(define (batch-get-locations full-batchref sub-batchref)
  (match-define (batchref full-batch full-idx) full-batchref)
  (match-define (batchref sub-batch sub-idx) sub-batchref)
  (unless (equal? sub-batch full-batch)
    (error 'batch-get-locations "Function assumes that batches are equal"))
  (define nodes (batch-nodes full-batch))
  (define sub-node (vector-ref nodes sub-idx))

  (reap [sow]
        (let loop ([idx full-idx]
                   [loc '()])
          (match (vector-ref nodes idx)
            [(== sub-node) (sow (reverse loc))]
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

(module+ test
  (require rackunit)
  (check-equal? (replace-expression '(- x (sin x)) 'x 1) '(- 1 (sin 1)))

  (check-equal? (replace-expression '(/ (cos (* 2 x))
                                        (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
                                    'cos
                                    '(/ 1 cos))
                '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))
