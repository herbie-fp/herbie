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
         make-block-replace-cache
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

(define (val-expr-cmp a b)
  (expr-cmp/raw (val-idx a)
                (val-block a)
                (if (val? b)
                    (val-idx b)
                    b)
                (and (val? b) (val-block b))))

(define (expr-cmp/raw a a-block b b-block)
  (define a-node
    (if a-block
        (block-node a-block a)
        a))
  (define b-node
    (if b-block
        (block-node b-block b)
        b))
  (cond
    [(and (list? a-node) (list? b-node))
     (define len-a (length a-node))
     (define len-b (length b-node))
     (cond
       [(< len-a len-b) -1]
       [(> len-a len-b) 1]
       [else
        (define cmp-op (expr-cmp (car a-node) (car b-node)))
        (if (zero? cmp-op)
            (let loop ([a-args (cdr a-node)]
                       [b-args (cdr b-node)])
              (cond
                [(null? a-args) 0]
                [else
                 (define cmp (expr-cmp/raw (car a-args) a-block (car b-args) b-block))
                 (if (zero? cmp)
                     (loop (cdr a-args) (cdr b-args))
                     cmp)]))
            cmp-op)])]
    [(and (approx? a-node) (approx? b-node))
     (define cmp-spec (expr-cmp (approx-spec a-node) (approx-spec b-node)))
     (if (zero? cmp-spec)
         (expr-cmp/raw (approx-impl a-node) a-block (approx-impl b-node) b-block)
         cmp-spec)]
    [else (expr-cmp a-node b-node)]))

(define (expr-cmp a b)
  (cond
    [(val? a) (val-expr-cmp a b)]
    [(val? b) (- (val-expr-cmp b a))]
    [else
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
          [else 1])])]))

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
(struct block-replace-cache ([values #:mutable] [generations #:mutable] [generation #:mutable]))

(define (make-block-replace-cache block)
  (define capacity (max 1 (block-length block)))
  (block-replace-cache (make-vector capacity) (make-vector capacity -1) 0))

(define (prepare-block-replace-cache! cache block)
  (define capacity (vector-length (block-replace-cache-values cache)))
  (when (> (block-length block) capacity)
    (define new-capacity (max (block-length block) (* 2 capacity)))
    (set-block-replace-cache-values! cache (make-vector new-capacity))
    (set-block-replace-cache-generations! cache (make-vector new-capacity -1)))
  (set-block-replace-cache-generation! cache (add1 (block-replace-cache-generation cache))))

(define (block-replace-subexpr block expr from to [can-refer #f] #:cache [cache #f])
  (set! cache (or cache (make-block-replace-cache block)))
  (prepare-block-replace-cache! cache block)
  (define from-idx (val-idx from))
  (define to-idx (val-idx to))
  (letrec
      ([loop (lambda (idx)
               (cond
                 [(< idx from-idx) idx]
                 [(= idx from-idx) to-idx]
                 [(and can-refer (not (set-member? can-refer idx))) idx]
                 [else
                  (define cached?
                    (= (vector-ref (block-replace-cache-generations cache) idx)
                       (block-replace-cache-generation cache)))
                  (if cached?
                      (vector-ref (block-replace-cache-values cache) idx)
                      (let* ([node (block-node block idx)]
                             [result
                              (cond
                                [(approx? node)
                                 (define impl (approx-impl node))
                                 (define impl* (loop impl))
                                 (if (= impl* impl)
                                     idx
                                     (val-idx (block-push! block (approx (approx-spec node) impl*))))]
                                [(pair? node)
                                 (define args* (replace-args (cdr node)))
                                 (if args*
                                     (val-idx (block-push! block (cons (car node) args*)))
                                     idx)]
                                [else idx])])
                        (vector-set! (block-replace-cache-generations cache)
                                     idx
                                     (block-replace-cache-generation cache))
                        (vector-set! (block-replace-cache-values cache) idx result)
                        result))]))]
       [replace-tail (lambda (args)
                       (if (null? args)
                           '()
                           (cons (loop (car args)) (replace-tail (cdr args)))))]
       [replace-args (lambda (args)
                       (cond
                         [(null? args) #f]
                         [else
                          (define arg (car args))
                          (define arg* (loop arg))
                          (if (= arg* arg)
                              (let ([rest* (replace-args (cdr args))]) (and rest* (cons arg rest*)))
                              (cons arg* (replace-tail (cdr args))))]))])
    (val block (loop (val-idx expr)))))

(module+ test
  (require rackunit)
  (check-equal? (replace-expression '(- x (sin x)) 'x 1) '(- 1 (sin 1)))

  (check-equal? (replace-expression '(/ (cos (* 2 x))
                                        (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
                                    'cos
                                    '(/ 1 cos))
                '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))
