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

; Index inside (batch-nodes batch) -> type
(define (repr-of-node batch idx ctx)
  (define node (batch-ref batch idx))
  (match node
    [(literal val precision) (get-representation precision)]
    [(? symbol?) (context-lookup ctx node)]
    [(approx _ impl) (repr-of-node batch impl ctx)]
    [(hole precision spec) (get-representation precision)]
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
    [((approx spec impl) (cons idx rest)) (approx spec (location-do rest impl f))]
    [((hole prec spec) (cons 1 rest)) (hole prec (location-do rest spec f))]
    [((? list?) (cons idx rest)) (list-set prog idx (location-do rest (list-ref prog idx) f))]))

(define/contract (location-set loc prog prog*)
  (-> location? expr? expr? expr?)
  (location-do loc prog (const prog*)))

(define/contract (batch-location-set loc0 full-batchref sub-batchref)
  (-> location? batchref? batchref? batchref?)
  (match-define (batchref sub-batch sub-idx) sub-batchref)
  (match-define (batchref full-batch full-idx) full-batchref)

  (unless (equal? sub-batch full-batch)
    (error 'batch-location-set "Function assumes that batches are equal"))

  (define idx*
    (let loop ([loc0 loc0]
               [idx full-idx])
      (let ([node (batch-ref full-batch idx)])
        (match* (node loc0)
          [(_ (? null?)) sub-idx]
          [((approx spec impl) (cons 1 rest)) (batch-push! full-batch (approx (loop rest spec) impl))]
          [((approx spec impl) (cons 2 rest)) (batch-push! full-batch (approx spec (loop rest impl)))]
          [((hole prec spec) (cons 1 rest)) (batch-push! full-batch (hole prec (loop rest spec)))]
          [((list op args ...) (cons loc rest))
           (define args* (list-update args (sub1 loc) (curry loop rest)))
           (batch-push! full-batch (cons op args*))]))))
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

  (define (locations-update locations prev-idx new-loc new-idx)
    (define prev-locs (vector-ref locations prev-idx))
    (unless (null? prev-locs) ; when prev-idx has some locs stored
      (define new-locs (map (curry cons new-loc) prev-locs)) ; append prev-locs with new-loc
      (vector-set! locations
                   new-idx
                   (append (vector-ref locations new-idx) new-locs)))) ; update new-locs at new-idx

  (cond
    [(> sub-idx full-idx)
     '()] ; sub-idx can not be a child of full-idx if it is inserted after full-idx
    [else
     (define locations (make-vector (batch-length full-batch) '()))
     (vector-set! locations sub-idx '(()))
     (for ([node (in-batch full-batch (add1 sub-idx) (add1 full-idx))]
           [n (in-naturals (add1 sub-idx))])
       (match node
         [(list _ args ...)
          (for ([arg (in-list args)]
                [i (in-naturals 1)])
            (locations-update locations arg i n))]
         [(approx _ impl) (locations-update locations impl 2 n)]
         [(hole _ spec) (locations-update locations spec 1 n)]
         [_ void])) ; literal/number/symbol
     (vector-ref locations full-idx)]))

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
