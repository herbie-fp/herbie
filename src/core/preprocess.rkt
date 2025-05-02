#lang racket

(require "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "../utils/float.rkt"
         "batch.rkt"
         "egglog-herbie.rkt"
         "../config.rkt"
         "egg-herbie.rkt"
         "points.rkt"
         "programs.rkt"
         "rules.rkt")

(provide find-preprocessing
         preprocess-pcontext
         remove-unnecessary-preprocessing)

(define (has-fabs-neg-impls? repr)
  (and (get-fpcore-impl '- (repr->prop repr) (list repr))
       (get-fpcore-impl 'fabs (repr->prop repr) (list repr))))

(define (has-copysign-impl? repr)
  (get-fpcore-impl 'copysign (repr->prop repr) (list repr repr)))

;; The even identities: f(x) = f(-x)
;; Requires `neg` and `fabs` operator implementations.
(define (make-even-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))]
             [repr (in-list (context-var-reprs ctx))]
             #:when (has-fabs-neg-impls? repr))
    (cons `(abs ,var) (replace-expression spec var `(neg ,var)))))

;; The odd identities: f(x) = -f(-x)
;; Requires `neg` and `fabs` operator implementations.
(define (make-odd-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))]
             [repr (in-list (context-var-reprs ctx))]
             #:when (and (has-fabs-neg-impls? repr) (has-copysign-impl? repr)))
    (cons `(negabs ,var) (replace-expression `(neg ,spec) var `(neg ,var)))))

;; Swap identities: f(a, b) = f(b, a)
(define (make-swap-identities spec ctx)
  (define pairs (combinations (context-vars ctx) 2))
  (for/list ([pair (in-list pairs)])
    (match-define (list a b) pair)
    (cons `(swap ,a ,b) (replace-vars `((,a . ,b) (,b . ,a)) spec))))

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing expr ctx)
  (define spec (prog->spec expr))

  ;; identities
  (define even-identities (make-even-identities spec ctx))
  (define odd-identities (make-odd-identities spec ctx))
  (define swap-identities (make-swap-identities spec ctx))
  (define identities (append even-identities odd-identities swap-identities))

  ;; make egg runner
  (define rules (*sound-rules*))

  (define batch (progs->batch (cons spec (map cdr identities))))
  (define runner
    (make-egraph batch
                 (batch-roots batch)
                 (make-list (vector-length (batch-roots batch)) (context-repr ctx))
                 `((,rules . ((node . ,(*node-limit*)) (scheduler . simple))))))

  ;; TODO : FIGURE HOW TO IMPLEMENT PREPROCESS

  ;; collect equalities
  (define abs-instrs
    (for/list ([(ident spec*) (in-dict even-identities)]
               #:when (egraph-equal? runner spec spec*))
      ident))

  (define negabs-instrs
    (for/list ([(ident spec*) (in-dict odd-identities)]
               #:when (egraph-equal? runner spec spec*))
      ident))

  (define swaps
    (for/list ([(ident spec*) (in-dict swap-identities)]
               #:when (egraph-equal? runner spec spec*))
      (match-define (list 'swap a b) ident)
      (list a b)))
  (define components (connected-components (context-vars ctx) swaps))
  (define sort-instrs
    (for/list ([component (in-list components)]
               #:when (> (length component) 1))
      (cons 'sort component)))

  (append abs-instrs negabs-instrs sort-instrs))

(define (connected-components variables swaps)
  (define components (disjoint-set (length variables)))
  (for ([swap (in-list swaps)])
    (match-define (list a b) swap)
    (disjoint-set-union! components
                         (disjoint-set-find! components (index-of variables a))
                         (disjoint-set-find! components (index-of variables b))))
  (group-by (compose (curry disjoint-set-find! components) (curry index-of variables)) variables))

(define (preprocess-pcontext context pcontext preprocessing)
  (define preprocess
    (apply compose
           (map (curry instruction->operator context)
                ;; Function composition applies the rightmost function first
                (reverse preprocessing))))
  (for/pcontext ([(x y) pcontext]) (preprocess x y)))

(define (vector-update v i f)
  (define copy (make-vector (vector-length v)))
  (vector-copy! copy 0 v)
  (vector-set! copy i (f (vector-ref copy i)))
  copy)

(define (vector-set* v indices vals)
  (define copy (make-vector (vector-length v)))
  (vector-copy! copy 0 v)
  (for ([i (in-list indices)]
        [v (in-list vals)])
    (vector-set! copy i v))
  copy)

(define (instruction->operator context instruction)
  (define variables (context-vars context))
  (define sort* (curryr sort (curryr </total (context-repr context))))
  (match instruction
    [(list 'sort component ...)
     (unless (subsequence? component variables)
       (error 'instruction->operator "component should always be a subsequence of variables"))
     (define indices (indexes-where variables (curryr member component)))
     (lambda (x y)
       (define subsequence (map (curry vector-ref x) indices))
       (define sorted (sort* subsequence))
       (values (vector-set* x indices sorted) y))]
    [(list 'abs variable)
     (define index (index-of variables variable))
     (define var-repr (context-lookup context variable))
     (define abs-proc (impl-info (get-fpcore-impl 'fabs (repr->prop var-repr) (list var-repr)) 'fl))
     (lambda (x y) (values (vector-update x index abs-proc) y))]
    [(list 'negabs variable)
     (define index (index-of variables variable))
     (define var-repr (context-lookup context variable))
     (define neg-var (impl-info (get-fpcore-impl '- (repr->prop var-repr) (list var-repr)) 'fl))

     (define repr (context-repr context))
     (define neg-expr (impl-info (get-fpcore-impl '- (repr->prop repr) (list repr)) 'fl))

     (lambda (x y)
       ;; Negation is involutive, i.e. it is its own inverse, so t^1(y') = -y'
       (if (negative? (repr->real (vector-ref x index) (context-repr context)))
           (values (vector-update x index neg-var) (neg-expr y))
           (values x y)))]))

; until fixed point, iterate through preprocessing attempting to drop preprocessing with no effect on error
(define (remove-unnecessary-preprocessing expression
                                          context
                                          pcontext
                                          preprocessing
                                          #:removed [removed empty])
  (define-values (result newly-removed)
    (let loop ([preprocessing preprocessing]
               [i 0]
               [removed removed])
      (cond
        [(>= i (length preprocessing)) (values preprocessing removed)]
        [(preprocessing-<=? expression context pcontext (drop-at preprocessing i) preprocessing)
         (loop (drop-at preprocessing i) i (cons (list-ref preprocessing i) removed))]
        [else (loop preprocessing (+ i 1) removed)])))
  (cond
    [(< (length result) (length preprocessing))
     (remove-unnecessary-preprocessing expression context pcontext result #:removed newly-removed)]
    [else
     (timeline-push! 'remove-preprocessing (map ~a newly-removed))
     result]))

(define (preprocessing-<=? expression context pcontext preprocessing1 preprocessing2)
  (define pcontext1 (preprocess-pcontext context pcontext preprocessing1))
  (define pcontext2 (preprocess-pcontext context pcontext preprocessing2))
  (<= (errors-score (errors expression pcontext1 context))
      (errors-score (errors expression pcontext2 context))))
