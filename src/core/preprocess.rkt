#lang racket

(require math/bigfloat)
(require "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/block.rkt"
         "egg-herbie.rkt"
         "points.rkt"
         "programs.rkt"
         "rules.rkt")

(provide find-preprocessing
         preprocess-pcontext
         remove-unnecessary-preprocessing
         compile-preprocessing
         compile-useful-preprocessing)

(define (has-fabs-impl? repr)
  (get-fpcore-impl 'fabs (repr->prop repr) (list repr)))

(define (has-fmin-fmax-impl? repr)
  (and (get-fpcore-impl 'fmin (repr->prop repr) (list repr repr))
       (get-fpcore-impl 'fmax (repr->prop repr) (list repr repr))))

(define (has-copysign-impl? repr)
  (and (get-fpcore-impl '* (repr->prop repr) (list repr repr))
       (get-fpcore-impl 'copysign (repr->prop repr) (list repr repr))))

(define (block-replace-vars! block replacements)
  (block-recurse block
                 (lambda (v recurse)
                   (dict-ref replacements
                             v
                             (lambda ()
                               (block-push! block
                                            (expr-recurse (val-def v) (compose val-idx recurse))))))))

;; The even identities: f(x) = f(-x)
;; Requires `neg` and `fabs` operator implementations.
(define (make-even-identities block spec-v output-repr)
  (for/list ([var (in-list (block-vars block))]
             [repr (in-list (block-var-reprs block))]
             #:when (has-fabs-impl? repr))
    (define var-v (block-add! block var))
    (define neg-var-v (block-add! block `(neg ,var-v)))
    (define replace-neg ((block-replace-vars! block `((,var-v . ,neg-var-v))) spec-v))
    (cons `(abs ,var) replace-neg)))

;; The odd identities: f(x) = -f(-x)
;; Requires `neg` and `fabs` operator implementations.
(define (make-odd-identities block spec-v output-repr)
  (for/list ([var (in-list (block-vars block))]
             [repr (in-list (block-var-reprs block))]
             #:when (and (has-fabs-impl? repr) (has-copysign-impl? output-repr)))
    (define neg-spec-v (block-add! block `(neg ,spec-v)))
    (define var-v (block-add! block var))
    (define neg-var-v (block-add! block `(neg ,var-v)))
    (define replace-neg ((block-replace-vars! block `((,var-v . ,neg-var-v))) neg-spec-v))
    (cons `(negabs ,var) replace-neg)))

;; Sort identities: f(a, b) = f(b, a)
(define (make-sort-identities block spec-v output-repr)
  (define pairs (combinations (block-vars block) 2))
  (define reprs (map cons (block-vars block) (block-var-reprs block)))
  (for/list ([pair (in-list pairs)]
             ;; Can only sort same-repr variables
             #:when (equal? (dict-ref reprs (first pair)) (dict-ref reprs (second pair)))
             #:when (has-fmin-fmax-impl? (dict-ref reprs (first pair))))
    (match-define (list a b) pair)
    (define a-v (block-add! block a))
    (define b-v (block-add! block b))
    (define sorted-spec-v ((block-replace-vars! block `((,a-v . ,b-v) (,b-v . ,a-v))) spec-v))
    (cons `(sort ,a ,b) sorted-spec-v)))

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing block spec-v ctx)
  (define repr (context-repr ctx))

  ;; identities
  (define identities
    (append (make-even-identities block spec-v repr)
            (make-odd-identities block spec-v repr)
            (make-sort-identities block spec-v repr)))

  ;; make egg runner
  (define vs (cons spec-v (map cdr identities)))
  (define runner (make-egraph block vs '(rewrite) ctx))

  ;; collect equalities
  (for/list ([(ident _) (in-dict identities)]
             [idx (in-naturals 1)]
             #:when (egraph-roots-equal? runner 0 idx))
    ident))

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
  (match instruction
    [(list 'sort a b)
     (define indices (indexes-where variables (curry set-member? (list a b))))
     (define repr (context-lookup context a))
     (lambda (x y)
       (define subsequence (map (curry vector-ref x) indices))
       (define sorted (sort subsequence (curryr </total repr)))
       (values (vector-set* x indices sorted) y))]
    [(list 'abs variable)
     (define index (index-of variables variable))
     (define var-repr (context-lookup context variable))
     (define fabs (impl-info (get-fpcore-impl 'fabs (repr->prop var-repr) (list var-repr)) 'fl))
     (lambda (x y) (values (vector-update x index fabs) y))]
    [(list 'negabs variable)
     (define index (index-of variables variable))
     (define var-repr (context-lookup context variable))
     (define repr (context-repr context))
     (define fabs (impl-info (get-fpcore-impl 'fabs (repr->prop var-repr) (list var-repr)) 'fl))
     (define mul (impl-info (get-fpcore-impl '* (repr->prop repr) (list repr repr)) 'fl))
     (define copysign (impl-info (get-fpcore-impl 'copysign (repr->prop repr) (list repr repr)) 'fl))
     (define repr1 ((representation-bf->repr repr) 1.bf))
     (lambda (x y)
       (values (vector-update x index fabs) (mul (copysign repr1 (vector-ref x index)) y)))]))

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
     (timeline-push! 'symmetry (map ~a result))
     result]))

(define (preprocessing-<=? expression context pcontext preprocessing1 preprocessing2)
  (define pcontext1 (preprocess-pcontext context pcontext preprocessing1))
  (define pcontext2 (preprocess-pcontext context pcontext preprocessing2))
  (<= (errors-score (errors expression pcontext1 context))
      (errors-score (errors expression pcontext2 context))))

(define (compile-preprocessing expression context preprocessing)
  (match preprocessing
    ; Not handled yet
    [(list 'sort a b)
     (define repr (context-lookup context a))
     (define fmin (get-fpcore-impl 'fmin (repr->prop repr) (list repr repr)))
     (define fmax (get-fpcore-impl 'fmax (repr->prop repr) (list repr repr)))
     (replace-vars (list (list a fmin a b) (list b fmax a b)) expression)]
    [(list 'abs var)
     (define repr (context-lookup context var))
     (define fabs (get-fpcore-impl 'fabs (repr->prop repr) (list repr)))
     (define replacement (list fabs var))
     (replace-expression expression var replacement)]
    [(list 'negabs var)
     (define repr (context-lookup context var))
     (define fabs (get-fpcore-impl 'fabs (repr->prop repr) (list repr)))
     (define replacement (list fabs var))
     (define mul (get-fpcore-impl '* (repr->prop repr) (list repr repr)))
     (define copysign (get-fpcore-impl 'copysign (repr->prop repr) (list repr repr)))
     `(,mul (,copysign ,(literal 1 (representation-name repr)) ,var)
            ,(replace-expression expression var replacement))]))

(define (compile-useful-preprocessing expression context pcontext preprocessing)
  (define useful-preprocessing
    (remove-unnecessary-preprocessing expression context pcontext preprocessing))
  (for/fold ([expr expression]) ([prep (in-list (reverse useful-preprocessing))])
    (compile-preprocessing expr context prep)))
