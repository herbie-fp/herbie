#lang racket

(require math/bigfloat)
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
         remove-unnecessary-preprocessing
         compile-preprocessing)

(define (has-fabs-impl? repr)
  (get-fpcore-impl 'fabs (repr->prop repr) (list repr)))

(define (has-fmin-fmax-impl? repr)
  (and (get-fpcore-impl 'fmin (repr->prop repr) (list repr repr))
       (get-fpcore-impl 'fmax (repr->prop repr) (list repr repr))))

(define (has-copysign-impl? repr)
  (and (get-fpcore-impl '* (repr->prop repr) (list repr repr))
       (get-fpcore-impl 'copysign (repr->prop repr) (list repr repr))))

;; The even identities: f(x) = f(-x)
;; Requires `neg` and `fabs` operator implementations.
(define (make-even-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))]
             [repr (in-list (context-var-reprs ctx))]
             #:when (has-fabs-impl? repr))
    (cons `(abs ,var) (replace-expression spec var `(neg ,var)))))

;; The odd identities: f(x) = -f(-x)
;; Requires `neg` and `fabs` operator implementations.
(define (make-odd-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))]
             [repr (in-list (context-var-reprs ctx))]
             #:when (and (has-fabs-impl? repr) (has-copysign-impl? (context-repr ctx))))
    (cons `(negabs ,var) (replace-expression `(neg ,spec) var `(neg ,var)))))

;; Sort identities: f(a, b) = f(b, a)
;; TODO: require both vars have the same repr
(define (make-sort-identities spec ctx)
  (define pairs (combinations (context-vars ctx) 2))
  (for/list ([pair (in-list pairs)]
             ;; Can only sort same-repr variables
             #:when (equal? (context-lookup ctx (first pair)) (context-lookup ctx (second pair)))
             #:when (has-fmin-fmax-impl? (context-lookup ctx (first pair))))
    (match-define (list a b) pair)
    (cons `(sort ,a ,b) (replace-vars `((,a . ,b) (,b . ,a)) spec))))

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing expr ctx)
  (define spec (prog->spec expr))

  ;; identities
  (define even-identities (make-even-identities spec ctx))
  (define odd-identities (make-odd-identities spec ctx))
  (define sort-identities (make-sort-identities spec ctx))
  (define identities (append even-identities odd-identities sort-identities))

  ;; make egg runner
  (define rules (*sound-rules*))

  (define batch (progs->batch (cons spec (map cdr identities))))
  (define runner
    (make-egraph batch
                 (batch-roots batch)
                 (make-list (vector-length (batch-roots batch)) (context-repr ctx))
                 `((,rules . ((node . ,(*node-limit*)))))))

  ;; collect equalities
  (define abs-instrs
    (for/list ([(ident spec*) (in-dict even-identities)]
               #:when (egraph-equal? runner spec spec*))
      ident))

  (define negabs-instrs
    (for/list ([(ident spec*) (in-dict odd-identities)]
               #:when (egraph-equal? runner spec spec*))
      ident))

  (define sort-instrs
    (for/list ([(ident spec*) (in-dict sort-identities)]
               #:when (egraph-equal? runner spec spec*))
      ident))
  (append abs-instrs negabs-instrs sort-instrs))

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
     (timeline-push! 'remove-preprocessing (map ~a newly-removed))
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
     (replace-vars (list (cons a (list fmin a b)) (cons b (list fmax a b))) expression)]
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
