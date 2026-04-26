#lang racket

(require math/bigfloat)
(require "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/batch.rkt"
         "egg-herbie.rkt"
         "points.rkt"
         "programs.rkt")

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

(define (has-periodic-impl? repr)
  (and (get-fpcore-impl '+ (repr->prop repr) (list repr repr))
       (get-fpcore-impl '* (repr->prop repr) (list repr repr))
       (get-fpcore-impl 'PI (repr->prop repr) '())
       (get-fpcore-impl 'rem2pi (repr->prop repr) (list repr))))

(define (has-exponential-impl? repr out-repr)
  (and (equal? repr out-repr)
       (equal? (representation-type repr) 'real)
       (get-fpcore-impl '* (repr->prop repr) (list repr repr))
       (get-fpcore-impl '- (repr->prop repr) (list repr repr))
       (get-fpcore-impl '/ (repr->prop repr) (list repr repr))
       (get-fpcore-impl 'log (repr->prop repr) (list repr))
       (get-fpcore-impl 'exp2 (repr->prop repr) (list repr))
       (get-fpcore-impl 'rint (repr->prop repr) (list repr))))

(define (make-periodic-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))]
             [repr (in-list (context-var-reprs ctx))]
             #:when (has-periodic-impl? repr))
    (cons `(periodic ,var) (replace-expression spec var `(+ ,var (* 2 (PI)))))))

(define (make-exponential-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))]
             [repr (in-list (context-var-reprs ctx))]
             #:when (has-exponential-impl? repr (context-repr ctx)))
    (cons `(exponential ,var) (replace-expression `(* 2 ,spec) var `(- ,var (log 2))))))

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
(define (make-sort-identities spec ctx)
  (define pairs (combinations (context-vars ctx) 2))
  (for/list ([pair (in-list pairs)]
             ;; Can only sort same-repr variables
             #:when (equal? (context-lookup ctx (first pair)) (context-lookup ctx (second pair)))
             #:when (has-fmin-fmax-impl? (context-lookup ctx (first pair))))
    (match-define (list a b) pair)
    (cons `(sort ,a ,b) (replace-vars `((,a . ,b) (,b . ,a)) spec))))

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing spec ctx)
  ;; identities
  (define identities
    (append (make-even-identities spec ctx)
            (make-odd-identities spec ctx)
            (make-sort-identities spec ctx)
            (make-periodic-identities spec ctx)
            (make-exponential-identities spec ctx)))
  ;; make egg runner
  (define-values (batch brfs) (progs->batch (cons spec (map cdr identities))))
  (define runner (make-egraph batch brfs (make-list (length brfs) (context-repr ctx)) '(rewrite) ctx))
  ;; collect equalities
  (for/list ([(ident spec*) (in-dict identities)]
             #:when (egraph-equal? runner spec spec*))
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
    [(list 'exponential variable)
     (define index (index-of variables variable))
     (define repr (context-lookup context variable))
     (define mul (impl-info (get-fpcore-impl '* (repr->prop repr) (list repr repr)) 'fl))
     (define sub (impl-info (get-fpcore-impl '- (repr->prop repr) (list repr repr)) 'fl))
     (define div (impl-info (get-fpcore-impl '/ (repr->prop repr) (list repr repr)) 'fl))
     (define exp2 (impl-info (get-fpcore-impl 'exp2 (repr->prop repr) (list repr)) 'fl))
     (define log (impl-info (get-fpcore-impl 'log (repr->prop repr) (list repr)) 'fl))
     (define rint (impl-info (get-fpcore-impl 'rint (repr->prop repr) (list repr)) 'fl))
     (define log2 (log ((representation-bf->repr repr) 2.bf)))
     (lambda (x y)
       (define val (vector-ref x index))
       (define k (rint (div val log2)))
       (define scale (exp2 k))
       (values (vector-update x index (lambda (_) (sub val (mul k log2)))) (div y scale)))]
    [(list 'periodic variable)
     (define index (index-of variables variable))
     (define var-repr (context-lookup context variable))
     (define rem2pi (impl-info (get-fpcore-impl 'rem2pi (repr->prop var-repr) (list var-repr)) 'fl))
     (lambda (x y) (values (vector-update x index (lambda (val) (rem2pi val))) y))]
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
    [(list 'exponential var)
     (define repr (context-lookup context var))
     (define mul (get-fpcore-impl '* (repr->prop repr) (list repr repr)))
     (define sub (get-fpcore-impl '- (repr->prop repr) (list repr repr)))
     (define div (get-fpcore-impl '/ (repr->prop repr) (list repr repr)))
     (define exp2 (get-fpcore-impl 'exp2 (repr->prop repr) (list repr)))
     (define log (get-fpcore-impl 'log (repr->prop repr) (list repr)))
     (define rint (get-fpcore-impl 'rint (repr->prop repr) (list repr)))
     (define log2 (list log (literal 2 (representation-name repr))))
     (define k (list rint (list div var log2)))
     (define reduced-var (list sub var (list mul k log2)))
     `(,mul (,exp2 ,k) ,(replace-expression expression var reduced-var))]
    [(list 'periodic var)
     (define repr (context-lookup context var))
     (define rem2pi (get-fpcore-impl 'rem2pi (repr->prop repr) (list repr)))
     (define replacement (list rem2pi var))
     (replace-expression expression var replacement)]
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
