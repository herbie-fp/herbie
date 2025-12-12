#lang racket

(require "../utils/float.rkt"
         "../syntax/types.rkt"
         "batch.rkt"
         "compiler.rkt")

(provide in-pcontext
         mk-pcontext
         for/pcontext
         pcontext?
         pcontext-points
         split-pcontext
         pcontext-length
         errors
         batchref-errors
         batch-errors
         exprs-errors
         errors-score)

;; pcontexts are Herbie's standard data structure for storing
;; ground-truth information. They contain 1) a set of sampled input
;; points; and 2) a ground-truth output for each input.

(struct pcontext (points exacts) #:prefab)

(define (in-pcontext pcontext)
  (in-parallel (in-vector (pcontext-points pcontext)) (in-vector (pcontext-exacts pcontext))))

(define (pcontext-length pcontext)
  (vector-length (pcontext-points pcontext)))

(define/contract (mk-pcontext points exacts)
  (-> (non-empty-listof vector?) (non-empty-listof any/c) pcontext?)
  (pcontext (list->vector points) (list->vector exacts)))

(define-syntax-rule (for/pcontext ([(pt ex) pcontext] other ...) body ...)
  (let-values ([(pts* exs*)
                (for/lists (pts* exs*) ([(pt ex) (in-pcontext pcontext)] other ...) body ...)])
    (mk-pcontext pts* exs*)))

(define (split-pcontext pctx num-a num-b)
  (match-define (pcontext pts exs) pctx)
  (unless (= (+ num-a num-b) (vector-length pts))
    (error 'split-pcontext
           "Cannot split pcontext of size ~a into ~a and ~a"
           (vector-length pts)
           num-a
           num-b))
  (define-values (pts-a pts-b) (vector-split-at pts num-a))
  (define-values (exs-a exs-b) (vector-split-at exs num-a))
  (values (pcontext pts-a exs-a) (pcontext pts-b exs-b)))

;; Herbie's standard error measure is the average bits of error across
;; all points in a pcontext.

(define (average . s)
  (/ (apply + s) (length s)))

(define (errors-score e)
  (apply average (map ulps->bits e)))

(define (errors expr pcontext ctx)
  (first (exprs-errors (list expr) pcontext ctx)))

(define (batchref-errors brf pcontext ctx)
  (first (batch-errors (batchref-batch brf) (list brf) pcontext ctx)))

(define (exprs-errors exprs pcontext ctx)
  (define fn (compile-progs exprs ctx))
  (define num-exprs (length exprs))
  (generate-errors fn pcontext ctx num-exprs))

(define (batch-errors batch brfs pcontext ctx)
  (define fn (compile-batch batch brfs ctx))
  (define num-exprs (length brfs))
  (generate-errors fn pcontext ctx num-exprs))

(define (generate-errors fn pcontext ctx num-exprs)
  (define repr (context-repr ctx))

  (define (flatten-value val repr)
    (match (representation-type repr)
      ['array
       (define elem-repr (array-representation-elem repr))
       (define elems
         (cond
           [(vector? val) (vector->list val)]
           [(list? val) val]
           [else (list val)]))
       (apply append (map (λ (v) (flatten-value v elem-repr)) elems))]
      [_ (list (cons val repr))]))

  ;; This generates the errors array in reverse because that's how lists work
  (define num-points (pcontext-length pcontext))
  (for/fold ([result (make-list num-exprs '())])
            ([point (in-vector (pcontext-points pcontext) (- num-points 1) -1 -1)]
             [exact (in-vector (pcontext-exacts pcontext) (- num-points 1) -1 -1)])
    (for/list ([out (in-vector (fn point))]
               [rest (in-list result)])
      (define flat-out (flatten-value out repr))
      (define flat-exact (flatten-value exact repr))
      (unless (= (length flat-out) (length flat-exact))
        (error 'generate-errors
               "Mismatched array sizes: ~a vs ~a"
               (length flat-out)
               (length flat-exact)))
      (define elem-errors
        (for/list ([out* (in-list flat-out)]
                   [exact* (in-list flat-exact)])
          (match-define (cons out-val out-repr) out*)
          (match-define (cons exact-val exact-repr) exact*)
          (define repr* out-repr)
          (define special? (representation-special-value? repr*))
          (define max-error (+ 1 (expt 2 (representation-total-bits repr*))))
          (if (special? out-val)
              max-error
              (ulp-difference out-val exact-val repr*))))
      (append elem-errors rest))))
