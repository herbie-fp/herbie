#lang racket

(require "../utils/float.rkt"
         "../syntax/types.rkt"
         "batch.rkt"
         "compiler.rkt")

(provide *pcontext*
         in-pcontext
         mk-pcontext
         for/pcontext
         pcontext?
         pcontext-points
         split-pcontext
         pcontext-length
         errors
         batch-errors
         errors-score)

;; pcontexts are Herbie's standard data structure for storing
;; ground-truth information. They contain 1) a set of sampled input
;; points; and 2) a ground-truth output for each input.

(define *pcontext* (make-parameter #f))
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
  (first (batch-errors (list expr) pcontext ctx)))

(define (batch-errors exprs pcontext ctx)
  (define fn (compile-progs exprs ctx))
  (define repr (context-repr ctx))
  (define special? (representation-special-value? repr))
  (define max-error (+ 1 (expt 2 (representation-total-bits repr))))

  ;; This generates the errors array in reverse because that's how lists work
  (define num-exprs
    (if (batch? exprs)
        (vector-length (batch-roots exprs))
        (length exprs)))
  (define num-points (pcontext-length pcontext))
  (for/fold ([result (make-list num-exprs '())])
            ([point (in-vector (pcontext-points pcontext) (- num-points 1) -1 -1)]
             [exact (in-vector (pcontext-exacts pcontext) (- num-points 1) -1 -1)])
    (for/list ([out (in-vector (fn point))]
               [rest (in-list result)])
      (cons (if (special? out)
                max-error
                (ulp-difference out exact repr))
            rest))))
