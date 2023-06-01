#lang racket

(require "config.rkt" "common.rkt" "float.rkt" "syntax/types.rkt" "programs.rkt")

(provide *pcontext* in-pcontext mk-pcontext for/pcontext
         pcontext? pcontext->lists json->pcontext pcontext->json
         split-pcontext join-pcontext pcontext-length
         errors batch-errors errors-score)

;; pcontexts are Herbie's standard data structure for storing
;; ground-truth information. They contain 1) a set of sampled input
;; points; and 2) a ground-truth output for each input.

(define *pcontext* (make-parameter #f))
(struct pcontext (points exacts))

(define (in-pcontext context)
  (in-parallel (in-vector (pcontext-points context)) (in-vector (pcontext-exacts context))))

(define (pcontext-length context)
  (vector-length (pcontext-points context)))

(define/contract (mk-pcontext points exacts)
  (-> (non-empty-listof (listof any/c)) (non-empty-listof any/c) pcontext?)
  (pcontext (list->vector points) (list->vector exacts)))

(define-syntax-rule (for/pcontext ([(pt ex) pcontext] other ...) body ...)
  (let-values ([(pts* exs*)
                (for/lists (pts* exs*) ([(pt ex) (in-pcontext pcontext)] other ...)
                  body ...)])
    (mk-pcontext pts* exs*)))

(define (pcontext->lists context)
  (for/lists (pts exs) ([(pt ex) (in-pcontext context)])
    (values pt ex)))

(define (split-pcontext context num-a num-b)
  (define num-total (vector-length (pcontext-points context)))
  (unless (= (+ num-a num-b) num-total)
    (error 'split-pcontext "Cannot split pcontext of size ~a into ~a and ~a"
           num-total num-a num-b))
  (match-define (pcontext pts exs) context)
  (define-values (pts-a pts-b) (vector-split-at pts num-a))
  (define-values (exs-a exs-b) (vector-split-at exs num-a))
  (values (pcontext pts-a exs-a) (pcontext pts-b exs-b)))

(define (join-pcontext . ctxs)
  (pcontext
   (apply vector-append (map pcontext-points ctxs))
   (apply vector-append (map pcontext-exacts ctxs))))

;; Herbie's standard error measure is the average bits of error across
;; all points in a pcontext.

(define (point-error out exact repr)
  (if ((representation-special-value? repr) out)
      (+ 1 (expt 2 (representation-total-bits repr)))
      (ulp-difference out exact repr)))

(define (average . s)
  (/ (apply + s) (length s)))

(define (errors-score e)
  (apply (if (flag-set? 'reduce 'avg-error) average max) (map ulps->bits e)))

(define (errors expr pcontext ctx)
  (map first (batch-errors (list expr) pcontext ctx)))

(define ((batch-errors-handler exprs point) e)
  (raise e)
  (eprintf "Error during batch-errors with exprs:\n")
  (for ([expr (in-list exprs)])
    (eprintf "  ~a\n" expr))
  (eprintf "on point ~a\n" point)
  (raise e))

(define (batch-errors exprs pcontext ctx)
  (define fn (batch-eval-progs exprs 'fl ctx))
  (for/list ([(point exact) (in-pcontext pcontext)])
    (with-handlers ([exn:fail? (batch-errors-handler exprs point)])
      (for/list ([out (in-list (apply fn point))])
        (point-error out exact (context-repr ctx))))))

;; Herbie <=> JSON conversion for pcontext
;; A JSON pcontext is just a list of lists
;; ((pt1 ex1) (pt2 ex2) ...)

(define (json->pcontext json ctx)
  (define output-repr (context-repr ctx))
  (define var-reprs (context-var-reprs ctx))
  (define-values (pts exs)
    (for/lists (pts exs) ([entry (in-list json)])
      (match-define (list pt ex) entry)
      (values (map real->repr pt var-reprs) (real->repr ex output-repr))))
  (mk-pcontext pts exs))

(define (pcontext->json pcontext)
  (for/list ([(pt ex) (in-pcontext pcontext)])
    (list pt ex)))
