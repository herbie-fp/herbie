#lang racket

(require "config.rkt" "float.rkt" "syntax/types.rkt" "programs.rkt")

(provide *pcontext* in-pcontext mk-pcontext for/pcontext pcontext? split-pcontext join-pcontext
         errors batch-errors errors-score oracle-error baseline-error oracle-error-idx)

;; pcontexts are Herbie's standard data structure for storing
;; ground-truth information. They contain 1) a set of sampled input
;; points; and 2) a ground-truth output for each input.

(define *pcontext* (make-parameter #f))
(struct pcontext (points exacts))

(define (in-pcontext context)
  (in-parallel (in-vector (pcontext-points context)) (in-vector (pcontext-exacts context))))

(define/contract (mk-pcontext points exacts)
  (-> (non-empty-listof (listof any/c)) (non-empty-listof any/c) pcontext?)
  (pcontext (list->vector points) (list->vector exacts)))

(define-syntax-rule (for/pcontext ([(pt ex) pcontext] other ...) body ...)
  (let-values ([(pts* exs*)
                (for/lists (pts* exs*) ([(pt ex) (in-pcontext pcontext)] other ...)
                  body ...)])
    (mk-pcontext pts* exs*)))

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

(define (eval-errors eval-fn pcontext repr)
  (for/list ([(point exact) (in-pcontext pcontext)])
    (point-error (apply eval-fn point) exact repr)))

(define (oracle-error-idx alt-bodies points exacts repr)
  (for/list ([point points] [exact exacts])
    (list point (argmin (λ (i) (point-error ((list-ref alt-bodies i) point) exact repr)) (range (length alt-bodies))))))

(define (oracle-error alt-bodies pcontext repr)
  (for/list ([(point exact) (in-pcontext pcontext)])
    (argmin identity (map (λ (alt) (point-error (apply alt point) exact repr)) alt-bodies))))

(define (baseline-error alt-bodies pcontext newpcontext repr)
  (define baseline (argmin (λ (alt) (errors-score (eval-errors alt pcontext repr))) alt-bodies))
  (eval-errors baseline newpcontext repr))

(define (average . s)
  (/ (apply + s) (length s)))

(define (errors-score e)
  (apply (if (flag-set? 'reduce 'avg-error) average max) (map ulps->bits e)))

(define (errors prog pcontext repr)
  (define fn (eval-prog prog 'fl repr))
  (for/list ([(point exact) (in-pcontext pcontext)])
    (with-handlers ([exn:fail? (λ (e) (eprintf "Error when evaluating ~a on ~a\n" prog point) (raise e))])
      (point-error (apply fn point) exact repr))))

(define (batch-errors progs pcontext repr)
  (define fn (batch-eval-progs progs 'fl repr))
  (for/list ([(point exact) (in-pcontext pcontext)])
    (with-handlers ([exn:fail? (λ (e) (eprintf "Error when evaluating ~a on ~a\n" progs point) (raise e))])
      (for/vector ([out (in-vector (apply fn point))])
        (point-error out exact repr)))))
