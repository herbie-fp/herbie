#lang racket

(require "../syntax/types.rkt" "../syntax/syntax.rkt" "../syntax/rules.rkt")
(require "../common.rkt" "../programs.rkt" "../alternative.rkt"
         "../timeline.rkt" "../egglog/egraph-conversion.rkt" "../egglog/run-egglog.rkt")

(provide pattern-match rewrite-expressions change-apply)

;;; Our own pattern matcher.
;;
;; The racket (match) macro doesn't give us access to the bindings made
;; by the matcher, so we wrote our own.
;;
;; The syntax is simple:
;;   numbers are literals ; symbols are variables ; lists are expressions
;;
;; Bindings are stored as association lists

(define (merge-bindings binding1 binding2)
  (and binding1
       binding2
       (let/ec quit
         (for/fold ([binding binding1]) ([(k v) (in-dict binding2)])
           (dict-update binding k (λ (x) (if (equal? x v) v (quit #f))) v)))))

(define (pattern-match pattern expr)
  (match pattern
   [(? number?)
    (and (equal? pattern expr) '())]
   [(? variable?)
    (list (cons pattern expr))]
   [(list phead _ ...)
    (and (list? expr)
         (equal? (car expr) phead)
         (= (length expr) (length pattern))
         (for/fold ([bindings '()])
             ([pat (cdr pattern)] [subterm (cdr expr)])
           (merge-bindings bindings (pattern-match pat subterm))))]))

(define (pattern-substitute pattern bindings)
  ; pattern binding -> expr
  (match pattern
   [(? number?) pattern]
   [(? variable?)
    (dict-ref bindings pattern)]
   [(list phead pargs ...)
    (cons phead (map (curryr pattern-substitute bindings) pargs))]))

;; Random helper functions

(define (rule-apply rule expr)
  (let ([bindings (pattern-match (rule-input rule) expr)])
    (if bindings
        (cons (pattern-substitute (rule-output rule) bindings) bindings)
        #f)))

(define (change-apply cng prog)
  (match-define (change rule location bindings) cng)
  (location-do location prog (const (pattern-substitute (rule-output rule) bindings))))


;;
;;  Egg recursive rewriter
;;
;; Fallback system
;;  batch-egg-rewrite - batched call to egg
;;  egg-rewrite - call to egg on an expression (skipped if batch-egg-rewrite called with 1 expr)
;;  egg-rewrite-iter-limit - call to egg on an expression with an iter limit (last resort)
;;

(define (batch-egg-rewrite exprs
                           ctx
                           pctx)
  (for/list
      ([variants (run-egglog ctx pctx exprs 50)]
       [expr exprs])
    (cons expr variants)))

;;  Recursive rewrite chooser
(define (rewrite-expressions exprs
                             ctx
                             pctx)
  (timeline-push! 'method "batch-egg-rewrite")
  (timeline-push! 'inputs (map ~a exprs))
  (define out (batch-egg-rewrite exprs ctx pctx))
  (timeline-push! 'outputs (map ~a out))
  out)
