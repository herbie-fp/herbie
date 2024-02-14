#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/rules.rkt"
         "../common.rkt"
         "../programs.rkt"
         "../timeline.rkt"
         "egg-herbie.rkt")

(provide pattern-match
         rewrite-expressions)

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

;;
;;  Non-recursive rewriter
;;

(define (rewrite-once expr ctx #:rules rules)
  ;; we want rules over representations
  (define canon (make-hash))
  (define rules*
    (for/fold ([rules* '()]) ([r (in-list rules)])
      (define impl-rules (rule->impl-rules r))
      (for ([r* (in-list impl-rules)])
        (hash-set! canon (rule-name r*) (rule-name r)))
      (append impl-rules rules*)))
  ;; actually match
  (define rule-apps (make-hash))
  (define expr-repr (repr-of expr ctx))
  (define changelists
    (reap [sow]
      (for ([rule rules*] #:when (equal? expr-repr (rule-otype rule)))
        (let* ([result (rule-apply rule expr)])
          (when result
            (define canon-name (hash-ref canon (rule-name rule)))
            (hash-update! rule-apps canon-name add1 1)
            (sow (list (car result) rule)))))))
  ;; rule statistics
  (for ([(name count) (in-hash rule-apps)])
    (when (> count 0) (timeline-push! 'rules (~a name) count)))
  changelists)

;;
;;  Egg recursive rewriter
;;
;; Fallback system
;;  batch-egg-rewrite - batched call to egg
;;  egg-rewrite - call to egg on an expression (skipped if batch-egg-rewrite called with 1 expr)
;;  egg-rewrite-iter-limit - call to egg on an expression with an iter limit (last resort)
;;
;;  Recursive rewrite chooser
(define (rewrite-expressions exprs
                             ctx
                             #:rules rules
                             #:depths [depths (make-list (length exprs) 1)]
                             #:once? [once? #f])
  ; choose correct rr driver
  (cond
   [(or (null? exprs) (null? rules)) (make-list (length exprs) '())]
   [(or once? (not (flag-set? 'generate 'rr)))
    (timeline-push! 'method "rewrite-once")
    (for/list ([expr exprs] [n (in-naturals 1)])
      (define timeline-stop! (timeline-start! 'times (~a expr)))
      (begin0 (rewrite-once expr ctx #:rules rules)
        (timeline-stop!)))]
   [else
    (timeline-push! 'method "batch-egg-rewrite")
    (timeline-push! 'inputs (map ~a exprs))
    (define e-input (make-egg-query exprs rules #:context ctx #:node-limit (*node-limit*)))
    (match-define (cons variantss _) (run-egg e-input #t))

    (define out
      (for/list ([variants variantss])
        (for/list ([variant (remove-duplicates variants)])
            (list variant e-input))))

    (timeline-push! 'outputs (map ~a (apply append variantss)))
    out]))

(module+ test
  (require rackunit)
  (require "../syntax/types.rkt" "../load-plugin.rkt")
  (load-herbie-builtins)

  (define repr (get-representation 'binary64))
  (*context* (make-debug-context '()))
  (*context* (context-extend (*context*) 'x repr))

  (let ([chngs (rewrite-once '(+.f64 x x) (*context*) #:rules (*rules*))])
    (check-equal? (length chngs) 14 (format "rewrites ~a" chngs)))

  (let ([chngs (rewrite-once '(*.f64 x x) (*context*) #:rules (*rules*))])
    (check-equal? (length chngs) 12 (format "rewrites ~a" chngs)))

  (void))
