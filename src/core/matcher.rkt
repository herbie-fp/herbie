#lang racket

(require egg-herbie)
(require "../common.rkt" "../programs.rkt" "../alternative.rkt"
         "../syntax/rules.rkt" "../interface.rkt" "../timeline.rkt" "simplify.rkt")

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
  (define (fail . irr) #f)
  (and binding1
       binding2
       (let/ec quit
         (for/fold ([binding binding1]) ([(k v) (in-dict binding2)])
           (dict-update binding k (λ (x) (if (equal? x v) v (quit #f))) v)))))

(define (pattern-match pattern expr)
  (define (fail . irr) #f)

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
;;  Non-recursive rewriter
;;

(define (rewrite-once expr repr #:rules rules #:root [root-loc '()] #:depth [depth 1])
  (define type (repr-of expr repr (*var-reprs*)))
  (reap [sow]
    (for ([rule rules] #:when (equal? type (rule-otype rule)))
      (let* ([result (rule-apply rule expr)])
        (when result
            (sow (list (change rule root-loc (cdr result)))))))))

;;
;;  Egg recursive rewriter
;;
;; Fallback system
;;  batch-egg-rewrite - batched call to egg
;;  egg-rewrite - call to egg on an expression (skipped if batch-egg-rewrite called with 1 expr)
;;  egg-rewrite-iter-limit - call to egg on an expression with an iter limit (last resort)
;;

(define (batch-egg-rewrite exprs
                           repr
                           #:rules rules
                           #:roots [root-locs (make-list (length exprs) '())]
                           #:depths [depths (make-list (length exprs) 1)])
  (define egg-rule (rule "egg-rr" 'x 'x (list repr) repr))
  (define irules (rules->irules rules))
  ; If unsoundness was detected, try running one epxression at a time.
  ; Can optionally set iter limit (will give up if unsoundness detected).
  (let loop ([exprs exprs] [root-locs root-locs] [iter-limit #f])
    ; Returns a procedure rather than the variants directly:
    ; if we need to fallback, we exit the `with-egraph` closure first
    ; so the existing egraph gets cleaned up
    (define result-thunk
      (with-egraph
        (λ (egg-graph)
          (egraph-add-exprs
            egg-graph
            exprs
            (λ (node-ids)
              (define iter-data (egg-run-rules egg-graph (*node-limit*) irules node-ids #t))
              (for ([rule rules])
                (define count (egraph-get-times-applied egg-graph (rule-name rule)))
                (when (> count 0) (timeline-push! 'rules (~a (rule-name rule)) count)))
              (cond
              [(egraph-is-unsound-detected egg-graph)
                ; unsoundness detected, fallback
                (match* (exprs iter-limit)
                 [((list e0 e1 es ...) #f)    ; run expressions individually
                  (λ ()
                    (for/list ([expr exprs] [root-loc root-locs])
                      (timeline-push! 'method "egg-rewrite")
                      (car (loop (list expr) (list root-loc) #f))))]
                 [((list e0) #f)              ; run expressions with iter limit
                  (λ ()
                    (let ([limit (- (length iter-data) 2)])
                      (timeline-push! 'method "egg-rewrite-iter-limit")
                      (loop exprs root-locs limit)))]
                 [(_ #t)                      ; give up
                  (λ () '())])]
              [else
                (define variants
                  (for/list ([id node-ids] [expr exprs] [root-loc root-locs])
                    (define output (egraph-get-variants egg-graph id expr))
                    (define extracted (egg-exprs->exprs output egg-graph))
                    (for/list ([variant (remove-duplicates extracted)])
                      (list (change egg-rule root-loc (list (cons 'x variant)))))))
                (λ () variants)]))))))
    (result-thunk)))

;;  Recursive rewrite chooser
(define (rewrite-expressions exprs
                             repr 
                             #:rules rules
                             #:roots [root-locs (make-list (length exprs) '())]
                             #:depths [depths (make-list (length exprs) 1)])
  ; choose correct rr driver
  (cond
   [(null? exprs) '()]
   [(flag-set? 'generate 'rr)
    (define driver batch-egg-rewrite)
    (timeline-push! 'method (~a (object-name driver)))
    (debug #:from 'progress #:depth 4 "batched rewriting for" exprs)
    (define tnow (current-inexact-milliseconds))
    (begin0 (driver exprs repr #:rules rules #:roots root-locs #:depths depths)
      (for ([expr exprs])
        (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow))))]
   [else
    (define driver rewrite-once)
    (timeline-push! 'method (~a (object-name driver)))
    (for/list ([expr exprs] [root-loc root-locs] [depth depths] [n (in-naturals 1)])
        (debug #:from 'progress #:depth 4 "[" n "/" (length exprs) "] rewriting for" expr)
        (define tnow (current-inexact-milliseconds))
        (begin0 (driver expr repr #:rules rules #:root root-loc #:depth depth)
          (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow))))]))
