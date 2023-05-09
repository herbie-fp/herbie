#lang racket

(require "../syntax/types.rkt" "../syntax/syntax.rkt" "../syntax/rules.rkt")
(require "../common.rkt" "../programs.rkt" "../alternative.rkt" "egg-herbie.rkt"
         "../timeline.rkt")

(provide pattern-match rewrite-expressions)

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
  (match-define (list rules* _ canon-names) (expand-rules rules))
  (define rule-apps (make-hash))
  ;; actually match
  (define expr-repr (repr-of expr ctx))
  (define changelists
    (reap [sow]
      (for ([rule rules*] #:when (equal? expr-repr (rule-otype rule)))
        (let* ([result (rule-apply rule expr)])
          (when result
            (define canon-name (hash-ref canon-names (rule-name rule)))
            (hash-update! rule-apps canon-name (curry + 1) 1)
            (sow (car result)))))))
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

(define (batch-egg-rewrite exprs
                           ctx
                           #:rules rules
                           #:depths [depths (make-list (length exprs) 1)])
  (define reprs (map (λ (e) (repr-of e ctx)) exprs))
  ; If unsoundness was detected, try running one epxression at a time.
  ; Can optionally set iter limit (will give up if unsoundness detected).
  (let loop ([exprs exprs] [iter-limit #f])
    ; Returns a procedure rather than the variants directly:
    ; if we need to fallback, we exit the `with-egraph` closure first
    ; so the existing egraph gets cleaned up
    
    (define egg-graph (make-egraph))
    (define node-ids (map (curry egraph-add-expr egg-graph) exprs))
    (define iter-data (egraph-run-rules egg-graph #:limit iter-limit (*node-limit*) rules node-ids #t))
    (cond
      [(egraph-is-unsound-detected egg-graph)
       ; unsoundness detected, fallback
       (match* (exprs iter-limit)
         [((list (? list?) (? list?) (? list?) ...) #f)     ; run expressions individually
          (set! egg-graph #f)                               ; allow old egraph to be GC'd
          (for/list ([expr exprs])
            (timeline-push! 'method "egg-rewrite")
            (car (loop (list expr) #f)))]
         [((list (? list?)) #f)                             ; run expressions with iter limit
          (set! egg-graph #f)                               ; allow old egraph to be GC'd
          (define limit (- (length iter-data) 2))
          (timeline-push! 'method "egg-rewrite-iter-limit")
          (loop exprs limit)]
         [(_ (? number?))                                   ; give up
          (timeline-push! 'method "egg-rewrite-fail")
          '(())])]
      [else
       (for/list ([id node-ids] [expr exprs] [expr-repr reprs])
         (define egg-rule (rule "egg-rr" 'x 'x (list expr-repr) expr-repr))
         (define output (egraph-get-variants egg-graph id expr))
         (remove-duplicates output))])))
    
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
    (define out (batch-egg-rewrite exprs ctx #:rules rules #:depths depths))
    (timeline-push! 'outputs (map ~a out))
    out]))
