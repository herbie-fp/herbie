#lang racket

(require racket/lazy-require racket/hash)
(require "../common.rkt" "../programs.rkt" "../alternative.rkt"
         "../syntax/rules.rkt" "../interface.rkt" "../timeline.rkt"
         "../errors.rkt" "simplify.rkt")

(provide
  pattern-match
  pattern-substitute
  rewrite-expressions
  change-apply
  rule-apply)

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

;;  Depth 1 rewriter

(define (rewrite-once expr repr #:rules rules #:root [root-loc '()] #:depth [depth 1])
  (define type (repr-of expr repr (*var-reprs*)))
  (reap [sow]
    (for ([rule rules] #:when (equal? type (rule-otype rule)))
      (let* ([result (rule-apply rule expr)])
        (when result
            (sow (list (change rule root-loc (cdr result)))))))))

;;  Recursive rewriter

(define (recursive-rewrite expr repr #:rules rules #:root [root-loc '()] #:depth [depth 1])
  (define type (repr-of expr repr (*var-reprs*)))
  (define (rewriter sow expr ghead glen loc cdepth)
    ; expr _ _ _ _ -> (list (list change))
    (for ([rule rules] #:when (equal? type (rule-otype rule)))
      (when (or
             (not ghead) ; Any results work for me
             (and
              (list? (rule-output rule))
              (= (length (rule-output rule)) glen)
              (eq? (car (rule-output rule)) ghead)))
        (for ([option (matcher* expr (rule-input rule) loc (- cdepth 1))])
          ;; Each option is a list of change lists
          (sow (cons (change rule (reverse loc) (cdr option)) (car option)))))))

  (define (reduce-children sow options)
    ; (list (list ((list change) * bindings)))
    ; -> (list ((list change) * bindings))
    (for ([children options])
      (let ([bindings* (foldl merge-bindings '() (map cdr children))])
        (when bindings*
          (sow (cons (apply append (map car children)) bindings*))))))

  (define (fix-up-variables sow pattern cngs)
    ; pattern (list change) -> (list change) * bindings
    (match-define (change rule loc bindings) (car cngs))
    (define result (pattern-substitute (rule-output rule) bindings))
    (define bindings* (pattern-match pattern result))
    (when bindings* (sow (cons cngs bindings*))))

  (define cache (make-hash))
  (define (matcher* expr pattern loc cdepth)
    (hash-ref! cache (list loc pattern cdepth)
               (λ () (matcher expr pattern loc cdepth))))

  (define (matcher expr pattern loc cdepth)
    ; expr pattern _ -> (list ((list change) * bindings))
    (reap [sow]
      (match pattern
        [(? variable?)
         (sow (cons '() (list (cons pattern expr))))]
        [(? number?)
         (when (equal? expr pattern)
           (sow (cons '() '())))]
        [(list phead _ ...)
         (when (and (list? expr) (equal? phead (car expr))
                    (= (length pattern) (length expr)))
           (let/ec k ;; We have an option to early exit if a child pattern cannot be matched
             (define child-options ; (list (list ((list cng) * bnd)))
               (for/list ([i (in-naturals)] [sube expr] [subp pattern] #:when (> i 0))
                 ;; Note: fuel is "depth" not "cdepth", because we're recursing to a child
                 (define options (matcher* sube subp (cons i loc) depth))
                 (when (null? options) (k)) ;; Early exit 
                 options))
             (reduce-children sow (apply cartesian-product child-options))))

         (when (and (> cdepth 0)
                    (or (flag-set? 'generate 'better-rr)
                        (not (and (list? expr) (equal? phead (car expr)) (= (length pattern) (length expr))))))
           ;; Sort of a brute force approach to getting the bindings
           (rewriter (curry fix-up-variables sow pattern)
                     expr (car pattern) (length pattern) loc (- cdepth 1)))])))

  ;; The "#f #f" means that any output result works. It's a bit of a hack
  (reap [sow] (rewriter (compose sow reverse) expr #f #f (reverse root-loc) depth)))

;;  Egg rewriter

;; Fallback system
;;  batch-egg-rewrite - batched call to egg
;;  egg-rewrite - call to egg on an expression (skipped if batch-egg-rewrite called with 1 expr)
;;  egg-rewrite (with iter limit) - call to egg on an expression with an iter limit (last resort)

(lazy-require
 [egg-herbie (with-egraph egraph-add-exprs egraph-get-variants
              egraph-is-unsound-detected egraph-get-times-applied
              egg-exprs->exprs)])

; If unsoundness was detected, try running one epxression at a time.
; Can optionally set iter limit (will give up if unsoundness detected).
; Returns (cons <rule-count> <variants>).
(define (egg-rewrite expr repr #:rules rules #:root [root-loc '()] #:limit [iter-limit #f])
  (define egg-rule (rule "egg-rr" 'x 'x (list repr) repr))
  (define irules (rules->irules rules))
  (timeline-push! 'method (~a (object-name egg-rewrite)))
  ;; returns a procedure rather than the variants directly:
  ;; if we need to fallback, we exit the `with-egraph` closure first
  ;; so the existing egraph gets cleaned up
  (define result-thunk
    (with-egraph
      (lambda (egg-graph)
        (egraph-add-exprs
          egg-graph
          (list expr)
          (lambda (node-ids)
            (define iter-data (egg-run-rules egg-graph (*node-limit*) irules node-ids #t #:limit iter-limit))
            (cond
             [(egraph-is-unsound-detected egg-graph)
              ; give up if iter limit is set
              ; otherwise try with iter limit
              (λ ()
                (if (and iter-limit (>= iter-limit 2))
                    (cons (hash) '())
                    (let ([limit (- (length iter-data) 2)])
                      (egg-rewrite expr repr #:rules rules #:root root-loc #:limit limit))))]
             [else
              (define expr-id (first node-ids))
              (define output (egraph-get-variants egg-graph expr-id expr))
              (define extracted (egg-exprs->exprs output egg-graph))
              (define rule-counts
                (for/hash ([rule rules])
                  (values (rule-name rule) (egraph-get-times-applied egg-graph (rule-name rule)))))
              (define variants
                (for/list ([variant (remove-duplicates extracted)])
                  (list (change egg-rule root-loc (list (cons 'x variant))))))
              (λ () (cons rule-counts variants))]))))))
  (result-thunk))

(define (batch-egg-rewrite exprs
                           repr
                           #:rules rules
                           #:roots [root-locs (make-list (length exprs) '())]
                           #:depths [depths (make-list (length exprs) 1)])
  (define egg-rule (rule "egg-rr" 'x 'x (list repr) repr))
  (define irules (rules->irules rules))
  ;; returns a procedure rather than the variants directly:
  ;; if we need to fallback, we exit the `with-egraph` closure first
  ;; so the existing egraph gets cleaned up
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
              ; something bad happened
              ; fallback and run one at a time
              (λ ()
                (define rule-counts (make-hash))
                (define iter-limit (and (= (length exprs) 1) (- (length iter-data) 2)))
                (define variants
                (for/list ([expr exprs] [root-loc root-locs])
                  (match-define (cons rcs variants)
                    (egg-rewrite expr repr #:rules rules #:root root-loc #:limit iter-limit))
                  (hash-union! rule-counts rcs #:combine +)
                  variants))
                  (for ([(name count) (in-hash rule-counts)])
                    (when (> count 0) (timeline-push! 'rules (~a name) count)))
                  variants)]
             [else
              (define variants
                (for/list ([id node-ids] [expr exprs] [root-loc root-locs])
                  (define output (egraph-get-variants egg-graph id expr))
                  (define extracted (egg-exprs->exprs output egg-graph))
                  (for/list ([variant (remove-duplicates extracted)])
                    (list (change egg-rule root-loc (list (cons 'x variant)))))))
              (λ () variants)]))))))
  (result-thunk))

;;  Recursive rewrite chooser
(define (rewrite-expressions exprs
                             repr 
                             #:rules rules
                             #:roots [root-locs (make-list (length exprs) '())]
                             #:depths [depths (make-list (length exprs) 1)])
  ; choose correct rr driver
  (cond
   [(null? exprs) '()]
   [else
    (define driver
      (cond
      [(not (flag-set? 'generate 'rr)) rewrite-once]
      [(flag-set? 'generate 'egg-rr) batch-egg-rewrite]
      [else recursive-rewrite]))
    (timeline-push! 'method (~a (object-name driver)))

    ; sequential or batched rewriting
    (match driver
     [batch-egg-rewrite
      (debug #:from 'progress #:depth 4 "batched rewriting for" exprs)
      (define tnow (current-inexact-milliseconds))
      (begin0 (driver exprs repr #:rules rules #:roots root-locs #:depths depths)
        (for ([expr exprs]) (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow))))]
     [_
      (for/list ([expr exprs] [root-loc root-locs] [depth depths] [n (in-naturals 1)])
        (debug #:from 'progress #:depth 4 "[" n "/" (length exprs) "] rewriting for" expr)
        (define tnow (current-inexact-milliseconds))
        (begin0 (driver expr repr #:rules rules #:root root-loc #:depth depth)
          (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow))))])]))
