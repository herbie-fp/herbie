#lang racket

(require "alternative.rkt" "common.rkt" "interface.rkt" "programs.rkt"
         "core/matcher.rkt" "core/taylor.rkt" "core/simplify.rkt"
         "timeline.rkt" "syntax/rules.rkt")

(provide
  (contract-out
   [patch-table-add! (-> alt? (listof natural?) boolean? void?)]
   [patch-table-get (-> expr? (listof expr?))]
   [patch-table-has-expr? (-> expr? boolean?)])
  patch-table-run!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Patch table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct patchtable (table queued queuedlow rewrites series) #:mutable)

; The "patch table"
; Stores a mapping from expression to improvements (expr -> (listof exprs))
(define *patch-table* (patchtable (make-hash) '() '() '() '()))

; patch table may be invalidated between runs
(register-reset
  (λ () (set! *patch-table* (patchtable (make-hash) '() '() '() '()))))

; setters / getters

(define (^queued^ [val #f])
  (when val (set-patchtable-queued! *patch-table* val))
  (patchtable-queued *patch-table*))

(define (^queuedlow^ [val #f])
  (when val (set-patchtable-queuedlow! *patch-table* val))
  (patchtable-queuedlow *patch-table*))

(define (^rewrites^ [val #f])
  (when val (set-patchtable-rewrites! *patch-table* val))
  (patchtable-rewrites *patch-table*))

(define (^series^ [val #f])
  (when val (set-patchtable-series! *patch-table* val))
  (patchtable-series *patch-table*))
  

; Adds an improvement to the patch table
; If `improve` is not provided, the patch table either
;  (a) leaves the existing list of improvements empty
;  (b) sets the improvements to be an empty list 
(define (add-patch! expr [improve #f])
  (if improve
      (hash-update! (patchtable-table *patch-table*) expr
                    (curry cons improve) (list improve))
      (hash-update! (patchtable-table *patch-table*) expr
                    identity (list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gen-rewrites!)
  (unless (^queued^)
    (raise-user-error 'gen-rewrites! "No expressions queued in patch table. Run `patch-table-add!`"))

  (timeline-event! 'rewrite)
  (define rewrite (if (flag-set? 'generate 'rr) rewrite-expression-head rewrite-expression))
  (timeline-push! 'method (~a (object-name rewrite)))

  (define changelists
    (for/list ([(location altn) (in-dict (^queued^))] [n (in-naturals 1)])
      (debug #:from 'progress #:depth 4 "[" n "/" (length (^queued^)) "] rewriting at" location)
      (define expr (program-body (alt-program altn)))
      (define tnow (current-inexact-milliseconds))
      (begin0 (rewrite expr (*output-repr*) #:rules (*rules*) #:root '(2))
        (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow)))))

  (define reprchange-rules
    (if (*pareto-mode*)
        (filter (λ (r) (expr-contains? (rule-output r) rewrite-repr-op?)) (*rules*))
        (list)))

  ; Empty in normal mode
  (define changelists-low-locs
    (for/list ([(location altn) (in-dict (^queuedlow^))] [n (in-naturals 1)])
      (debug #:from 'progress #:depth 4 "[" n "/" (length (^queuedlow^)) "] rewriting at" location)
      (define expr (program-body (alt-program altn)))
      (define tnow (current-inexact-milliseconds))
      (begin0 (rewrite expr (*output-repr*) #:rules reprchange-rules #:root '(2))
        (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow)))))

  (define comb-changelists (append changelists changelists-low-locs))
  (define altns (map cdr (append (^queued^) (^queuedlow^))))

  (define rules-used
    (append-map (curry map change-rule) (apply append comb-changelists)))
  (define rule-counts
    (for ([rgroup (group-by identity rules-used)])
      (timeline-push! 'rules (~a (rule-name (first rgroup))) (length rgroup))))

  (define (repr-change altn)
    (alt (apply-repr-change (alt-program altn)) (alt-event altn) (alt-prevs altn)))

  (define rewritten
    (filter (compose program-body alt-program)  ; false body means failure
      (for/list ([cls comb-changelists] [altn altns]
                #:when true [cl cls])
        (for/fold ([altn altn] #:result (repr-change altn)) ([cng cl])
            (alt (change-apply cng (alt-program altn))
                 (list 'change cng)
                 (list altn))))))

  (define rewritten*
    (if (and (*pareto-mode*) (> (length rewritten) 1000))
        (take rewritten 1000)
        rewritten))
        
  (timeline-push! 'count (length (^queued^)) (length rewritten*))
  (^rewrites^ rewritten*)
  ; TODO: accuracy concerns for timeline
  (void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-has-expr? expr)
  (hash-has-key? (patchtable-table *patch-table*) expr))

(define (patch-table-add! altn loc down?)
  (define expr (location-get loc (alt-program altn)))
  (when (patch-table-has-expr? expr)
    (raise-user-error 'patch-table-add! "Attempting to add previously patched expression!"))
  (define vars (program-variables (alt-program altn)))
  (define altn* (alt `(λ ,vars ,expr) (list 'patch loc) (list altn)))
  (if down?
      (^queuedlow^ (cons (cons loc altn*) (^queuedlow^)))
      (^queued^ (cons (cons loc altn*) (^queued^))))
  (void))

(define (patch-table-get expr)
  (void))

(define (patch-table-run!)
  (debug #:from 'progress #:depth 3 "generating series expansions")
  (debug #:from 'progress #:depth 3 "generating rewritten candidates")
  (debug #:from 'progress #:depth 3 "simplifying candidates")
  (gen-rewrites!)
  (void))
