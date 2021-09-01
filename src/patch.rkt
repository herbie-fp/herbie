#lang racket

(require "core/matcher.rkt" "core/taylor.rkt" "core/simplify.rkt"
         "alternative.rkt" "common.rkt" "interface.rkt" "programs.rkt"
         "timeline.rkt" "syntax/rules.rkt" "syntax/sugar.rkt")

(provide
  (contract-out
   [patch-table-add! (-> expr? (listof symbol?) boolean? void?)]
   [patch-table-get (-> expr? (listof alt?))]
   [patch-table-has-expr? (-> expr? boolean?)]
   [patch-table-runnable? (-> boolean?)]
   [patch-table-run (-> (listof alt?))]
   [patch-table-run-simplify (-> (listof alt?) (listof alt?))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Patch table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct patchtable (table queued queuedlow rewrites series final) #:mutable)

; The "patch table"
; Stores a mapping from expression to improvements (expr -> (listof exprs))
(define *patch-table* (patchtable (make-hash) '() '() #f #f #f))

; patch table may be invalidated between runs
(register-reset
  (λ () (set! *patch-table* (patchtable (make-hash) '() '() #f #f #f))))

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

(define (^final^ [val #f])
  (when val (set-patchtable-final! *patch-table* val))
  (patchtable-final *patch-table*))

; Adds an improvement to the patch table
; If `improve` is not provided, a key is added
; with no improvements
(define (add-patch! expr [improve #f])
  (cond
   [(not (*use-improve-cache*)) (void)]
   [improve
    (hash-update! (patchtable-table *patch-table*) expr
                  (curry cons improve) (list))]
   [else
    (hash-update! (patchtable-table *patch-table*) expr
                  identity (list))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (neg ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

;; Taylor is problematic since it doesn't know what reprs are
;; There are two types of errors that occur due to this inconsistency
;;  - reduce:
;;      the internal simplifier will try to desugar an subexpression
;;      with an operator/precision mismatch
;;  - external:
;;      Taylor is successful in generating an expression but
;;      external desugaring fails because of an unsupported/mismatched
;;      operator
(define (taylor-fail-desugaring expr)
  (λ _
    (debug #:from 'progress #:depth 5 "Series expansion (desugaring failure)")
    (debug #:from 'progress #:depth 5 "Problematic expression: " expr)
    #f))

; taylor uses older format, resugaring and desugaring needed
; not all taylor transforms are valid in a given repr, return false on failure
(define (taylor-expr expr repr var f finv)
  (define expr* (resugar-program expr repr #:full #f))
  (with-handlers ([exn:fail? (const #f)])       ; in case taylor fails internally
    (define genexpr (approximate expr* var #:transform (cons f finv)))
    (λ (x) (desugar-program (genexpr) repr (*var-reprs*) #:full #f))))

(define (taylor-alt altn)
  (define expr (program-body (alt-program altn)))
  (define repr (repr-of expr (*output-repr*) (*var-reprs*)))
  (define vars (free-variables expr))
  (reap [sow]
    (for* ([var vars] [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define genexpr (taylor-expr expr repr var f finv))
      (cond
       [genexpr  ; taylor successful
        #;(define pts (for/list ([(p e) (in-pcontext (*pcontext*))]) p))
        (for ([i (in-range 4)])
          (define expr* 
            (with-handlers ([exn:fail? (taylor-fail-desugaring expr)]) ; failed on desugaring
              (location-do '(2) (alt-program altn) genexpr)))
          (when expr*
            (sow (alt expr* `(taylor ,name ,var (2)) (list altn)))))]
       [else  ; taylor failed
        (debug #:from 'progress #:depth 5 "Series expansion (internal failure)")
        (debug #:from 'progress #:depth 5 "Problematic expression: " expr)
        (sow altn)]))))

(define (gen-series!)
  (when (flag-set? 'generate 'taylor)
    (timeline-event! 'series)
    (define series-expansions
      (apply append
        (for/list ([altn (in-list (^queued^))] [n (in-naturals 1)])
          (define expr (program-body (alt-program altn)))
          (debug #:from 'progress #:depth 4 "[" n "/" (length (^queued^)) "] generating series for" expr)
          (define tnow (current-inexact-milliseconds))
          (begin0 (filter-not (curry alt-equal? altn) (taylor-alt altn))
            (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow))))))

    ; Probably unnecessary, at least CI passes!
    (define (is-nan? x)
      (and (operator? x) (equal? (hash-ref parametric-operators-reverse x #f) 'NAN)))
    (define series-expansions*
      (filter-not
        (λ (x) (expr-contains? (program-body (alt-program x)) is-nan?))
        series-expansions))

    ; TODO: accuracy stats for timeline
    (timeline-push! 'count (length (^queued^)) (length series-expansions*))
    (^series^ series-expansions*))
  (void))

(define (bad-alt! altn)
  (define expr (program-body (alt-program altn)))
  (when (expr-contains? expr rewrite-repr-op?)
    (error 'bad-alt! "containg rewrite repr ~a" expr)))

(define (gen-rewrites!)
  (when (and (null? (^queued^)) (null? (^queuedlow^)))
    (raise-user-error 'gen-rewrites! "No expressions queued in patch table. Run `patch-table-add!`"))

  (timeline-event! 'rewrite)
  (define rewrite (if (flag-set? 'generate 'rr) rewrite-expression-head rewrite-expression))
  (timeline-push! 'method (~a (object-name rewrite)))

  (define changelists
    (for/list ([altn (in-list (^queued^))] [n (in-naturals 1)])
      (define expr (program-body (alt-program altn)))
      (debug #:from 'progress #:depth 4 "[" n "/" (length (^queued^)) "] rewriting for" expr)
      (define tnow (current-inexact-milliseconds))
      (begin0 (rewrite expr (*output-repr*) #:rules (*rules*) #:root '(2))
        (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow)))))

  (define reprchange-rules
    (if (*pareto-mode*)
        (filter (λ (r) (expr-contains? (rule-output r) rewrite-repr-op?)) (*rules*))
        (list)))

  ; Empty in normal mode
  (define changelists-low-locs
    (for/list ([altn (in-list (^queuedlow^))] [n (in-naturals 1)])
      (define expr (program-body (alt-program altn)))
      (debug #:from 'progress #:depth 4 "[" n "/" (length (^queuedlow^)) "] rewriting for" expr)
      (define tnow (current-inexact-milliseconds))
      (begin0 (rewrite expr (*output-repr*) #:rules reprchange-rules #:root '(2))
        (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow)))))

  (define comb-changelists (append changelists changelists-low-locs))
  (define altns (append (^queued^) (^queuedlow^)))

  (define rules-used
    (append-map (curry map change-rule) (apply append comb-changelists)))
  (define rule-counts
    (for ([rgroup (group-by identity rules-used)])
      (timeline-push! 'rules (~a (rule-name (first rgroup))) (length rgroup))))

  (define rewritten
    (for/fold ([done '()] #:result (reverse done))
              ([cls comb-changelists] [altn altns] #:when true [cl cls])
      (let loop ([cl cl] [altn altn])
        (if (null? cl)
            (cons altn done)
            (let ([prog* (apply-repr-change (change-apply (car cl) (alt-program altn)))])
              (if (program-body prog*)
                  (loop (cdr cl) (alt prog* (list 'change (car cl)) (list altn)))
                  done))))))

  (define rewritten*
    (if (and (*pareto-mode*) (> (length rewritten) 1000))
        (take rewritten 1000)
        rewritten))
        
  (timeline-push! 'count (length (^queued^)) (length rewritten*))
  ; TODO: accuracy stats for timeline
  (^rewrites^ rewritten*)
  (void))

(define (get-starting-expr altn)
  (match (alt-event altn)
   [(list 'patch) (program-body (alt-program altn))]
   [_ (get-starting-expr (first (alt-prevs altn)))]))

(define (simplify!)
  (unless (or (^series^) (^rewrites^))
    (raise-user-error 'simplify! "No candidates generated. Run (gen-series!) or (gen-rewrites!)"))

  (when (flag-set? 'generate 'simplify)
    (timeline-event! 'simplify)
    (define children (append (or (^series^) empty) (or (^rewrites^) empty)))

    ;; We want to avoid simplifying if possible, so we only
    ;; simplify things produced by function calls in the rule
    ;; pattern. This means no simplification if the rule output as
    ;; a whole is not a function call pattern, and no simplifying
    ;; subexpressions that don't correspond to function call
    ;; patterns.
    (define locs-list
      (for/list ([child (in-list children)] [n (in-naturals 1)])
        (match (alt-event child)
          [(list 'taylor _ _ loc) (list loc)]
          [(list 'change cng)
           (match-define (change rule loc _) cng)
           (define pattern (rule-output rule))
           (cond
            [(not (list? pattern)) '()]
            [else
             (for/list ([pos (in-naturals 1)]
                        [arg-pattern (cdr pattern)] #:when (list? arg-pattern))
               (append loc (list pos)))])]
          [_ (list '(2))])))

    (define to-simplify
      (for/list ([child (in-list children)] [locs locs-list]
                 #:when true [loc locs])
        (location-get loc (alt-program child))))

    (define simplification-options
      (simplify-batch to-simplify #:rules (*simplify-rules*) #:precompute true))

    (define simplify-hash
      (make-immutable-hash (map cons to-simplify simplification-options)))

    (define simplified
      (apply append
             (for/list ([child (in-list children)] [locs locs-list])
              (make-simplification-combinations child locs simplify-hash))))

    ; dedup for cache
    (define simplified* (remove-duplicates simplified alt-equal?))
    (unless (and (null? (^queued^)) (null? (^queuedlow^)))  ; don't run for simplify-only
      (for ([altn (in-list simplified*)])
        (define cachable (map (compose program-body alt-program) (^queued^)))
        (let ([expr0 (get-starting-expr altn)])
          (when (set-member? cachable expr0)
            (add-patch! (get-starting-expr altn) altn)))))
    
    (timeline-push! 'count (length locs-list) (length simplified*))
    (^final^ simplified*))
  (void))

(define (patch-table-clear!)
  (^queued^ '())
  (^queuedlow^ '())
  (^rewrites^ #f)
  (^series^ #f)
  (^final^ #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-has-expr? expr)
  (hash-has-key? (patchtable-table *patch-table*) expr))

(define (patch-table-add! expr vars down?)
  (when (patch-table-has-expr? expr)
    (raise-user-error 'patch-table-add!
      "attempting to add previously patched expression: ~a"
      expr))
  (define altn* (alt `(λ ,vars ,expr) `(patch) '()))
  (if down?
      (^queuedlow^ (cons altn* (^queuedlow^)))
      (^queued^ (cons altn* (^queued^))))
  (void))

(define (patch-table-get expr)
  (hash-ref (patchtable-table *patch-table*) expr))

(define (patch-table-runnable?)
  (or (not (null? (^queued^))) (not (null? (^queuedlow^)))))

(define (patch-table-run)
  (debug #:from 'progress #:depth 3 "generating series expansions")
  (if (null? (^queued^))
      (^series^ '())
      (gen-series!))
  (debug #:from 'progress #:depth 3 "generating rewritten candidates")
  (gen-rewrites!)
  (debug #:from 'progress #:depth 3 "simplifying candidates")
  (simplify!)
  (begin0 (^final^)
    (patch-table-clear!)))

(define (patch-table-run-simplify altns)
  (^rewrites^ (append altns (if (^rewrites^) (^rewrites^) '())))
  (simplify!)
  (begin0 (^final^)
    (patch-table-clear!)))
          
