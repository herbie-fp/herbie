#lang racket

(require "core/matcher.rkt" "core/taylor.rkt" "core/simplify.rkt"
         "alternative.rkt" "common.rkt" "errors.rkt" "interface.rkt" "programs.rkt"
         "timeline.rkt" "syntax/rules.rkt" "syntax/sugar.rkt")

(provide
  (contract-out
   [patch-table-has-expr? (-> expr? boolean?)]
   [patch-table-run-simplify (-> (listof alt?) (listof alt?))]
   [patch-table-run (-> (listof (cons/c (listof symbol?) expr?))
                        (listof (cons/c (listof symbol?) expr?))
                        (listof alt?))]))

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
  (when (*use-improve-cache*)
    (hash-update! (patchtable-table *patch-table*) expr
                  (if improve (curry cons improve) identity) (list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Taylor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (define genexpr (approximate expr* var #:transform (cons f finv)))
  (λ ()
    (with-handlers ([exn:fail:user:herbie:missing? (taylor-fail-desugaring expr)])
      (desugar-program (genexpr) repr (*var-reprs*) #:full #f))))

(define (taylor-alt altn)
  (define prog (alt-program altn))
  (define expr (program-body prog))
  (define repr (repr-of expr (*output-repr*) (*var-reprs*)))
  (reap [sow]
    (for* ([var (free-variables expr)] [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define genexpr (taylor-expr expr repr var f finv))
      (for ([i (in-range 4)])
        (define replace (genexpr))
        (when replace
          (define expr* (list (first prog) (second prog) replace))
          (sow (alt expr* `(taylor ,name ,var (2)) (list altn))))))))

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
      (and (impl-exists? x) (equal? (impl->operator x) 'NAN)))

    (define series-expansions*
      (filter-not
        (λ (x) (expr-contains? (program-body (alt-program x)) is-nan?))
        series-expansions))

    ; TODO: accuracy stats for timeline
    (timeline-push! 'count (length (^queued^)) (length series-expansions*))
    (^series^ series-expansions*))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gen-rewrites!)
  (when (and (null? (^queued^)) (null? (^queuedlow^)))
    (raise-user-error 'gen-rewrites! "No expressions queued in patch table. Run `patch-table-add!`"))
  (timeline-event! 'rewrite)

  (define reprchange-rules                              ;; empty in non-Pareto mode
    (if (*pareto-mode*)
        (filter (λ (r) (expr-contains? (rule-output r) rewrite-repr-op?)) (*rules*))
        (list)))

  ;; get subexprs and locations
  (define exprs (map (compose program-body alt-program) (^queued^)))
  (define lowexprs (map (compose program-body alt-program) (^queuedlow^)))
  (define locs (make-list (length (^queued^)) '(2)))          ;; always at the root
  (define lowlocs (make-list (length (^queuedlow^)) '(2)))    ;; always at the root

  ;; rewrite
  (define changelists
    (rewrite-expressions exprs (*output-repr*) #:rules (*rules*) #:roots locs))
  (define changelists-low-locs
    (rewrite-expressions lowexprs (*output-repr*) #:rules reprchange-rules #:roots lowlocs))

  (define comb-changelists (append changelists changelists-low-locs))
  (define altns (append (^queued^) (^queuedlow^)))

  (define rules-used
    (append-map (curry map change-rule) (apply append comb-changelists)))
  (define rule-counts
    (for ([rgroup (group-by identity rules-used)])
      (timeline-push! 'rules (~a (rule-name (first rgroup))) (length rgroup))))
  
  (define rewritten
    (for/fold ([done '()] #:result (reverse done))
              ([cls comb-changelists] [altn altns]
              #:when true [cl cls])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simplify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-starting-expr altn)
  (match (alt-event altn)
   [(list 'patch) (program-body (alt-program altn))]
   [_ (get-starting-expr (first (alt-prevs altn)))]))

(define (simplify!)
  (unless (or (^series^) (^rewrites^))
    (raise-user-error 'simplify! "No candidates generated. Run (gen-series!) or (gen-rewrites!)"))

  ; load final in case simplify is disabled
  (^final^ (append (or (^series^) '()) (or (^rewrites^) '())))
  (when (flag-set? 'generate 'simplify)
    (timeline-event! 'simplify)
    (define children (^final^))

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

(define (patch-table-run locs lowlocs)
  (define cached
    (for/fold ([qed '()] [ced '()]
              #:result (begin0 (reverse ced) (^queued^ (reverse qed))))
              ([(vars expr) (in-dict locs)])
      (if (patch-table-has-expr? expr)
          (values qed (cons expr ced))
          (let ([altn* (alt `(λ ,vars ,expr) `(patch) '())])
            (values (cons altn* qed) ced)))))
  (^queuedlow^
    (for/list ([(vars expr) (in-dict lowlocs)])
      (alt `(λ ,vars ,expr) `(patch) '())))
  (cond
   [(and (null? (^queued^))       ; early exit, nothing queued
         (null? (^queuedlow^))
         (null? cached))
    '()]
   [(and (null? (^queued^))       ; only fetch cache
         (null? (^queuedlow^)))
    (append-map patch-table-get cached)]
   [else                          ; run patches
    (debug #:from 'progress #:depth 3 "generating series expansions")
    (if (null? (^queued^))
        (^series^ '())
        (gen-series!))
    (debug #:from 'progress #:depth 3 "generating rewritten candidates")
    (gen-rewrites!)
    (debug #:from 'progress #:depth 3 "simplifying candidates")
    (simplify!)
    (begin0 (apply append (^final^) (map patch-table-get cached))
      (patch-table-clear!))]))

(define (patch-table-run-simplify altns)
  (^rewrites^ (append altns (if (^rewrites^) (^rewrites^) '())))
  (simplify!)
  (begin0 (^final^)
    (patch-table-clear!)))
          
