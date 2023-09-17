#lang racket

(require "syntax/types.rkt" "syntax/syntax.rkt" "syntax/rules.rkt" "syntax/sugar.rkt")
(require "alternative.rkt" "common.rkt" "errors.rkt" "timeline.rkt")
(require "programs.rkt" "conversions.rkt" "core/matcher.rkt" "core/taylor.rkt" "core/simplify.rkt" "core/egg-herbie.rkt")

(provide
  (contract-out
   [patch-table-has-expr? (-> expr? boolean?)]
   [patch-table-run-simplify (-> (listof alt?) (listof alt?))]
   [patch-table-run (-> (listof expr?) (listof expr?) (listof alt?))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Patch table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct patchtable (table queued queuedlow rewrites series final) #:mutable)

(define (empty-patchtable)
  (patchtable (make-hash) '() '() #f #f #f))

; The "patch table"
; Stores a mapping from expression to improvements (expr -> (listof exprs))
(define-resetter *patch-table*
  (λ () (empty-patchtable))
  (λ () (empty-patchtable)))

; setters / getters

(define (^queued^ [val #f])
  (when val (set-patchtable-queued! (*patch-table*) val))
  (patchtable-queued (*patch-table*)))

(define (^queuedlow^ [val #f])
  (when val (set-patchtable-queuedlow! (*patch-table*) val))
  (patchtable-queuedlow (*patch-table*)))

(define (^rewrites^ [val #f])
  (when val (set-patchtable-rewrites! (*patch-table*) val))
  (patchtable-rewrites (*patch-table*)))

(define (^series^ [val #f])
  (when val (set-patchtable-series! (*patch-table*) val))
  (patchtable-series (*patch-table*)))

(define (^final^ [val #f])
  (when val (set-patchtable-final! (*patch-table*) val))
  (patchtable-final (*patch-table*)))

; Adds an improvement to the patch table
; If `improve` is not provided, a key is added
; with no improvements
(define (add-patch! expr [improve #f])
  (when (*use-improve-cache*)
    (hash-update! (patchtable-table (*patch-table*)) expr
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

(define (taylor-expr expr repr var f finv)
  (define expr* (resugar-program expr repr #:full #f))
  (define genexpr (approximate expr* var #:transform (cons f finv)))
  (λ ()
    (with-handlers ([exn:fail:user:herbie:missing? (const #f)])
      (desugar-program (genexpr) (*context*) #:full #f))))

(define (taylor-alt altn)
  (define expr (alt-expr altn))
  (define repr (repr-of expr (*context*)))
  (reap [sow]
    (for* ([var (free-variables expr)] [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define timeline-stop! (timeline-start! 'series (~a expr) (~a var) (~a name)))
      (define genexpr (taylor-expr expr repr var f finv))
      (for ([i (in-range 4)])
        (define replace (genexpr))
        (when replace
          (sow (alt replace `(taylor () ,name ,var) (list altn) '()))))
      (timeline-stop!))))

(define (gen-series!)
  (when (flag-set? 'generate 'taylor)
    (timeline-event! 'series)
    (define series-expansions
      (apply append
        (for/list ([altn (in-list (^queued^))] [n (in-naturals 1)])
          (filter-not (curry alt-equal? altn) (taylor-alt altn)))))

    ; Probably unnecessary, at least CI passes!
    (define (is-nan? x)
      (and (impl-exists? x) (equal? (impl->operator x) 'NAN)))

    (define series-expansions*
      (filter-not
        (λ (x) (expr-contains? (alt-expr x) is-nan?))
        series-expansions))

    ; TODO: accuracy stats for timeline
    (timeline-push! 'count (length (^queued^)) (length series-expansions*))
    (^series^ series-expansions*))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Splits rules into three categories
;  - reprchange: rules that change precision
;  - expansive: rules of the form `x -> f(x)` that are not reprchange
;  - normal: everything else
(define (partition-rules rules)
  (define-values (expansive-or-repr-change normal)
    (partition (compose variable? rule-input) rules))
  (define-values (reprchange expansive*)
    (partition (λ (r) (expr-contains? (rule-output r) rewrite-repr-op?))
               expansive-or-repr-change))
  (values reprchange expansive* normal))

(define (merge-changelists . lsts)
  (map (curry apply append) (flip-lists lsts)))

(define (gen-rewrites!)
  (when (and (null? (^queued^)) (null? (^queuedlow^)))
    (raise-user-error 'gen-rewrites! "No expressions queued in patch table. Run `patch-table-add!`"))

  (^rewrites^ '())
  (when (flag-set? 'generate 'rr)
    (timeline-event! 'rewrite)
    (define real-alts (filter (λ (a) (equal? (type-of (alt-expr a) (*context*)) 'real)) (^queued^)))

    ;; partition the rules
    (define-values (reprchange-rules expansive-rules normal-rules) (partition-rules (*rules*)))

    ;; get subexprs and locations
    (define real-exprs (map alt-expr real-alts))
    (define lowexprs (map alt-expr (^queuedlow^)))

    ;; HACK:
    ;; - check loaded representations
    ;; - if there is only one real representation, allow expansive rules to be run in egg
    ;; This is just a workaround and should definitely be fixed
    (define one-real-repr? (= (count (λ (r) (equal? (representation-type r) 'real)) (*needed-reprs*)) 1))

    ;; rewrite high-error locations
    (define changelists
      (if one-real-repr?
          (merge-changelists
            (rewrite-expressions real-exprs (*context*) #:rules (append expansive-rules normal-rules))
            (rewrite-expressions real-exprs (*context*) #:rules reprchange-rules #:once? #t))
          (merge-changelists
            (rewrite-expressions real-exprs (*context*) #:rules normal-rules)
            (rewrite-expressions real-exprs (*context*) #:rules expansive-rules #:once? #t)
            (rewrite-expressions real-exprs (*context*) #:rules reprchange-rules #:once? #t))))

    ;; rewrite low-error locations (only precision changes allowed)
    (define changelists-low-locs
      (rewrite-expressions lowexprs (*context*) #:rules reprchange-rules #:once? #t))

    (define comb-changelists (append changelists changelists-low-locs))
    (define altns (append real-alts (^queuedlow^)))
    
    (define rewritten
      (for/fold ([done '()] #:result (reverse done))
                ([cls comb-changelists] [altn altns]
                #:when true [cl cls])
          (match-define (list subexp input) cl)
            (define body* (apply-repr-change-expr subexp (*context*)))
            (if body*
              ; We need to pass '() here so it can get overwritten on patch-fix
              (cons (alt body* (list 'rr '() input #f #f) (list altn) '()) done)
              done)))

    (timeline-push! 'count (length (^queued^)) (length rewritten))
    ; TODO: accuracy stats for timeline
    (^rewrites^ rewritten))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simplify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-starting-expr altn)
  (match (alt-event altn)
   [(list 'patch) (alt-expr altn)]
   [_ (get-starting-expr (first (alt-prevs altn)))]))

(define (simplify!)
  (unless (or (^series^) (^rewrites^))
    (raise-user-error 'simplify! "No candidates generated. Run (gen-series!) or (gen-rewrites!)"))

  ; load final in case simplify is disabled
  (^final^ (append (or (^series^) '()) (or (^rewrites^) '())))
  (when (flag-set? 'generate 'simplify)
    (timeline-event! 'simplify)
    (define children (^final^))

    (define to-simplify (map alt-expr children))

    (define egg-query (make-egg-query to-simplify (*simplify-rules*)))
    (define simplification-options (simplify-batch egg-query))

    (define simplified
      (remove-duplicates
       (for/list ([child (in-list children)] [input (in-list to-simplify)]
                  [outputs (in-list simplification-options)]
                  #:when true
                  [output outputs])
         (if (equal? input output)
             child
             (alt output `(simplify () ,egg-query #f #f) (list child) '())))
       alt-equal?))

    ; dedup for cache
    (unless (and (null? (^queued^)) (null? (^queuedlow^)))  ; don't run for simplify-only
      (for ([altn (in-list simplified)])
        (define cachable (map alt-expr (^queued^)))
        (let ([expr0 (get-starting-expr altn)])
          (when (set-member? cachable expr0)
            (add-patch! (get-starting-expr altn) altn)))))
    
    (timeline-push! 'count (length children) (length simplified))
    (^final^ simplified))
  (void))

(define (patch-table-clear!)
  (^queued^ '())
  (^queuedlow^ '())
  (^rewrites^ #f)
  (^series^ #f)
  (^final^ #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-has-expr? expr)
  (hash-has-key? (patchtable-table (*patch-table*)) expr))

(define (patch-table-add! expr down?)
  (when (patch-table-has-expr? expr)
    (raise-user-error 'patch-table-add!
      "attempting to add previously patched expression: ~a"
      expr))
  (define altn* (alt expr `(patch) '() '()))
  (if down?
      (^queuedlow^ (cons altn* (^queuedlow^)))
      (^queued^ (cons altn* (^queued^))))
  (void))

(define (patch-table-get expr)
  (hash-ref (patchtable-table (*patch-table*)) expr))

(define (patch-table-runnable?)
  (or (not (null? (^queued^))) (not (null? (^queuedlow^)))))

(define (patch-table-run locs lowlocs)
  (define cached
    (for/fold ([qed '()] [ced '()]
              #:result (begin0 (reverse ced) (^queued^ (reverse qed))))
              ([expr (in-list locs)])
      (if (patch-table-has-expr? expr)
          (values qed (cons expr ced))
          (let ([altn* (alt expr `(patch) '() '())])
            (values (cons altn* qed) ced)))))
  (^queuedlow^
    (for/list ([expr (in-list lowlocs)])
      (alt expr `(patch) '() '())))
  (cond
   [(and (null? (^queued^))       ; only fetch cache
         (null? (^queuedlow^)))
    (append-map patch-table-get cached)]
   [else                          ; run patches
    (if (null? (^queued^))
        (^series^ '())
        (gen-series!))
    (gen-rewrites!)
    (simplify!)
    (begin0 (apply append (^final^) (map patch-table-get cached))
      (patch-table-clear!))]))

(define (patch-table-run-simplify altns)
  (^rewrites^ (append altns (if (^rewrites^) (^rewrites^) '())))
  (simplify!)
  (begin0 (^final^)
    (patch-table-clear!)))
          
