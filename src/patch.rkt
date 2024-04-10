#lang racket

(require "syntax/rules.rkt"
         "syntax/sugar.rkt"
         "syntax/syntax.rkt"
         "syntax/types.rkt"
         "core/egg-herbie.rkt"
         "core/matcher.rkt"
         "core/simplify.rkt"
         "core/taylor.rkt"
         "config.rkt"
         "accelerator.rkt"
         "alternative.rkt"
         "common.rkt"
         "conversions.rkt"
         "errors.rkt"
         "platform.rkt"
         "programs.rkt"
         "timeline.rkt")

(provide
  (contract-out
   [patch-table-has-expr? (-> expr? boolean?)]
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
(define (add-patch! expr improvements)
  (when (*use-improve-cache*)
    (hash-update! (patchtable-table (*patch-table*)) expr
                  (curry cons improvements) (list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Taylor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (neg ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

;; Taylor is problematic since it doesn't know what reprs are.
;; There are two types of errors that occur due to this inconsistency
;;  - reduce:
;;      the internal simplifier will try to desugar an subexpression
;;      with an operator/precision mismatch
;;  - external:
;;      Taylor is successful in generating an expression but
;;      external desugaring fails because of an unsupported/mismatched
;;      operator

(define (taylor-alt altn)
  (define expr (expand-accelerators (*rules*) (prog->spec (alt-expr altn))))
  (reap [sow]
    (for* ([var (free-variables expr)] [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define timeline-stop! (timeline-start! 'series (~a expr) (~a var) (~a name)))
      (define genexpr (approximate expr var #:transform (cons f finv)))
      (for ([_ (in-range (*taylor-order-limit*))])
        (define replace
          (with-handlers ([exn:fail:user:herbie:missing? (const #f)])
            (spec->prog (genexpr) (*context*))))
        (when replace
          (sow (alt replace `(taylor () ,name ,var) (list altn) '()))))
      (timeline-stop!))))

(define (gen-series queued)
  (timeline-event! 'series)
  (timeline-push! 'inputs (map ~a queued))
  (define series-expansions
    (apply append
           (for/list ([altn (in-list queued)] [n (in-naturals 1)])
             (filter-not (curry alt-equal? altn) (taylor-alt altn)))))
  (timeline-push! 'outputs (map ~a series-expansions))
  (timeline-push! 'count (length queued) (length series-expansions))
  series-expansions)

(define (gen-series!)
  (^series^ '())
  (when (flag-set? 'generate 'taylor)
    (^series^ (gen-series (^queued^))))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gen-repr-change queued queuedlow)
  (timeline-event! 'rewrite)
  (define all-alts (append queued queuedlow))
  (define alts
    (filter (λ (a) (equal? (type-of (alt-expr a) (*context*)) 'real))
            alts))

  (define reprchange-rules (platform-reprchange-rules (*active-platform*)))

  ;; get subexprs and locations
  (define exprs (map alt-expr alts))

  ;; rewrite high-error locations
  (define changelists
    (rewrite-expressions exprs (*context*) #:rules reprchange-rules #:once? #t))

  (define rewritten
    (reap [sow]
          (for ([changelist (in-list changelists)] [altn (in-list alts)]
                #:when true
                [cl (in-list changelist)])
            (match-define (list subexp input) cl)
            (define body* (apply-repr-change-expr subexp (*context*)))
            (when body*
              ; apply-repr-change-expr is partial
              ; we need to pass '() here so it can get overwritten on patch-fix
              (sow (alt body* (list 'rr '() input #f #f) (list altn) '()))))))

  (timeline-push! 'count (length queued) (length rewritten))
  rewritten)

(define (gen-rewrites queued queuedlow)
  (timeline-event! 'rewrite)
  (define alts (append queued queuedlow))

  (define changelists
    (rewrite-expressions (map alt-expr alts) (*context*) #:rules (*rules*)))

  (define rewritten
    (for/list ([changelist (in-list changelists)] [altn (in-list alts)]
               #:when true
               [cl (in-list changelist)])
      (match-define (list subexp input) cl)
      (alt subexp (list 'rr '() input #f #f) (list altn) '())))

  (timeline-push! 'count (length alts) (length rewritten))
  rewritten)

(define (gen-rewrites!)
  (when (and (null? (^queued^)) (null? (^queuedlow^)))
    (raise-user-error 'gen-rewrites! "No expressions queued in patch table. Run `patch-table-add!`"))
  (^rewrites^ '())
  (when (flag-set? 'generate 'rr)
    (^rewrites^ (gen-rewrites (^queued^) (^queuedlow^))))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simplify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify children)
  (timeline-event! 'simplify)
  (define to-simplify (map alt-expr children))

  (define egg-query (make-egg-query to-simplify (*simplify-rules*)))
  (define simplification-options (simplify-batch egg-query))

  (define simplified
    (remove-duplicates
     (for/list ([child (in-list children)]
                [input (in-list to-simplify)]
                [outputs (in-list simplification-options)]
                #:when true [output outputs])
       (if (equal? input output)
           child
           (alt output `(simplify () ,egg-query #f #f) (list child) '())))
       alt-equal?))

  (timeline-push! 'count (length children) (length simplified))
  simplified)

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
    (define simplified (simplify (^final^)))

    ; dedup for cache
    (unless (and (null? (^queued^)) (null? (^queuedlow^)))  ; don't run for simplify-only
      (for ([altn (in-list simplified)])
        (define cachable (map alt-expr (^queued^)))
        (let ([expr0 (get-starting-expr altn)])
          (when (set-member? cachable expr0)
            (add-patch! (get-starting-expr altn) altn)))))
    
    (^final^ (simplify (^final^))))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-clear!)
  (^queued^ '())
  (^queuedlow^ '())
  (^rewrites^ #f)
  (^series^ #f)
  (^final^ #f))

(define (patch-table-has-expr? expr)
  (hash-has-key? (patchtable-table (*patch-table*)) expr))

(define (patch-table-get expr)
  (hash-ref (patchtable-table (*patch-table*)) expr))

(define (patch-table-run locs lowlocs)
  (define-values (cached queued)
    (for/fold ([qed '()] [ced '()]
              #:result (values (reverse ced) (reverse qed)))
              ([expr (in-list locs)])
      (if (patch-table-has-expr? expr)
          (values qed (cons expr ced))
          (let ([altn* (alt expr `(patch) '() '())])
            (values (cons altn* qed) ced)))))
  (^queued^ queued)
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
