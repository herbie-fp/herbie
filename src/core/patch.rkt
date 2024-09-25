#lang racket

(require "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "egg-herbie.rkt"
         "programs.rkt"
         "rules.rkt"
         "simplify.rkt"
         "taylor.rkt"
         "batch.rkt")

(provide generate-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simplify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lower-approximations approxs global-batch)
  (timeline-event! 'simplify)

  (define reprs
    (for/list ([approx (in-list approxs)])
      (define prev (car (alt-prevs approx)))
      (repr-of (batchref->expr (alt-expr prev)) (*context*))))

  ; generate real rules
  (define rules (real-rules (*simplify-rules*)))
  (define lowering-rules (platform-lowering-rules))

  ; egg runner
  (define schedule
    (if (flag-set? 'generate 'simplify)
        ; if simplify enabled, 2-phases for real rewrites and implementation selection
        `((,rules . ((node . ,(*node-limit*))))
          (,lowering-rules . ((iteration . 1) (scheduler . simple))))
        ; if disabled, only implementation selection
        `((,lowering-rules . ((iteration . 1) (scheduler . simple))))))

  (define roots
    (for/vector ([approx (in-list approxs)])
      (batchref-idx (alt-expr approx))))

  ; run egg
  (define runner (make-egg-runner global-batch roots reprs schedule))
  (define simplification-options
    (simplify-batch runner
                    (typed-egg-batch-extractor
                     (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc)
                     global-batch)))

  ; Start of global-batch modification
  ; ----------------------------------
  (define global-batch-mutable (batch->mutable-batch global-batch))

  ; convert to altns
  (define simplified
    (reap [sow]
          (for ([altn (in-list approxs)]
                [outputs (in-list simplification-options)])
            (match-define (cons _ simplified) outputs)
            (define prev (car (alt-prevs altn)))
            (for ([expr (in-list simplified)])
              (define spec (prog->spec (batchref->expr (alt-expr prev))))
              (define idx
                (batch-push! global-batch-mutable
                             (approx (mutable-batch-munge! global-batch-mutable spec)
                                     (batchref-idx expr))))
              (sow (alt (batchref global-batch idx) `(simplify ,runner #f #f) (list altn) '()))))))

  ; Commit changes to global-batch
  (batch-copy-mutable-nodes! global-batch global-batch-mutable)
  ; End of global-batch modification
  ; ---------------------------------

  (timeline-push! 'count (length approxs) (length simplified))
  simplified)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Taylor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))]
        [exp-x (λ (x) `(exp ,x))]
        [log-x (λ (x) `(log ,x))]
        [ninvert-x (λ (x) `(/ 1 (neg ,x)))])
    `((0 ,identity ,identity) (inf ,invert-x ,invert-x)
                              (-inf ,ninvert-x ,ninvert-x)
                              #;(exp ,exp-x ,log-x)
                              #;(log ,log-x ,exp-x))))

(define (taylor-alts altns global-batch)
  (define exprs
    (for/list ([altn (in-list altns)])
      (prog->spec (batchref->expr (alt-expr altn)))))
  (define free-vars (map free-variables exprs))
  (define vars (list->set (append* free-vars)))

  ; Start of global-batch modification
  ; ----------------------------------
  (define global-batch-mutable (batch->mutable-batch global-batch))

  (define approxs
    (reap [sow]
          (for* ([var (in-set vars)]
                 [transform-type transforms-to-try])
            (match-define (list name f finv) transform-type)
            (define timeline-stop! (timeline-start! 'series (~a exprs) (~a var) (~a name)))
            (define genexprs (approximate exprs var #:transform (cons f finv)))
            (for ([genexpr (in-list genexprs)]
                  [altn (in-list altns)]
                  [fv (in-list free-vars)]
                  #:when (member var fv)) ; check whether var exists in expr at all
              (for ([i (in-range (*taylor-order-limit*))])
                (define gen (genexpr))
                (unless (spec-has-nan? gen)
                  (define idx (mutable-batch-munge! global-batch-mutable gen))
                  ; we create a batchref that doesn't exist yet in global-batch, we update it later
                  (sow (alt (batchref global-batch idx) `(taylor ,name ,var) (list altn) '())))))
            (timeline-stop!))))

  ; Commit changes to global-batch
  (batch-copy-mutable-nodes! global-batch global-batch-mutable)
  ; End of global-batch modification
  ; ----------------------------------

  approxs)

(define (spec-has-nan? expr)
  (expr-contains? expr (lambda (term) (eq? term 'NAN))))

(define (run-taylor altns global-batch)
  (timeline-event! 'series)
  (timeline-push! 'inputs (map ~a altns))

  (define approxs (taylor-alts altns global-batch))

  (timeline-push! 'outputs (map ~a approxs))
  (timeline-push! 'count (length altns) (length approxs))
  (lower-approximations approxs global-batch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-rr altns global-batch)
  (timeline-event! 'rewrite)

  ; generate required rules
  (define rules (real-rules (*rules*)))
  (define lifting-rules (platform-lifting-rules))
  (define lowering-rules (platform-lowering-rules))

  (define extractor
    (typed-egg-batch-extractor
     (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc)
     global-batch))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
      (,rules . ((node . ,(*node-limit*))))
      (,lowering-rules . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define exprs (map (compose batchref->expr alt-expr) altns))
  (define roots (list->vector (map (compose batchref-idx alt-expr) altns)))
  (define reprs (map (curryr repr-of (*context*)) exprs))
  (timeline-push! 'inputs (map ~a exprs))

  (define runner (make-egg-runner global-batch roots reprs schedule #:context (*context*)))
  ; batchrefss is a (listof (listof batchref))
  (define batchrefss (run-egg runner `(multi . ,extractor)))

  ; apply changelists
  (define rewritten
    (reap [sow]
          (for ([batchrefs (in-list batchrefss)]
                [altn (in-list altns)])
            (for ([batchref* (in-list batchrefs)])
              (sow (alt batchref* (list 'rr runner #f #f) (list altn) '()))))))

  (timeline-push! 'outputs (map (compose ~a alt-expr) rewritten))
  (timeline-push! 'count (length altns) (length rewritten))
  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-candidates exprs)
  ; Batch to where we will extract everything
  ; Roots of this batch are constantly updated
  (define global-batch (progs->batch exprs))
  (define start-roots (batch-roots global-batch))

  ; Starting alternatives
  (define start-altns
    (for/list ([expr (in-list exprs)]
               [root (in-vector start-roots)])
      (define repr (repr-of expr (*context*)))
      (alt (batchref global-batch root) (list 'patch expr repr) '() '())))

  ; Series expand
  (define approximations (if (flag-set? 'generate 'taylor) (run-taylor start-altns global-batch) '()))
  ; Recursive rewrite
  (define rewritten (if (flag-set? 'generate 'rr) (run-rr start-altns global-batch) '()))

  (define out (append approximations rewritten))

  out)
