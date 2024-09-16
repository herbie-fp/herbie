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

(define (lower-approximations approxs)
  (timeline-event! 'simplify)

  (define reprs
    (for/list ([approx (in-list approxs)])
      (define prev (car (alt-prevs approx)))
      (repr-of (alt-expr prev) (*context*))))

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

  ; run egg
  (define batch (progs->batch (map alt-expr approxs)))
  (define runner (make-egg-runner batch (batch-roots batch) reprs schedule))
  (define simplification-options
    (simplify-batch runner
                    (typed-egg-extractor
                     (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc))))

  ; convert to altns
  (define simplified
    (reap [sow]
          (for ([altn (in-list approxs)]
                [outputs (in-list simplification-options)])
            (match-define (cons _ simplified) outputs)
            (define prev (car (alt-prevs altn)))
            (for ([expr (in-list simplified)])
              (define spec (prog->spec (alt-expr prev)))
              (sow (alt (approx spec expr) `(simplify ,runner #f #f) (list altn) '()))))))

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

(define (taylor-alts altns)
  (define exprs
    (for/list ([altn (in-list altns)])
      (prog->spec (alt-expr altn))))
  (define free-vars (map free-variables exprs))
  (define vars (list->set (append* free-vars)))

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
                (sow (alt gen `(taylor ,name ,var) (list altn) '())))))
          (timeline-stop!))))

(define (spec-has-nan? expr)
  (expr-contains? expr (lambda (term) (eq? term 'NAN))))

(define (run-taylor altns)
  (timeline-event! 'series)
  (timeline-push! 'inputs (map ~a altns))

  (define approxs (taylor-alts altns))

  (timeline-push! 'outputs (map ~a approxs))
  (timeline-push! 'count (length altns) (length approxs))
  (lower-approximations approxs))

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
  (define exprs (map alt-expr altns))
  (define reprs (map (curryr repr-of (*context*)) exprs))
  (timeline-push! 'inputs (map ~a exprs))
  (define batch (progs->batch exprs))
  (define runner (make-egg-runner batch (batch-roots batch) reprs schedule #:context (*context*)))
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

  ; Starting alternatives
  (define start-altns
    (for/list ([expr (in-list exprs)])
      (define repr (repr-of expr (*context*)))
      (alt expr (list 'patch expr repr) '() '())))

  ; Series expand
  (define approximations (if (flag-set? 'generate 'taylor) (run-taylor start-altns) '()))
  ; Recursive rewrite
  (define rewritten (if (flag-set? 'generate 'rr) (run-rr start-altns global-batch) '()))

  ; deref everything in rewritten
  (set! rewritten
        (for/list ([r (in-list rewritten)])
          (alt (batchref->expr (alt-expr r)) (alt-event r) (alt-prevs r) (alt-preprocessing r))))

  (append approximations rewritten))
