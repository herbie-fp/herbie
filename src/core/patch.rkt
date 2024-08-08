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
         "rr.rkt"
         "rules.rkt"
         "simplify.rkt"
         "taylor.rkt")

(provide patch-table-has-expr?
         patch-table-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Patch table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/reset *patch-table* (make-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simplify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lower-approximations approxs approx->prev)
  (timeline-event! 'simplify)

  (define reprs
    (for/list ([approx (in-list approxs)])
      (define prev (hash-ref approx->prev approx))
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
  (define runner (make-egg-runner (map alt-expr approxs) reprs schedule))
  (define simplification-options
    (simplify-batch runner
                    (typed-egg-extractor
                     (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc))))

  ; convert to altns
  (define simplified
    (reap [sow]
          (for ([altn (in-list approxs)] [outputs (in-list simplification-options)])
            (match-define (cons _ simplified) outputs)
            (define prev (hash-ref approx->prev altn))
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

(define (taylor-alt altn)
  (define expr (prog->spec (alt-expr altn)))
  (reap [sow]
        (for* ([var (free-variables expr)] [transform-type transforms-to-try])
          (match-define (list name f finv) transform-type)
          (define timeline-stop! (timeline-start! 'series (~a expr) (~a var) (~a name)))
          (define genexpr (approximate expr var #:transform (cons f finv)))
          (for ([_ (in-range (*taylor-order-limit*))])
            (sow (alt (genexpr) `(taylor ,name ,var) (list altn) '())))
          (timeline-stop!))))

(define (spec-has-nan? expr)
  (expr-contains? expr (lambda (term) (eq? term 'NAN))))

(define (run-taylor altns)
  (timeline-event! 'series)
  (timeline-push! 'inputs (map ~a altns))

  (define approx->prev (make-hasheq))
  (define approxs
    (reap [sow]
          (for ([altn (in-list altns)])
            (for ([approximation (taylor-alt altn)])
              (unless (spec-has-nan? (alt-expr approximation))
                (hash-set! approx->prev approximation altn)
                (sow approximation))))))

  (timeline-push! 'outputs (map ~a approxs))
  (timeline-push! 'count (length altns) (length approxs))
  (lower-approximations approxs approx->prev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-rr altns)
  (timeline-event! 'rewrite)

  ; generate required rules
  (define rules (real-rules (*rules*)))
  (define lifting-rules (platform-lifting-rules))
  (define lowering-rules (platform-lowering-rules))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
      (,rules . ((node . ,(*node-limit*))))
      (,lowering-rules . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define specs (map alt-expr altns))
  (define reprs (map (lambda (altn) (repr-of (alt-expr altn) (*context*))) altns))
  (define changelistss (rewrite-expressions specs reprs schedule (*context*)))

  ; apply changelists
  (define rewritten
    (reap [sow]
          (for ([changelists changelistss] [altn altns])
            (for ([cl changelists])
              (match-define (list subexpr input) cl)
              (sow (alt subexpr (list 'rr input #f #f) (list altn) '()))))))

  (timeline-push! 'count (length altns) (length rewritten))
  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-has-expr? expr)
  (hash-has-key? (*patch-table*) expr))

(define (patch-table-run locs)
  ; Starting alternatives
  (define start-altns
    (for/list ([expr (in-list locs)])
      (define repr (repr-of expr (*context*)))
      (alt expr (list 'patch expr repr) '() '())))
  ; Series expand
  (define approximations (if (flag-set? 'generate 'taylor) (run-taylor start-altns) '()))
  ; Recursive rewrite
  (define rewritten (if (flag-set? 'generate 'rr) (run-rr start-altns) '()))

  (append approximations rewritten))
