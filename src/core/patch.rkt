#lang racket

(require "rules.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "egg-herbie.rkt"
         "rr.rkt"
         "simplify.rkt"
         "taylor.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../syntax/platform.rkt"
         "programs.rkt"
         "../utils/timeline.rkt")

(provide patch-table-has-expr?
         patch-table-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Patch table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resetter *patch-table* (λ () (make-hash)) (λ () (make-hash)))

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

(define (run-taylor altns reprs)
  (timeline-event! 'series)
  (timeline-push! 'inputs (map ~a altns))
  (define approximations
    (reap [sow]
          (for ([altn (in-list altns)] [repr (in-list reprs)])
            (for ([approximation (taylor-alt altn)])
              (unless (spec-has-nan? (alt-expr approximation))
                (sow (cons approximation repr)))))))
  (timeline-push! 'outputs (map (lambda (e&r) (~a (car e&r))) approximations))
  (timeline-push! 'count (length altns) (length approximations))
  approximations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-rr altns&reprs)
  (timeline-event! 'rewrite)
  (define altns (map car altns&reprs))
  (define reprs (map cdr altns&reprs))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simplify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-simplify altns&reprs)
  (timeline-event! 'simplify)
  (define altns (map car altns&reprs))
  (define reprs (map cdr altns&reprs))

  ; generate real rules
  (define rules (real-rules (*simplify-rules*)))
  (define lowering-rules (platform-lowering-rules))
  (define schedule
    (if (flag-set? 'generate 'simplify)
        ; if simplify enabled, 2-phases for real rewrites and implementation selection
        `((,rules . ((node . ,(*node-limit*))))
          (,lowering-rules . ((iteration . 1) (scheduler . simple))))
        ; if disabled, only implementation selection
        `((,lowering-rules . ((iteration . 1) (scheduler . simple))))))
  ; egg runner (2-phases for real rewrites and implementation selection)
  (define runner
    (make-egg-runner (map alt-expr altns)
                     reprs
                     schedule))

  ; run egg
  (define simplification-options
    (simplify-batch runner
                    (typed-egg-extractor
                     (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc))))

  ; convert to altns
  (define simplified
    (reap [sow]
          (for ([altn (in-list altns)] [outputs (in-list simplification-options)])
            (match-define (cons _ simplified) outputs)
            (for ([expr (in-list simplified)])
              (sow (alt expr `(simplify ,runner #f #f) (list altn) '()))))))

  (timeline-push! 'count (length altns) (length simplified))
  simplified)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-has-expr? expr)
  (hash-has-key? (*patch-table*) expr))

(define (patch-table-run locs)
  ; Representations
  (define reprs (map (lambda (e) (repr-of e (*context*))) locs))
  ; Starting alternatives
  (define start-altns
    (for/list ([expr (in-list locs)] [repr (in-list reprs)])
      (alt expr (list 'patch expr repr) '() '())))
  ; Core
  (define approximations (if (flag-set? 'generate 'taylor) (run-taylor start-altns reprs) '()))
  (define rewritten (if (flag-set? 'generate 'rr) (run-rr (map cons start-altns reprs)) '()))
  (define simplified
    (run-simplify approximations))

  (define altns (append rewritten simplified))
  ;; Uncaching
  altns)
