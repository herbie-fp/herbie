#lang racket

(require "syntax/rules.rkt"
         "syntax/sugar.rkt"
         "syntax/syntax.rkt"
         "syntax/types.rkt"
         "core/egg-herbie.rkt"
         "core/rr.rkt"
         "core/simplify.rkt"
         "core/taylor.rkt"
         "accelerator.rkt"
         "alternative.rkt"
         "common.rkt"
         "platform.rkt"
         "programs.rkt"
         "timeline.rkt")

(provide patch-table-has-expr?
         patch-table-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Patch table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resetter *patch-table*
  (λ () (make-hash))
  (λ () (make-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Taylor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))]
        [exp-x (λ (x) `(exp ,x))]
        [log-x (λ (x) `(log ,x))]
      	[ninvert-x (λ (x) `(/ 1 (neg ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

(define (taylor-alt altn)
  (define expr (expand-accelerators (prog->spec (alt-expr altn))))
  (reap [sow]
    (for* ([var (free-variables expr)] [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define timeline-stop! (timeline-start! 'series (~a expr) (~a var) (~a name)))
      (define genexpr (approximate expr var #:transform (cons f finv)))
      (for ([_ (in-range (*taylor-order-limit*))])
        (sow (alt (genexpr) `(taylor ,name ,var) (list altn) '())))
      (timeline-stop!))))

(define (spec-has-nan? expr)
  (expr-contains?
    expr
    (lambda (term)
      (and (symbol? term)
           (eq? term 'NAN)))))

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

(define (impl-well-typed? prog repr)
  (define impls (list->set (platform-impls (*active-platform*))))
  (let/ec return
    (let loop ([prog prog] [repr repr])
      (match prog
        [(? number?) (return #f)]
        [(? symbol?) (void)]
        [(? literal?) (void)]
        [(list 'if cond ift iff)
         (loop cond (get-representation 'bool))
         (loop ift repr)
         (loop iff repr)]
        [(list (? operator-exists?) _ ...)
         (return #f)]
        [(list impl args ...)
         (unless (set-member? impls impl)
           (return #f))
         (unless (eq? (impl-info impl 'otype) repr)
           (return #f))
         (for-each loop args (impl-info impl 'itype))]))
    (return #t)))

(define (run-rr altns&reprs)
  (timeline-event! 'rewrite)
  (define altns (map car altns&reprs))
  (define reprs (map cdr altns&reprs))

  ; generate required rules
  (define rules (real-rules (*rules*)))
  (define lowering-rules (platform-lowering-rules))

  ; egg schedule (2-phases for real rewrites and implementation selection)
  (define schedule
    `((run ,rules ((node . ,(*node-limit*))))
      (run ,lowering-rules ((iteration . 1) (scheduler . simple)))))
  
  ; run egg
  (define specs (map alt-expr altns))
  (define changelistss (rewrite-expressions specs reprs schedule (*context*)))

  ; apply changelists
  (define rewritten
    (reap [sow]
      (for ([changelists changelistss] [altn altns] [repr reprs])
        (for ([cl changelists])
          (match-define (list subexpr input) cl)
          (when (impl-well-typed? subexpr repr)
            (sow (alt subexpr (list 'rr input #f #f) (list altn) '())))))))

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

  ; egg schedule (2-phases for real rewrites and implementation selection)
  (define schedule
    `((run ,rules ((node . ,(*node-limit*))))
      (run ,lowering-rules ((iteration . 1) (scheduler . simple)))))

  ; egg runner
  (define specs (map alt-expr altns))
  (define egg-query
    (make-egg-query specs
                    reprs
                    schedule
                    #:extractor (typed-egg-extractor platform-egg-cost-proc)))
  
  ; convert to altns
  (define simplification-options (simplify-batch egg-query))
  (define simplified
    (reap [sow]
      (for ([altn (in-list altns)] [repr (in-list reprs)]
            [outputs (in-list simplification-options)] #:when #t
            [output (in-list outputs)])
        (when (impl-well-typed? output repr)
          (sow (alt output `(simplify ,egg-query #f #f) (list altn) '()))))))
  
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
      (define spec (expand-accelerators (*rules*) (prog->spec expr)))
      (alt spec (list 'patch expr repr) '() '())))
  ; Core
  (define approximations (run-taylor start-altns reprs))
  (define altns
    (append
      (run-rr (map cons start-altns reprs))
      (run-simplify approximations)))
  ;; Uncaching
  altns)
