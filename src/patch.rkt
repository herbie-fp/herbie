#lang racket

(require "syntax/rules.rkt"
         "syntax/syntax.rkt"
         "syntax/types.rkt"
         "core/matcher.rkt"
         "core/rr.rkt"
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
  (define expr (expand-accelerators (*rules*) (alt-expr altn)))
  (reap [sow]
    (for* ([var (free-variables expr)] [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define timeline-stop! (timeline-start! 'series (~a expr) (~a var) (~a name)))
      (define genexpr (approximate expr var #:transform (cons f finv)))
      (for ([_ (in-range (*taylor-order-limit*))])
        (sow (alt (genexpr) `(taylor ,name ,var) (list altn) '())))
      (timeline-stop!))))

(define (run-taylor altns)
  (timeline-event! 'series)
  (timeline-push! 'inputs (map ~a altns))
  (define approximations
    (reap [sow]
      (for ([altn (in-list altns)])
        (for ([approximation (taylor-alt altn)])
          (sow approximation)))))
  (timeline-push! 'outputs (map ~a approximations))
  (timeline-push! 'count (length altns) (length approximations))
  approximations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rules from spec to spec
(define-resetter *real-rules*
  (λ () #f)
  (λ () #f))

;; Spec contains no accelerators
(define (spec-has-accelerator? spec)
  (match spec
    [(list (? accelerator?) _ ...) #t]
    [(list _ args ...) (ormap spec-has-accelerator? args)]
    [_ #f]))

(define (gen-real-rules!)
  (*real-rules*
    (filter-not
      (lambda (rule)
        (or (representation? (rule-otype rule))
            (spec-has-accelerator? (rule-input rule))
            (spec-has-accelerator? (rule-output rule))))
      (*rules*))))

;; Rules from spec to impl
(define-resetter *lowering-rules*
  (λ () #f)
  (λ () #f))

(define (gen-lowering-rules!)
  (define impls (list->set (platform-impls (*active-platform*))))
  ; direct lowering: each operator has a set of implementations
  (define rules
    (for/list ([op (in-list (all-operators))] #:unless (accelerator? op)
               [impl (in-list (operator-all-impls op))] #:when (set-member? impls impl))
      (define vars (map (lambda (_) (gensym)) (operator-info op 'itype)))
      (rule (sym-append op '-lowering- impl)
            (cons op vars)
            (cons impl vars)
            #f
            #f)))
  ; accelerator lowering
  (define accelerator-rules
    (for/list ([op (in-list (all-operators))]
               #:when (accelerator? op)
               [impl (in-list (operator-all-impls op))]
               #:when (set-member? impls impl))
      (rule (sym-append 'accelerator-lowering- impl)
            (accelerator-info op 'body)
            (cons impl (accelerator-info op 'vars))
            #f
            #f)))
  ; put them together
  (*lowering-rules* (append rules accelerator-rules)))

(define (impl-in-platform? prog pform)
  (define impls (platform-impls pform))
  (let/ec return
    (let loop ([prog prog])
      (match prog
        [(list 'if cond ift iff)
         (loop cond)
         (loop ift)
         (loop iff)]
        [(list (? operator-exists?) _ ...)
         (return #f)]
        [(list impl args ...)
         (unless (set-member? impls impl)
           (return #f))
         (for-each loop args)]
        [(? literal?) (void)]
        [(? symbol?) (void)]))
    (return #t)))

(define (run-rr altns reprs)
  (timeline-event! 'rewrite)
  (define exprs (map alt-expr altns))

  ; generate real rules is not cached
  (unless (*real-rules*)
    (gen-real-rules!))

  ; generate lowering rules if not cached
  (unless (*lowering-rules*)
    (gen-lowering-rules!))

  ; egg schedule (2-phases for real rewrites and implementation selection)
  (define schedule
    `((run ,(*real-rules*) ((node . ,(*node-limit*))))
      (run ,(*lowering-rules*) ((iteration . 1) (scheduler . simple)))
      (convert)
      (prune-spec)))
  (define changelistss
    (rewrite-expressions exprs reprs schedule (*context*)))

  ; apply changelists
  (define num-rewritten 0)
  (define rewritten
    (reap [sow]
      (for ([changelists changelistss] [altn altns])
        (for ([cl changelists])
          (match-define (list subexpr input) cl)
          (set! num-rewritten (add1 num-rewritten))
          (when (impl-in-platform? subexpr (*active-platform*))
            (sow (alt subexpr (list 'rr input #f #f) (list altn) '())))))))

  (printf "valid ~a/~a\n" (length rewritten) num-rewritten)

  (timeline-push! 'count (length altns) (length rewritten))
  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-has-expr? expr)
  (hash-has-key? (*patch-table*) expr))

(define (patch-table-run locs)
  ; Starting alternatives
  (define start-altns
    (for/list ([(spec repr) (in-dict locs)])
      (alt spec (list 'patch spec repr) '() '())))
  ; Core
  ; (define approximation (run-taylor uncached))
  (define reprs (map cdr locs))
  (define altns (run-rr start-altns reprs))

  ; (define uncached
  ;   (for/list ([spec (in-list specs)] #:unless (patch-table-has-expr? spec))
  ;     (alt spec (list 'patch spec) '() '())))
  ; ;; Core
  ; (define approximation (run-taylor uncached))
  ; (define altns (run-rr (append uncached approximation)))
  ;; Uncaching
  altns)
