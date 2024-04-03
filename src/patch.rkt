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

(provide
  (contract-out
   [patch-table-has-expr? (-> expr? boolean?)]
   [patch-table-run (-> (listof expr?) (listof expr?) (listof alt?))]))

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

(define-resetter *real-rules*
  (λ () #f)
  (λ () #f))

(define (remove-literals expr)
  (match expr
    [(list op args ...) (cons op (map remove-literals args))]
    [(? literal?) (literal-value expr)]
    [_ expr]))

;; Spec contains no accelerators
(define (spec-has-accelerator? spec)
  (match spec
    [(list (? accelerator?) _ ...) #t]
    [(list _ args ...) (ormap spec-has-accelerator? args)]
    [_ #f]))

(define (run-rr altns)
  (timeline-event! 'rewrite)
  (define exprs (map alt-expr altns))
  (define real-exprs
    (reap [sow]
      (for ([expr (in-list exprs)])
        (match expr
          [(list 'if _ _ _) (sow expr)]
          [(list op _ ...)
           (when (eq? (operator-info op 'otype) 'real)
             (sow expr))]
          [_ (sow expr)]))))

  ; generate real rules is not cached
  (unless (*real-rules*)
    (*real-rules*
      (filter-not
        (lambda (rule)
          (or (representation? (rule-otype rule))
              (spec-has-accelerator? (rule-input rule))
              (spec-has-accelerator? (rule-output rule))))
        (*rules*))))

  ; rewrite using real rules
  (define changelistss
    (rewrite-expressions real-exprs
                         (*context*)
                         #:rules (*real-rules*)))

  ; apply changelists
  (define rewritten
    (reap [sow]
      (for ([changelists changelistss] [altn altns])
        (for ([cl changelists])
          (match-define (list subexpr input) cl)
          (sow (alt (remove-literals subexpr) (list 'rr input #f #f) (list altn) '()))))))

  (timeline-push! 'count (length altns) (length rewritten))
  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lowering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resetter *lowering-rules*
  (λ () #f)
  (λ () #f))

(define (gen-lowering-rules!)
  (define impls (list->set (platform-impls (*active-platform*))))
  ; direct lowering: each operator has a set of implementations
  (define rules
    (for/list ([op (in-list (all-operators))] #:unless (accelerator? op)
               [impl (in-list (operator-all-impls op))] #:when (set-member? impls impl))
      (define vars (map (lambda _ (gensym)) (operator-info op 'itype)))
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
  ; set cache
  (*lowering-rules* (append rules accelerator-rules)))

(define (merge-bindings bind1 bind2)
  (for/fold ([bind bind1]) ([(k v) (in-dict bind2)])
    (dict-update bind k
                 (λ (x)
                   (unless (eq? x v)
                     (error 'merge-bindings
                            "pattern variable has conflicting representations: ~a => ~a ~a"
                            k x v))
                   v)
                 v)))

;; Computes a map from pattern variable to representation
(define (pattern-reprs pattern repr)
  (let loop ([pattern pattern])
    (match pattern
      [(? number?) '()]
      [(? symbol? pvar) (list (cons pvar repr))]
      [(list 'if cond ift iff)
       (merge-bindings
         (pattern-reprs cond (get-representation 'bool))
         (merge-bindings (loop ift) (loop iff)))]
      [(list op args ...)
       (for/fold ([binds '()])
                 ([arg (in-list args)]
                  [itype (in-list (impl-info op 'itype))])
         (merge-bindings binds (pattern-reprs arg itype)))])))

(define (lower-spec spec rules context)
  (let loop ([spec spec] [repr (context-repr context)])
    (match spec
      [(? symbol?) (list spec)]
      [(? number?) (list (literal spec (representation-name repr)))]
      [(list 'if cond ift iff)
       (for*/list ([cond (loop cond (get-representation 'bool))]
                   [ift (loop ift repr)]
                   [iff (loop iff repr)])
         (list 'if cond ift iff))]
      [(list _ _ ...)
       (reap [sow]
         (for ([rule (in-list rules)])
           (define-values (lhs rhs) (values (rule-input rule) (rule-output rule)))
           (define bindings (pattern-match lhs spec))
           (when (and bindings (eq? (impl-info (car rhs) 'otype) repr))
             (define vars (map car bindings))
             (define types (pattern-reprs rhs repr))
             (define arg-choices (map loop (map cdr bindings) (map (curry dict-ref types) vars)))
             (for ([args (in-list (apply cartesian-product arg-choices))])
               (sow (pattern-substitute rhs (map cons vars args)))))))])))


(define (run-lowering altns)
  ; generate lowering rules if not cached
  (unless (*lowering-rules*)
    (gen-lowering-rules!))
  ; run lowering algorithm
  (define specs (map alt-expr altns))
  (for/list ([spec (in-list specs)]
             [altn (in-list altns)] #:when #t
             [expr (lower-spec spec (*lowering-rules*) (*context*))])
    (alt expr (list 'lower) (list altn) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-has-expr? expr)
  (hash-has-key? (*patch-table*) expr))

(define (patch-table-run hi-err-specs lo-err-specs)
  (define specs (append hi-err-specs lo-err-specs)) ; who cares any more
  (define uncached
    (for/list ([spec (in-list specs)] #:unless (patch-table-has-expr? spec))
      (alt spec (list 'patch spec) '() '())))
  ;; Core
  (define approximations (run-taylor uncached))
  (define rewritten (run-rr uncached))
  (define new-specs (append approximations rewritten))
  (define altns (run-lowering new-specs))
  ;; Uncaching
  altns)
