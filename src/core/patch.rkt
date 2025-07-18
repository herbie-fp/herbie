#lang racket

(require "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "batch.rkt"
         "egg-herbie.rkt"
         "egglog-herbie.rkt"
         "programs.rkt"
         "rules.rkt"
         "rival.rkt"
         "taylor.rkt")

(provide generate-candidates)

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
  (define roots (list->vector (map (compose batchref-idx alt-expr) altns)))
  (define reprs
    (for/list ([root (in-vector roots)])
      (repr-of-node global-batch root (*context*))))
  (define specs (map prog->spec (batch->progs global-batch)))
  (define free-vars (map free-variables specs))
  (define vars (context-vars (*context*)))

  (reap [sow]
        (define global-batch-mutable (batch->mutable-batch global-batch)) ; Create a mutable batch
        (for* ([var (in-list vars)]
               [transform-type transforms-to-try])
          (match-define (list name f finv) transform-type)
          (define timeline-stop! (timeline-start! 'series (~a var) (~a name)))
          (define genexprs (approximate specs var #:transform (cons f finv)))
          (for ([genexpr (in-list genexprs)]
                [spec (in-list specs)]
                [repr (in-list reprs)]
                [altn (in-list altns)]
                [fv (in-list free-vars)]
                #:when (set-member? fv var)) ; check whether var exists in expr at all
            (for ([i (in-range (*taylor-order-limit*))])
              (define gen (approx spec (hole (representation-name repr) (genexpr))))
              (define idx (mutable-batch-munge! global-batch-mutable gen)) ; Munge gen
              (sow (alt (batchref global-batch idx) `(taylor ,name ,var) (list altn) '()))))
          (timeline-stop!))
        (batch-copy-mutable-nodes! global-batch global-batch-mutable))) ; Update global-batch

(define (run-taylor altns global-batch)
  (timeline-event! 'series)
  (timeline-push! 'inputs (map ~a (batch->progs global-batch)))

  (define (key x)
    (approx-impl (deref (alt-expr x))))

  (define approxs (remove-duplicates (taylor-alts altns global-batch) #:key key))
  (define approxs* (run-lowering approxs global-batch))

  (timeline-push! 'outputs (map ~a (map (compose debatchref alt-expr) approxs*)))
  (timeline-push! 'count (length altns) (length approxs*))

  approxs*)

(define (run-lowering altns global-batch)
  (define schedule `((lower . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define roots (list->vector (map (compose batchref-idx alt-expr) altns)))
  (define reprs
    (for/list ([root (in-vector roots)])
      (repr-of-node global-batch root (*context*))))

  (define batch* (batch-remove-zombie global-batch roots))

  (define runner
    (if (flag-set? 'generate 'egglog)
        (make-egglog-runner batch* reprs schedule (*context*))
        (make-egraph batch* reprs schedule (*context*))))

  (define batchrefss
    (if (flag-set? 'generate 'egglog)
        (run-egglog-multi-extractor runner global-batch)
        (egraph-best runner global-batch)))

  ; apply changelists
  (reap [sow]
        (for ([batchrefs (in-list batchrefss)]
              [altn (in-list altns)])
          (for ([batchref* (in-list batchrefs)])
            (sow (alt batchref* (list 'rr runner #f) (list altn) '()))))))

(define (run-evaluate altns global-batch)
  (timeline-event! 'sample)
  (define free-vars (batch-free-vars global-batch))
  (define real-altns
    (for/list ([altn (in-list altns)]
               #:when (set-empty? (vector-ref free-vars (batchref-idx (alt-expr altn)))))
      altn))
  (define roots
    (for/vector ([altn (in-list real-altns)])
      (batchref-idx (alt-expr altn))))
  (define contexts
    (for/list ([root (in-vector roots)])
      (context '() (repr-of-node global-batch root (*context*)) '())))
  (define batch* (batch-remove-zombie global-batch roots))
  (define specs (map prog->spec (batch->progs batch*)))
  (timeline-push! 'inputs (map ~a specs))
  (define-values (status pts)
    (if (null? specs)
        (values 'invalid #f)
        (let ([real-compiler (make-real-compiler specs contexts)])
          (real-apply real-compiler (vector)))))
  (define literals
    (for/list ([pt (in-list (if (equal? status 'valid)
                                pts
                                '()))]
               [ctx (in-list contexts)]
               #:when (equal? status 'valid))
      (define repr (context-repr ctx))
      (literal (repr->real pt repr) (representation-name repr))))

  (timeline-push! 'outputs (map ~a literals))
  (define global-batch-mutable (batch->mutable-batch global-batch)) ; Create a mutable batch
  (define final-altns
    (for/list ([literal (in-list literals)]
               [altn (in-list real-altns)]
               #:when (equal? status 'valid))
      (define idx (mutable-batch-munge! global-batch-mutable literal))
      (alt (batchref global-batch idx) '(evaluate) (list altn) '())))
  (batch-copy-mutable-nodes! global-batch global-batch-mutable)
  final-altns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-rr altns global-batch)
  (timeline-event! 'rewrite)

  ; generate required rules
  (define rules (*rules*))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    (list `(lift . ((iteration . 1) (scheduler . simple)))
          `(,rules . ((node . ,(*node-limit*))))
          `(lower . ((iteration . 1) (scheduler . simple)))))

  (define roots (list->vector (map (compose batchref-idx alt-expr) altns)))
  (define reprs
    (for/list ([root (in-vector roots)])
      (repr-of-node global-batch root (*context*))))
  (define batch* (batch-remove-zombie global-batch roots))
  (timeline-push! 'inputs (map (compose ~a debatchref alt-expr) altns))

  (define runner
    (if (flag-set? 'generate 'egglog)
        (make-egglog-runner batch* reprs schedule (*context*))
        (make-egraph batch* reprs schedule (*context*))))

  (define batchrefss
    (if (flag-set? 'generate 'egglog)
        (run-egglog-multi-extractor runner global-batch)
        (egraph-variations runner global-batch)))

  ; apply changelists
  (define rewritten
    (reap [sow]
          (for ([batchrefs (in-list batchrefss)]
                [altn (in-list altns)])
            (for ([batchref* (in-list batchrefs)])
              (sow (alt batchref* (list 'rr runner #f) (list altn) '()))))))

  (timeline-push! 'outputs (map (compose ~a debatchref alt-expr) rewritten))
  (timeline-push! 'count (length altns) (length rewritten))

  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-candidates global-batch)
  ; Starting alternatives
  (define start-altns
    (for/list ([root (in-vector (batch-roots global-batch))])
      (alt (batchref global-batch root) 'patch '() '())))

  (define evaluations
    (if (flag-set? 'generate 'evaluate)
        (run-evaluate start-altns global-batch)
        '()))

  ; Series expand
  (define approximations
    (if (flag-set? 'generate 'taylor)
        (run-taylor start-altns global-batch)
        '()))

  ; Recursive rewrite
  (define rewritten
    (if (flag-set? 'generate 'rr)
        (run-rr start-altns global-batch)
        '()))

  (remove-duplicates (append evaluations rewritten approximations) #:key alt-expr))
