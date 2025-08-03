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
  (define brfs (map alt-expr altns))
  (define reprs
    (for/list ([brf brfs])
      (repr-of-batchref brf (*context*))))
  (define specs (map prog->spec (batch->progs global-batch brfs)))
  (define free-vars (map free-variables specs))
  (define vars (context-vars (*context*)))

  (reap [sow]
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
              (define brf (batch-add! global-batch gen)) ; Munge gen
              (sow (alt brf `(taylor ,name ,var) (list altn) '()))))
          (timeline-stop!))))

(define (run-taylor altns global-batch)
  (timeline-event! 'series)
  (define (key x)
    (approx-impl (deref (alt-expr x))))

  (define approxs (remove-duplicates (taylor-alts altns global-batch) #:key key))
  (define approxs* (run-lowering approxs global-batch))

  (define exprs (batch-reconstruct-exprs global-batch))
  (timeline-push! 'inputs (map (compose ~a exprs alt-expr) altns))
  (timeline-push! 'outputs (map (compose ~a exprs alt-expr) approxs*))
  (timeline-push! 'count (length altns) (length approxs*))
  approxs*)

(define (run-lowering altns global-batch)
  (define schedule `((lower . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define brfs (map alt-expr altns))
  (define reprs
    (for/list ([brf brfs])
      (repr-of-batchref brf (*context*))))

  (define-values (batch* brfs*) (batch-remove-zombie global-batch brfs))

  (define runner
    (if (flag-set? 'generate 'egglog)
        (make-egglog-runner batch* brfs* reprs schedule (*context*))
        (make-egraph batch* brfs* reprs schedule (*context*))))

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

  ; TODO: rewrite this
  (define brfs (map alt-expr real-altns))
  (define contexts
    (for/list ([brf brfs])
      (context '() (repr-of-batchref brf (*context*)) '())))

  (define-values (batch* brfs*) (batch-remove-zombie global-batch brfs))
  (define specs (map prog->spec (batch->progs batch* brfs*)))

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

  (define final-altns
    (for/list ([literal (in-list literals)]
               [altn (in-list real-altns)]
               #:when (equal? status 'valid))
      (define brf (batch-add! global-batch literal))
      (alt brf '(evaluate) (list altn) '())))

  (timeline-push! 'inputs (map ~a specs))
  (timeline-push! 'outputs (map ~a literals))
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

  ; TODO: rewrite this part
  (define brfs (map alt-expr altns))
  (define reprs
    (for/list ([brf brfs])
      (repr-of-batchref brf (*context*))))

  (define-values (batch* brfs*) (batch-remove-zombie global-batch brfs))

  (define runner
    (if (flag-set? 'generate 'egglog)
        (make-egglog-runner batch* brfs* reprs schedule (*context*))
        (make-egraph batch* brfs* reprs schedule (*context*))))

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

  (define exprs (batch-reconstruct-exprs global-batch))
  (timeline-push! 'inputs (map (compose ~a exprs alt-expr) altns))
  (timeline-push! 'outputs (map (compose ~a exprs alt-expr) rewritten))
  (timeline-push! 'count (length altns) (length rewritten))

  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-candidates batch brfs)
  ; Starting alternatives
  (define start-altns
    (for/list ([brf brfs])
      (alt brf 'patch '() '())))

  (define evaluations
    (if (flag-set? 'generate 'evaluate)
        (run-evaluate start-altns batch)
        '()))

  ; Series expand
  (define approximations
    (if (flag-set? 'generate 'taylor)
        (run-taylor start-altns batch)
        '()))

  ; Recursive rewrite
  (define rewritten
    (if (flag-set? 'generate 'rr)
        (run-rr start-altns batch)
        '()))

  (remove-duplicates (append evaluations rewritten approximations) #:key alt-expr))
