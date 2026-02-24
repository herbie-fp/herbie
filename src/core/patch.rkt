#lang racket

(require "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../core/alternative.rkt"
         "../utils/common.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/batch.rkt"
         "egg-herbie.rkt"
         "egglog-herbie.rkt"
         "programs.rkt"
         "rules.rkt"
         "../syntax/rival.rkt"
         "taylor.rkt")

(provide generate-candidates
         get-starting-expr)

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

(define (taylor-alts altns global-batch spec-batch reducer)
  (define vars (context-vars (*context*)))
  (define brfs (map alt-expr altns))
  (define reprs (map (batch-reprs global-batch (*context*)) brfs))
  ;; Specs
  (define spec-brfs (batch-to-spec! global-batch brfs)) ;; These specs will go into (approx spec impl)
  (define free-vars (map (batch-free-vars global-batch) spec-brfs))
  (define spec-brfs*
    (map (batch-copy-only! spec-batch global-batch)
         spec-brfs)) ;; copy from global-batch to spec-batch
  (define copier (batch-copy-only! global-batch spec-batch)) ;; copy from spec-batch to global-batch

  (reap [sow]
        (parameterize ([reduce reducer] ;; reduces over spec-batch
                       [add (λ (x) (batch-add! spec-batch x))]) ;; adds to spec-batch
          ;; Zero expansion
          (define genexpr0 (batch-add! global-batch 0))
          (define gen0 (approx (car spec-brfs) (hole (representation-name (car reprs)) genexpr0)))
          (define brf0 (batch-add! global-batch gen0))
          (sow (alt brf0 `(taylor zero undef-var) (list (car altns))))

          ;; Taylor expansions
          ;; List<List<(cons offset coeffs)>>
          (define taylor-coeffs (taylor-coefficients spec-batch spec-brfs* vars transforms-to-try))
          (define idx 0)
          (for* ([var (in-list vars)]
                 [transform-type transforms-to-try])
            (match-define (list name f finv) transform-type)
            (define timeline-stop! (timeline-start! 'series (~a var) (~a name)))
            (define taylor-coeffs* (list-ref taylor-coeffs idx))
            (define genexprs (approximate taylor-coeffs* spec-batch var #:transform (cons f finv)))
            (for ([genexpr (in-list genexprs)]
                  [spec-brf (in-list spec-brfs)]
                  [repr (in-list reprs)]
                  [altn (in-list altns)]
                  [fv (in-list free-vars)]
                  #:when (set-member? fv var)) ;; check whether var exists in expr at all
              (for ([i (in-range (*taylor-order-limit*))])
                ;; adding a new expansion to the global batch
                (define gen (approx spec-brf (hole (representation-name repr) (copier (genexpr)))))
                (define brf (batch-add! global-batch gen))
                (sow (alt brf `(taylor ,name ,var) (list altn)))))
            (set! idx (add1 idx))
            (timeline-stop!)))))

(define (run-taylor altns global-batch spec-batch reducer)
  (timeline-event! 'series)
  (define (key x)
    (define expr (deref (alt-expr x)))
    (if (approx? expr)
        (approx-impl expr)
        expr))

  (define approxs (remove-duplicates (taylor-alts altns global-batch spec-batch reducer) #:key key))
  (define approxs* (remove-duplicates (run-lowering approxs global-batch) #:key key))

  (timeline-push! 'inputs (batch->jsexpr global-batch (map alt-expr altns)))
  (timeline-push! 'outputs (batch->jsexpr global-batch (map alt-expr approxs*)))
  (timeline-push! 'count (length altns) (length approxs*))
  approxs*)

(define (run-lowering altns global-batch)
  (define schedule '(lower))

  ; run egg
  (define brfs (map alt-expr altns))
  (define reprs (map (batch-reprs global-batch (*context*)) brfs))

  (define runner
    (cond
      [(flag-set? 'generate 'egglog)
       (define batch* (batch-empty))
       (define copy-f (batch-copy-only! batch* global-batch))
       (define brfs* (map copy-f brfs))
       (make-egglog-runner batch* brfs* reprs schedule (*context*))]
      [else (make-egraph global-batch brfs reprs schedule (*context*))]))

  (define batchrefss
    (if (flag-set? 'generate 'egglog)
        (run-egglog runner global-batch 'taylor #:extract 1)
        (egraph-best runner global-batch)))

  ; apply changelists
  (reap [sow]
        (for ([batchrefs (in-list batchrefss)]
              [altn (in-list altns)])
          (for ([batchref* (in-list batchrefs)])
            (sow (alt batchref* (list 'rr runner #f) (list altn)))))))

(define (run-evaluate altns global-batch)
  (timeline-event! 'sample)
  (define all-brfs (map alt-expr altns))
  (define spec-brfs (batch-to-spec! global-batch all-brfs))
  (define free-vars (batch-free-vars global-batch))
  (define repr-of (batch-reprs global-batch (*context*)))
  (define real-pairs
    (for/list ([altn (in-list altns)]
               [spec-brf (in-list spec-brfs)]
               #:when (set-empty? (free-vars spec-brf))
               #:unless (literal? (deref (alt-expr altn)))
               #:when (equal? (representation-type (repr-of (alt-expr altn))) 'real))
      (cons altn spec-brf)))
  (define real-altns (map car real-pairs))
  (define real-spec-brfs (map cdr real-pairs))

  (define brfs (map alt-expr real-altns))
  (define reprs (map repr-of brfs))
  (define contexts
    (for/list ([repr (in-list reprs)])
      (context '() repr '())))

  (define-values (status pts)
    (if (null? real-spec-brfs)
        (values 'invalid #f)
        (let ([real-compiler (make-real-compiler global-batch real-spec-brfs contexts)])
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
      (alt brf '(evaluate) (list altn))))

  (timeline-push! 'inputs (batch->jsexpr global-batch real-spec-brfs))
  (timeline-push! 'outputs (map ~a literals))
  final-altns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-rr altns global-batch)
  (timeline-event! 'rewrite)

  ; egg schedule (4-phases for mathematical rewrites, sound-X removal, and implementation selection)
  (define schedule '(lift rewrite unsound lower))

  (define brfs (map alt-expr altns))
  (define reprs (map (batch-reprs global-batch (*context*)) brfs))

  (define runner
    (cond
      [(flag-set? 'generate 'egglog)
       (define batch* (batch-empty))
       (define copy-f (batch-copy-only! batch* global-batch))
       (define brfs* (map copy-f brfs))
       (make-egglog-runner batch* brfs* reprs schedule (*context*))]
      [else (make-egraph global-batch brfs reprs schedule (*context*))]))

  (define batchrefss
    (if (flag-set? 'generate 'egglog)
        (run-egglog runner global-batch 'rewrite #:extract 1000000) ; "infinity"
        (egraph-variations runner global-batch)))

  ; apply changelists
  (define rewritten
    (reap [sow]
          (for ([batchrefs (in-list batchrefss)]
                [altn (in-list altns)])
            (for ([batchref* (in-list batchrefs)])
              (sow (alt batchref* (list 'rr runner #f) (list altn)))))))

  (timeline-push! 'inputs (batch->jsexpr global-batch (map alt-expr altns)))
  (timeline-push! 'outputs (batch->jsexpr global-batch (map alt-expr rewritten)))
  (timeline-push! 'count (length altns) (length rewritten))

  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-starting-expr altn)
  (match (alt-prevs altn)
    [(list) (alt-expr altn)]
    [(list prev) (get-starting-expr prev)]))

(define (generate-candidates batch brfs spec-batch reducer)
  ; Starting alternatives
  (define start-altns
    (for/list ([brf brfs])
      (alt brf 'patch '())))

  (define evaluations
    (if (flag-set? 'generate 'evaluate)
        (run-evaluate start-altns batch)
        '()))

  ; Series expand
  (define approximations
    (if (flag-set? 'generate 'taylor)
        (run-taylor start-altns batch spec-batch reducer)
        '()))

  ; Recursive rewrite
  (define rewritten
    (if (flag-set? 'generate 'rr)
        (run-rr start-altns batch)
        '()))

  (remove-duplicates (append evaluations rewritten approximations)
                     #:key (λ (altn) (cons (alt-expr altn) (get-starting-expr altn)))))
