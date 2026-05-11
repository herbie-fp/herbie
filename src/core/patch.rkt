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
         "taylor.rkt"
         "chebyshev.rkt")

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

(struct taylor-approx (spec repr impl-spec name var order prev) #:transparent)

(define (taylor-alts altns global-batch spec-batch reducer)
  (define vars
    (for/list ([var (in-list (batch-vars global-batch))]
               [repr (in-list (batch-var-reprs global-batch))]
               #:when (equal? (representation-type repr) 'real))
      var))
  (define brfs (map alt-expr altns))
  (define reprs (map batch-repr-of brfs))
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
          (for ([spec-brf (in-list spec-brfs)]
                [repr (in-list reprs)]
                [altn (in-list altns)]
                #:when (equal? (representation-type repr) 'real))
            (define genexpr0 (batch-add! global-batch 0))
            (sow (taylor-approx spec-brf repr genexpr0 'zero 'undef-var -1 altn)))

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
                (define genexpr-brf (copier (genexpr)))
                (sow (taylor-approx spec-brf repr genexpr-brf name var i altn))))
            (set! idx (add1 idx))
            (timeline-stop!)))))

(define (run-taylor altns global-batch spec-batch reducer [pcontext #f] [source-brfs '()])
  (timeline-event! 'series)
  (define (taylor-key x)
    (taylor-approx-impl-spec x))
  (define (approx-key x)
    (approx-impl (deref (alt-expr x))))

  (define taylor-approxs (taylor-alts altns global-batch spec-batch reducer))
  (define chebyshev-approxs
    (chebyshev-alts altns global-batch spec-batch reducer pcontext source-brfs taylor-approx))
  (define approxs (remove-duplicates (append taylor-approxs chebyshev-approxs) #:key taylor-key))
  (define approxs* (remove-duplicates (run-lowering approxs global-batch) #:key approx-key))

  (timeline-push! 'inputs (batch->jsexpr global-batch (map alt-expr altns)))
  (timeline-push! 'outputs (batch->jsexpr global-batch (map alt-expr approxs*)))
  (timeline-push! 'count (length altns) (length approxs*))
  approxs*)

(define (run-lowering taylors global-batch)
  (define schedule '(lower))

  ; run egg
  (define-values (specs impl-specs reprs names vars orders prevs)
    (for/lists (specs impl-specs reprs names vars orders prevs)
               ([taylor (in-list taylors)])
               (values (taylor-approx-spec taylor)
                       (taylor-approx-impl-spec taylor)
                       (taylor-approx-repr taylor)
                       (taylor-approx-name taylor)
                       (taylor-approx-var taylor)
                       (taylor-approx-order taylor)
                       (taylor-approx-prev taylor))))

  (define runner
    (cond
      [(flag-set? 'generate 'egglog)
       (define batch* (batch-empty (*context*)))
       (define copy-f (batch-copy-only! batch* global-batch))
       (define impl-specs* (map copy-f impl-specs))
       (make-egglog-runner batch* impl-specs* schedule (*context*))]
      [else (make-egraph global-batch impl-specs schedule (*context*))]))

  (define batchrefss
    (if (flag-set? 'generate 'egglog)
        (run-egglog runner global-batch reprs 'taylor #:extract 1)
        (egraph-best runner global-batch reprs)))

  ; apply changelists
  (reap [sow]
        (for ([batchrefs (in-list batchrefss)]
              [spec (in-list specs)]
              [name (in-list names)]
              [var (in-list vars)]
              [order (in-list orders)]
              [prev (in-list prevs)])
          (for ([batchref* (in-list batchrefs)])
            (define brf (batch-add! global-batch (approx spec batchref*)))
            (define taylor-altn (alt brf `(taylor ,name ,var ,order) (list prev)))
            (sow (alt brf (list 'rr runner #f) (list taylor-altn)))))))

(define (run-evaluate altns global-batch)
  (timeline-event! 'sample)
  (define all-brfs (map alt-expr altns))
  (define spec-brfs (batch-to-spec! global-batch all-brfs))
  (define free-vars (batch-free-vars global-batch))
  (define real-pairs
    (for/list ([altn (in-list altns)]
               [spec-brf (in-list spec-brfs)]
               #:when (set-empty? (free-vars spec-brf))
               #:unless (literal? (deref (alt-expr altn)))
               #:when (equal? (representation-type (batch-repr-of (alt-expr altn))) 'real))
      (cons altn spec-brf)))
  (define real-altns (map car real-pairs))
  (define real-spec-brfs (map cdr real-pairs))

  (define brfs (map alt-expr real-altns))
  (define reprs (map batch-repr-of brfs))
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
               [repr (in-list reprs)]
               #:when (equal? status 'valid))
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
  (define schedule '(rewrite unsound lower))

  (define brfs (map alt-expr altns))
  (define spec-brfs (batch-to-spec! global-batch brfs))
  (define reprs (map batch-repr-of brfs))
  (define runner
    (cond
      [(flag-set? 'generate 'egglog)
       (define batch* (batch-empty (*context*)))
       (define copy-f (batch-copy-only! batch* global-batch))
       (define brfs* (map copy-f spec-brfs))
       (make-egglog-runner batch* brfs* schedule (*context*))]
      [else (make-egraph global-batch spec-brfs schedule (*context*))]))

  (define batchrefss
    (if (flag-set? 'generate 'egglog)
        (run-egglog runner global-batch reprs 'rewrite #:extract 1000000) ; "infinity"
        (egraph-variations runner global-batch reprs)))

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

(define (generate-candidates batch brfs spec-batch reducer [pcontext #f] [source-brfs brfs])
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
        (run-taylor start-altns batch spec-batch reducer pcontext source-brfs)
        '()))

  ; Recursive rewrite
  (define rewritten
    (if (flag-set? 'generate 'rr)
        (run-rr start-altns batch)
        '()))

  (remove-duplicates (append evaluations rewritten approximations)
                     #:key (λ (altn) (cons (alt-expr altn) (get-starting-expr altn)))))
