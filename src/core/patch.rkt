#lang racket

(require "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../core/alternative.rkt"
         "../utils/common.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/block.rkt"
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

(struct taylor-approx (spec repr impl-spec name var order prev) #:transparent)

(define (taylor-alts altns global-block spec-block reducer)
  (define vars
    (for/list ([var (in-list (block-vars global-block))]
               [repr (in-list (block-var-reprs global-block))]
               #:when (equal? (representation-type repr) 'real))
      var))
  (define vs (map alt-expr altns))
  (define reprs (map block-repr-of vs))
  ;; Specs
  (define spec-vs (block-to-spec! global-block spec-block vs))
  (define free-vars (map (block-free-vars spec-block) spec-vs))

  (reap [sow]
        (parameterize ([reduce reducer] ;; reduces over spec-block
                       [add (λ (x) (block-add! spec-block x))]) ;; adds to spec-block
          ;; Zero expansion
          (for ([spec-v (in-list spec-vs)]
                [repr (in-list reprs)]
                [altn (in-list altns)]
                #:when (equal? (representation-type repr) 'real))
            (define genexpr0 (block-add! spec-block 0))
            (sow (taylor-approx spec-v repr genexpr0 'zero 'undef-var -1 altn)))

          ;; Taylor expansions
          ;; List<List<(cons offset coeffs)>>
          (define taylor-coeffs (taylor-coefficients spec-block spec-vs vars transforms-to-try))
          (define idx 0)
          (for* ([var (in-list vars)]
                 [transform-type transforms-to-try])
            (match-define (list name f finv) transform-type)
            (define timeline-stop! (timeline-start! 'series (~a var) (~a name)))
            (define taylor-coeffs* (list-ref taylor-coeffs idx))
            (define genexprs (approximate taylor-coeffs* spec-block var #:transform (cons f finv)))
            (for ([genexpr (in-list genexprs)]
                  [spec-v (in-list spec-vs)]
                  [repr (in-list reprs)]
                  [altn (in-list altns)]
                  [fv (in-list free-vars)]
                  #:when (set-member? fv var)) ;; check whether var exists in expr at all
              (for ([i (in-range (*taylor-order-limit*))])
                (sow (taylor-approx spec-v repr (genexpr) name var i altn))))
            (set! idx (add1 idx))
            (timeline-stop!)))))

(define (run-taylor altns global-block spec-block reducer)
  (timeline-event! 'series)
  (define (taylor-key x)
    (taylor-approx-impl-spec x))
  (define (approx-key x)
    (approx-impl (val-def (alt-expr x))))

  (define approxs
    (remove-duplicates (taylor-alts altns global-block spec-block reducer) #:key taylor-key))
  (define approxs*
    (remove-duplicates (run-lowering approxs global-block spec-block) #:key approx-key))
  (timeline-push! 'inputs (block->jsexpr global-block spec-block (map alt-expr altns)))
  (timeline-push! 'outputs (block->jsexpr global-block spec-block (map alt-expr approxs*)))
  (timeline-push! 'count (length altns) (length approxs*))
  approxs*)

(define (run-lowering taylors global-block spec-block)
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
      [(flag-set? 'generate 'egglog) (make-egglog-runner spec-block impl-specs schedule (*context*))]
      [else (make-egraph spec-block impl-specs schedule (*context*))]))

  (define valss
    (if (flag-set? 'generate 'egglog)
        (run-egglog runner global-block reprs 'taylor #:extract 1)
        (egraph-best runner global-block reprs)))

  ; apply changelists
  (reap [sow]
        (for ([vals (in-list valss)]
              [spec (in-list specs)]
              [name (in-list names)]
              [var (in-list vars)]
              [order (in-list orders)]
              [prev (in-list prevs)])
          (for ([val* (in-list vals)])
            (define v (block-add! global-block (approx spec val*)))
            (define taylor-altn (alt v `(taylor ,name ,var ,order) (list prev)))
            (sow (alt v (list 'rr runner #f) (list taylor-altn)))))))

(define (run-evaluate altns global-block spec-block)
  (timeline-event! 'sample)
  (define all-vs (map alt-expr altns))
  (define spec-vs (block-to-spec! global-block spec-block all-vs))
  (define constant-block (block-empty (context '() #f '())))
  (define copy-constant (block-copy-only! constant-block spec-block))
  (define constant-vs (map copy-constant spec-vs))
  (define free-vars (block-free-vars constant-block))
  (define real-pairs
    (for/list ([altn (in-list altns)]
               [constant-v (in-list constant-vs)]
               #:when (set-empty? (free-vars constant-v))
               #:unless (literal? (val-def (alt-expr altn)))
               #:when (equal? (representation-type (block-repr-of (alt-expr altn))) 'real))
      (cons altn constant-v)))
  (define real-altns (map car real-pairs))
  (define real-spec-vs (map cdr real-pairs))

  (define vs (map alt-expr real-altns))
  (define reprs (map block-repr-of vs))

  (define-values (status pts)
    (if (null? real-spec-vs)
        (values 'invalid #f)
        (let ([real-compiler (make-real-compiler constant-block real-spec-vs reprs)])
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
      (define v (block-add! global-block literal))
      (alt v '(evaluate) (list altn))))

  (timeline-push! 'inputs (block->jsexpr constant-block constant-block real-spec-vs))
  (timeline-push! 'outputs (map ~a literals))
  final-altns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-rr altns global-block spec-block)
  (timeline-event! 'rewrite)

  ; egg schedule (4-phases for mathematical rewrites, sound-X removal, and implementation selection)
  (define schedule '(rewrite unsound lower))

  (define vs (map alt-expr altns))
  (define spec-vs (block-to-spec! global-block spec-block vs))
  (define reprs (map block-repr-of vs))
  (define runner
    (cond
      [(flag-set? 'generate 'egglog) (make-egglog-runner spec-block spec-vs schedule (*context*))]
      [else (make-egraph spec-block spec-vs schedule (*context*))]))

  (define valss
    (if (flag-set? 'generate 'egglog)
        (run-egglog runner global-block reprs 'rewrite #:extract 1000000) ; "infinity"
        (egraph-variations runner global-block reprs)))

  ; apply changelists
  (define rewritten
    (reap [sow]
          (for ([vals (in-list valss)]
                [altn (in-list altns)])
            (for ([val* (in-list vals)])
              (sow (alt val* (list 'rr runner #f) (list altn)))))))

  (timeline-push! 'inputs (block->jsexpr global-block spec-block (map alt-expr altns)))
  (timeline-push! 'outputs (block->jsexpr global-block spec-block (map alt-expr rewritten)))
  (timeline-push! 'count (length altns) (length rewritten))

  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-starting-expr altn)
  (match (alt-prevs altn)
    [(list) (alt-expr altn)]
    [(list prev) (get-starting-expr prev)]))

(define (generate-candidates block vs spec-block reducer)
  ; Starting alternatives
  (define start-altns
    (for/list ([v vs])
      (alt v 'patch '())))

  (define evaluations
    (if (flag-set? 'generate 'evaluate)
        (run-evaluate start-altns block spec-block)
        '()))

  ; Series expand
  (define approximations
    (if (flag-set? 'generate 'taylor)
        (run-taylor start-altns block spec-block reducer)
        '()))

  ; Recursive rewrite
  (define rewritten
    (if (flag-set? 'generate 'rr)
        (run-rr start-altns block spec-block)
        '()))

  (remove-duplicates (append evaluations rewritten approximations)
                     #:key (λ (altn) (cons (alt-expr altn) (get-starting-expr altn)))))
