#lang racket

(require "../utils/alternative.rkt"
         "points.rkt"
         "programs.rkt"
         "egg-herbie.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt")

(provide add-derivations)

(define (canonicalize-proof prog proof loc pcontext ctx)
  (and proof
       ;; Proofs are actually on subexpressions,
       ;; we need to construct the proof for the full expression
       (for/list ([step (in-list proof)])
         (location-do loc prog (const step)))))

;; Computes a `equal?`-based hash table key for an alternative
(define (altn->key altn)
  (match altn
    [(alt expr `(rr ,loc ,method ,_) prevs _) (list expr (list 'rr loc method) (map alt-expr prevs))]
    [(alt expr `(simplify ,loc ,method ,_) prevs _)
     (list expr (list 'simplify loc method) (map alt-expr prevs))]
    [_ (error 'altn->key "unimplemented ~a" altn)]))

;; Creates two tables:
;;  - map from alternative to a pair (e, l ~> r) where `e` is an `egg-runner`
;;      and `l ~> r` is the rewrite we want a proof for.
;;  - map from egg query to list of proofs
(define (make-proof-tables altns)
  (define alt->query&rws (make-hash))
  (define query->rws (make-hash))

  (define (build! altn)
    (match altn
      ; recursive rewrite using egg (impl -> impl)
      [(alt expr `(rr ,loc ,(? egg-runner? runner) #f) `(,prev) _)
       (define start-expr (location-get loc (alt-expr prev)))
       (define end-expr (location-get loc expr))
       (define rewrite (cons start-expr end-expr))
       (hash-set! alt->query&rws (altn->key altn) (cons runner rewrite))
       (hash-update! query->rws runner (lambda (rws) (set-add rws rewrite)) '())]

      ; simplify using egg
      ;  usually: impl -> impl
      ;  taylor: spec -> approx (_, impl)
      [(alt expr `(simplify ,loc ,(? egg-runner? runner) #f) `(,prev) _)
       (define start-expr (location-get loc (alt-expr prev)))
       (define end-expr (location-get loc expr))
       (define rewrite (cons start-expr end-expr))
       (hash-set! alt->query&rws (altn->key altn) (cons runner rewrite))
       (hash-update! query->rws runner (lambda (rws) (set-add rws rewrite)) '())]

      ; everything else
      [_ (void)])

    altn)

  ; build the table
  (for ([altn (in-list altns)])
    (alt-for-each build! altn))
  (values alt->query&rws query->rws))

;; Runs proof extraction.
;; Result is a map from egg query to rewrites.
(define (compute-proofs query->rws)
  (for/hash ([(runner rws) (in-hash query->rws)])
    (define proofs (run-egg runner `(proofs . ,rws)))
    (values runner (map cons rws proofs))))

;; Lookups a proof based on an alternative
(define ((lookup-proof alt->query&rws query->proofs) altn)
  (match-define (cons runner rw) (hash-ref alt->query&rws (altn->key altn)))
  (cdr (assoc rw (hash-ref query->proofs runner))))

;; Adds proof information to alternatives.
(define (add-derivations-to altn pcontext ctx alt->proof)
  (match altn
    ; recursive rewrite or simplify, both using egg
    [(alt expr (list phase loc (? egg-runner? runner) #f) `(,prev) _)
     #:when (or (equal? phase 'simplify) (equal? phase 'rr))
     (define proof (canonicalize-proof (alt-expr altn) (alt->proof altn) loc pcontext ctx))
     (alt expr `(rr ,loc ,runner ,proof) `(,prev) '())]

    ; everything else
    [_ altn]))

(define (add-derivations alts pcontext ctx)
  (define-values (alt->query&rws query->rws) (make-proof-tables alts))
  (define query->proofs (compute-proofs query->rws))
  (define lookup-proc (lookup-proof alt->query&rws query->proofs))
  (for/list ([altn (in-list alts)])
    (alt-map (curryr add-derivations-to pcontext ctx lookup-proc) altn)))
