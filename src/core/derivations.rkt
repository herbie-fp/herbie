#lang racket

(require "../core/alternative.rkt"
         "../syntax/batch.rkt"
         "programs.rkt"
         "egg-herbie.rkt"
         "../config.rkt")

(provide add-derivations)

(define (canonicalize-proof batch prog-brf proof start-brf)
  ;; Proofs are on subexpressions; lift to full expression
  ;; Returns a list of batchrefs instead of expressions
  (and proof
       (for/list ([step (in-list proof)])
         (define step-brf (batch-add! batch step))
         (batch-replace-subexpr batch prog-brf start-brf step-brf))))

;; Adds proof information to alternatives.
;; start-expr and end-expr are batchrefs
(define (add-derivations-to altn)
  (match altn
    ; recursive rewrite or simplify, both using egg
    ; start-brf and end-brf are batchrefs for the subexpressions that were transformed
    [(alt expr (list 'rr start-brf end-brf (? egg-runner? runner) #f) `(,prev))
     (define batch (egg-runner-batch runner))
     (define proof (and (not (flag-set? 'generate 'egglog)) (egraph-prove runner start-brf end-brf)))
     (define proof* (canonicalize-proof batch (alt-expr altn) proof start-brf))
     (alt expr `(rr ,start-brf ,end-brf ,runner ,proof*) (list prev))]

    ; everything else
    [_ altn]))

(define (add-derivations alts)
  (define cache (make-hash))
  (for/list ([altn (in-list alts)])
    ;; We need to cache this because we'll see the same alt several times
    (alt-map (lambda (altn) (hash-ref! cache altn (lambda () (add-derivations-to altn)))) altn)))
