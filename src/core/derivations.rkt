#lang racket

(require "../utils/alternative.rkt"
         "batch.rkt"
         "programs.rkt"
         "egg-herbie.rkt"
         "../config.rkt")

(provide add-derivations)

(define (canonicalize-proof batch prog-brf proof start-brf)
  ;; Proofs are on subexpressions; lift to full expression
  (define exprs (batch-exprs batch))
  (define prog (exprs prog-brf))
  (define start-expr (exprs start-brf))
  (and proof (map (curry replace-expression prog start-expr) proof)))

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
