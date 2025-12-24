#lang racket

(require "../utils/alternative.rkt"
         "programs.rkt"
         "egg-herbie.rkt"
         "../config.rkt")

(provide add-derivations)

(define (canonicalize-proof prog proof start-expr)
  ;; Proofs are on subexpressions; lift to full expression
  (and proof (map (curry replace-expression prog start-expr) proof)))

;; Adds proof information to alternatives.
;; After unbatchify-alts, start-expr and end-expr are regular expressions, not batchrefs
(define (add-derivations-to altn)
  (match altn
    ; recursive rewrite or simplify, both using egg
    ; start-expr and end-expr are the subexpressions that were transformed
    [(alt expr (list 'rr start-expr end-expr (? egg-runner? runner) #f) `(,prev))
     (define proof
       (and (not (flag-set? 'generate 'egglog)) (egraph-prove runner start-expr end-expr)))
     (define proof* (canonicalize-proof (alt-expr altn) proof start-expr))
     (alt expr `(rr ,start-expr ,end-expr ,runner ,proof*) (list prev))]

    ; everything else
    [_ altn]))

(define (add-derivations alts)
  (define cache (make-hash))
  (for/list ([altn (in-list alts)])
    ;; We need to cache this because we'll see the same alt several times
    (alt-map (lambda (altn) (hash-ref! cache altn (lambda () (add-derivations-to altn)))) altn)))
