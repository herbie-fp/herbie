#lang racket

(require "../utils/alternative.rkt"
         "programs.rkt"
         "egg-herbie.rkt"
         "../config.rkt")

(provide add-derivations)

(define (canonicalize-proof prog proof start-expr)
  ;; Proofs are actually on subexpressions,
  ;; we need to construct the proof for the full expression by replacing
  ;; all occurrences of each proof step with the next
  (and proof
       (let loop ([proof proof]
                  [current start-expr])
         (match proof
           ['() '()]
           [(cons step rest) (cons (replace-expression prog current step) (loop rest step))]))))

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
