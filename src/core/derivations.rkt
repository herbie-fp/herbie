#lang racket

(require "../utils/alternative.rkt"
         "programs.rkt"
         "egg-herbie.rkt"
         "../config.rkt")

(provide add-derivations)

(define (canonicalize-proof prog proof loc)
  ;; Proofs are actually on subexpressions,
  ;; we need to construct the proof for the full expression
  (and proof (map (curry location-set loc prog) proof)))

;; Adds proof information to alternatives.
(define (add-derivations-to altn)
  (match altn
    ; recursive rewrite or simplify, both using egg
    [(alt expr (list 'rr loc (? egg-runner? runner) #f) `(,prev) preprocessing)
     (define start-expr (location-get loc (alt-expr prev)))
     (define end-expr (location-get loc expr))
     (define proof
       (and (not (flag-set? 'generate 'egglog)) (egraph-prove runner start-expr end-expr)))
     (define proof* (canonicalize-proof (alt-expr altn) proof loc))
     (alt expr `(rr ,loc ,runner ,proof*) `(,prev) preprocessing)]

    ; everything else
    [_ altn]))

(define (add-derivations alts)
  (define cache (make-hash))
  (for/list ([altn (in-list alts)])
    ;; We need to cache this because we'll see the same alt several times
    (alt-map (lambda (altn) (hash-ref! cache altn (lambda () (add-derivations-to altn)))) altn)))
