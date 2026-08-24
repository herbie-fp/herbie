#lang racket

(require "../core/alternative.rkt"
         "../syntax/block.rkt"
         "../syntax/platform.rkt"
         "programs.rkt"
         "egg-herbie.rkt"
         "../config.rkt"
         "../syntax/syntax.rkt")

(provide add-derivations)

(define (copy-proof-specs spec-block expr)
  (match expr
    [(approx spec impl) (approx (block-add! spec-block spec) (copy-proof-specs spec-block impl))]
    [(list op args ...) (cons op (map (curry copy-proof-specs spec-block) args))]
    [_ expr]))

(define (canonicalize-proof block spec-block prog-v proof start-v)
  ;; Proofs are on subexpressions; lift to full expression
  ;; Returns a list of vals instead of expressions
  (and proof
       (for/list ([step (in-list proof)])
         (define step-v (block-add! block (copy-proof-specs spec-block step)))
         (block-replace-subexpr block prog-v start-v step-v))))

;; Adds proof information to alternatives.
;; start-expr and end-expr are vals
(define (add-derivations-to altn)
  (match altn
    ; recursive rewrite or simplify, both using egg
    ; start-v and end-v are vals for the subexpressions that were transformed
    [(alt expr (list 'rr start-v end-v (? egg-runner? runner) #f) `(,prev))
     (define block (val-block expr))
     (define spec-block (egg-runner-block runner))
     (define-values (proof-start proof-end)
       (apply values (block-to-spec! block spec-block (list start-v end-v))))
     (define proof
       (and (not (flag-set? 'generate 'egglog)) (egraph-prove runner proof-start proof-end)))
     (define proof* (canonicalize-proof block spec-block (alt-expr altn) proof start-v))
     (alt expr `(rr ,start-v ,end-v ,runner ,proof*) (list prev))]

    ; everything else
    [_ altn]))

(define (add-derivations alts)
  (define cache (make-hash))
  (for/list ([altn (in-list alts)])
    ;; We need to cache this because we'll see the same alt several times
    (alt-map (lambda (altn) (hash-ref! cache altn (lambda () (add-derivations-to altn)))) altn)))
