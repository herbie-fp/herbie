#lang typed/racket

(require "../utils/alternative.rkt")

(require/typed "programs.rkt"
               [location-do (-> Loc Program (-> Program Program) Program)]
               [location-get (-> Loc Program Program)])

(require/typed "egg-herbie.rkt"
               [#:struct egg-runner
                ([batch : Any] [roots : Any]
                               [reprs : Any]
                               [schedule : Any]
                               [ctx : Any]
                               [new-roots : Any]
                               [egg-graph : Any])]
               [egraph-prove (-> egg-runner Program Program Proof)])

(provide add-derivations)

(define-type Program Any)
(define-type Proof (Listof Any))
(define-type Loc (Listof Natural))

(: canonicalize-proof (-> Program Proof Loc Proof))
(define (canonicalize-proof prog proof loc)
  (and proof
       ;; Proofs are actually on subexpressions,
       ;; we need to construct the proof for the full expression
       (for/list ([step (in-list proof)])
         (location-do loc prog (const step)))))

;; Adds proof information to alternatives.
(: add-derivations-to (-> alt alt))
(define (add-derivations-to altn)
  (match altn
    ; recursive rewrite or simplify, both using egg
    [(alt expr (list (or 'simplify 'rr) loc (? egg-runner? runner) #f) `(,prev) preprocessing)
     (define start-expr (location-get (cast loc Loc) (alt-expr prev)))
     (define end-expr (location-get (cast loc Loc) expr))
     (define proof (egraph-prove runner start-expr end-expr))
     (define proof* (canonicalize-proof (alt-expr altn) proof (cast loc Loc)))
     (alt expr `(rr ,loc ,runner ,proof*) `(,prev) preprocessing)]

    ; everything else
    [_ altn]))

(: add-derivations (-> (Listof alt) (Listof alt)))
(define (add-derivations alts)
  (: cache (HashTable alt alt))
  (define cache (make-hash))
  (for/list ([altn (in-list alts)])
    ;; We need to cache this because we'll see the same alt several times
    (alt-map (lambda (altn) (hash-ref! cache altn (lambda () (add-derivations-to altn)))) altn)))
