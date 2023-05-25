#lang racket

(require "alternative.rkt" "points.rkt" "programs.rkt"
         "core/egg-herbie.rkt" "core/simplify.rkt" "syntax/types.rkt" 
         "core/matcher.rkt" "syntax/rules.rkt")

(provide add-soundiness)

(define (canonicalize-rewrite proof)
  (match proof
    [`(Rewrite=> ,rule ,something)
     (list 'Rewrite=> (get-canon-rule-name rule rule) something)]
    [`(Rewrite<= ,rule ,something)
     (list 'Rewrite<= (get-canon-rule-name rule rule) something)]
    [(list _ ...)
     (map canonicalize-rewrite proof)]
    [else proof]))

(define (get-proof-errors proof pcontext ctx)
  (define proof-exprs (map remove-rewrites proof))
  (define proof-errors (batch-errors proof-exprs pcontext ctx))
  (define proof-diffs
    (cons (list 0 0)
          (for/list ([prev proof-errors] [current (rest proof-errors)])
            (define num-increase (count > current prev))
            (define num-decrease (count < current prev))
            (list num-increase num-decrease (length prev)))))
  proof-diffs)

(define (canonicalize-proof prog loc variants? e-input p-input)
  (match-define 
   (cons variants proof)
   (run-egg e-input variants?
            #:proof-input p-input
            #:proof-ignore-when-unsound? #t))
  (cond
   [proof
    ;; Proofs are actually on subexpressions,
    ;; we need to construct the proof for the full expression
    (define proof*
      (for/list ([step (in-list proof)])
        (location-do loc prog (const (canonicalize-rewrite step)))))
    (define errors
      (get-proof-errors proof* pcontext ctx))
    (cons proof* errors)]
   [else
    (cons #f #f)]))

(define (add-soundiness-to pcontext ctx cache altn)
  (match altn

    [(alt expr `(rr (2 ,@loc) ,(? egraph-query? e-input) #f #f) `(,prev))
     (define p-input (cons (location-get loc (alt-expr prev)) (location-get loc (alt-expr altn))))
     (match-define (cons proof errs)
       (hash-ref! cache (cons p-input e-input)
                  (λ () (canonicalize-proof (alt-expr altn) loc #t e-input p-input))))
     (alt expr `(rr (2 ,@loc) ,e-input ,proof ,errs) `(,prev))]

    [(alt expr `(rr (2 ,@loc) ,(? rule? input) #f #f) `(,prev))
     (match-define (cons proof errs)
       (hash-ref! cache (cons input expr)
                  (λ ()
                    (define proof
                      (list (alt-expr prev)
                            (list 'Rewrite=> (rule-name input) (alt-expr altn))))
                    (define errs
                      (get-proof-errors proof pcontext ctx))
                    (cons proof errs))))
     (alt expr `(rr (2 ,@loc) ,input ,proof ,errs) `(,prev))]

    ;; This is alt coming from simplify
    [(alt expr `(simplify (2 ,@loc) ,(? egraph-query? e-input) #f #f) `(,prev))
     (define p-input (cons (location-get loc (alt-expr prev)) (location-get loc (alt-expr altn))))
     (match-define (cons proof errs)
       (hash-ref! cache (cons p-input e-input)
                  (λ () (canonicalize-proof (alt-expr altn) loc #f e-input p-input))))
     (alt expr `(simplify (2 ,@loc) ,e-input ,proof ,errs) `(,prev))]

    [else altn]))

(define (add-soundiness alts pcontext ctx)
  (define cache (make-hash))
  (for/list ([altn alts])
    (alt-map (curry add-soundiness-to pcontext ctx cache) altn)))
