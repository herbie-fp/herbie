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

(define (canonicalize-proof prog table loc pcontext ctx variants? e-input p-input)
  (define proof (dict-ref (hash-ref table e-input) p-input))
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

(define (collect-necessary-proofs altn table)
  (match altn
    [(alt expr (list (or 'rr 'simplify) loc (? egraph-query? e-input) #f #f) (list prev) _)
     (define p-input (cons (location-get loc (alt-expr prev))
                           (location-get loc (alt-expr altn))))
     (hash-update! table e-input (curryr set-add p-input) '())]
    [_ (void)])
  altn)
  

(define (add-soundiness-to pcontext ctx cache table altn)
  (match altn

    [(alt expr `(rr (,@loc) ,(? egraph-query? e-input) #f #f) `(,prev) _)
     (define p-input (cons (location-get loc (alt-expr prev)) (location-get loc (alt-expr altn))))
     (match-define (cons proof errs)
       (hash-ref! cache (cons p-input e-input)
                  (λ () (canonicalize-proof (alt-expr altn) table loc pcontext ctx #t e-input p-input))))
     (alt expr `(rr (,@loc) ,e-input ,proof ,errs) `(,prev) '())]

    [(alt expr `(rr (,@loc) ,(? rule? input) #f #f) `(,prev) _)
     (match-define (cons proof errs)
       (hash-ref! cache (cons input expr)
                  (λ ()
                    (define proof
                      (list (alt-expr prev)
                            (list 'Rewrite=> (rule-name input) (alt-expr altn))))
                    (define errs
                      (get-proof-errors proof pcontext ctx))
                    (cons proof errs))))
     (alt expr `(rr (,@loc) ,input ,proof ,errs) `(,prev) '())]

    ;; This is alt coming from simplify
    [(alt expr `(simplify (,@loc) ,(? egraph-query? e-input) #f #f) `(,prev) _)
     (define p-input (cons (location-get loc (alt-expr prev)) (location-get loc (alt-expr altn))))
     (match-define (cons proof errs)
       (hash-ref! cache (cons p-input e-input)
                  (λ () (canonicalize-proof (alt-expr altn) table loc pcontext ctx #f e-input p-input))))
     (alt expr `(simplify (,@loc) ,e-input ,proof ,errs) `(,prev) '())]

    [else altn]))

(define (add-soundiness alts pcontext ctx)
  alts)
  ; (define table (make-hasheq))
  ; (for ([altn (in-list alts)])
  ;   (alt-map (curryr collect-necessary-proofs table) altn))
  ; (define proof-table
  ;   (for/hash ([(e-input p-inputs) (in-hash table)])
  ;     (match-define (cons variants proofs)
  ;       (run-egg e-input #f #:proof-inputs p-inputs
  ;                #:proof-ignore-when-unsound? #t))
  ;     (values e-input (map cons p-inputs proofs))))

  ; (define cache (make-hash))
  ; (for/list ([altn alts])
  ;   (alt-map (curry add-soundiness-to pcontext ctx cache proof-table) altn)))
