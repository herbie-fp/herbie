#lang racket

(require "alternative.rkt" "points.rkt" "programs.rkt"
         "core/egg-herbie.rkt" "core/simplify.rkt" "syntax/types.rkt")

(provide add-soundiness)

(define (remove-rewrites proof)
  (match proof
    [`(Rewrite=> ,rule ,something)
     (remove-rewrites something)]
    [`(Rewrite<= ,rule ,something)
     (remove-rewrites something)]
    [(list _ ...)
     (map remove-rewrites proof)]
    [else proof]))

(define (canonicalize-rewrite proof)
  (match proof
    [`(Rewrite=> ,rule ,something)
     (list 'Rewrite=> (get-canon-rule-name rule rule) something)]
    [`(Rewrite<= ,rule ,something)
     (list 'Rewrite<= (get-canon-rule-name rule rule) something)]
    [(list _ ...)
     (map canonicalize-rewrite proof)]
    [else proof]))

(define (get-proof-errors proof pcontext ctx program-vars)
  (define proof-programs
    (for/list ([step (in-list proof)])
      `(λ ,program-vars ,(remove-rewrites step))))
  (define proof-errors (batch-errors proof-programs pcontext ctx))
  (define proof-diffs
    (cons (list 0 0)
          (for/list ([prev proof-errors] [current (rest proof-errors)])
                    (define num-increase
                      (for/sum ([a prev] [b current])
                               (if (> b a)
                                   1
                                   0)))
                    (define num-decrease
                      (for/sum ([a prev] [b current])
                               (if (< b a)
                                   1
                                   0)))
                    (list num-increase
                          num-decrease (length prev)))))
  proof-diffs)
  

(define (add-soundiness-to pcontext ctx altn)
  (match altn
    [(alt prog `(simplify ,loc ,input #f #f) `(,prev))
     (define proof (get-proof input
                              (location-get loc prog)
                              (location-get loc (alt-program prev))))
     ;; Proofs are actually on subexpressions,
     ;; we need to construct the proof for the full expression
     (define proof*
       (for/list ([step proof])
         (let ([step* (canonicalize-rewrite step)])
           (program-body (location-do loc prog (λ _ step*))))))
     (define errors
       (let ([vars (program-variables prog)])
         (get-proof-errors proof* pcontext ctx vars)))
     (alt prog `(simplify ,loc ,input ,proof* ,errors) `(,prev))]
    [else
     altn]))


(define (add-soundiness alts pcontext ctx)
  (for/list ([altn alts])
    (alt-map (curry add-soundiness-to pcontext ctx) altn)))
