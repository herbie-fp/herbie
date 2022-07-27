#lang racket

(require racket/lazy-require)
(require "alternative.rkt" "points.rkt" "programs.rkt")

(provide add-soundiness)

(lazy-require
 [egg-herbie (egg-expr->expr)])

(define (remove-rewrites proof)
  (match proof
    [`(Rewrite=> ,rule ,something)
     (remove-rewrites something)]
    [`(Rewrite<= ,rule ,something)
     (remove-rewrites something)]
    [(list _ ...)
     (map remove-rewrites proof)]
    [else proof]))

(define (get-proof-errors proof pcontext ctx program-vars)
  (define proof-programs
    (map (lambda (expr)
           `(λ ,program-vars
              ,(remove-rewrites expr)))
         proof))
  (define proof-errors
    (map (lambda (x) (errors x pcontext ctx)) (remove-rewrites proof-programs)))
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
    [(alt prog `(simplify ,loc ,proof #f) `(,prev))
     (define vars (program-variables prog))
     (alt prog `(simplify ,loc ,proof ,(get-proof-errors proof pcontext ctx vars)) `(,prev))]
    [else
     altn]))

(define (add-soundiness alt pcontext ctx)
  (alt-map
   (curry add-soundiness-to pcontext ctx)
   alt))