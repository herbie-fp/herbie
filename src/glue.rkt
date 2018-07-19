#lang racket

(require "common.rkt")
(require "alternative.rkt")
(require "points.rkt")
(require "programs.rkt")
(require "core/regimes.rkt")
(require "core/taylor.rkt")
(require "type-check.rkt")

(provide combine-alts best-alt taylor-alt)

;; Implementation

(define (combine-alts splitpoints alts)
  (define expr
    (for/fold
        ([expr (program-body (alt-program (list-ref alts (sp-cidx (last splitpoints)))))])
        ([splitpoint (cdr (reverse splitpoints))])
      `(if (<= ,(sp-bexpr splitpoint) ,(sp-point splitpoint))
           ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
           ,expr)))
  (alt `(λ ,(program-variables (*start-prog*)) ,expr)
       (list 'regimes splitpoints) alts))

(define (best-alt alts)
  (argmin alt-cost
	  (argmins (compose errors-score alt-errors)
		   alts)))

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (- ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

(define (taylor-alt altn loc)
  (define expr (location-get loc (alt-program altn)))
  (define vars (free-variables expr))
  (if (or (null? vars) ;; `approximate` cannot be called with a null vars list
          (not (equal? (type-of expr (for/hash ([var vars]) (values var 'real))) 'real)))
      (list altn)
      (for/list ([transform-type transforms-to-try])
        (match-define (list name f finv) transform-type)
        (define transformer (map (const (cons f finv)) vars))
        (alt
         (location-do loc (alt-program altn) (λ (expr) (approximate expr vars #:transform transformer)))
         `(taylor ,name ,loc)
         (list altn)))))

