#lang racket

(require "common.rkt")
(require "points.rkt")
(require "alternative.rkt")
(require "programs.rkt")
(require "core/simplify.rkt")
(require "core/localize.rkt")
(require "core/regimes.rkt")
(require "core/periodicity.rkt")
(require "core/taylor.rkt")
(require "core/alt-table.rkt")
(require "core/matcher.rkt")
(require  "type-check.rkt")

(provide setup-prog setup-alt-simplified
         split-table extract-alt combine-alts
         best-alt simplify-alt completely-simplify-alt
         taylor-alt)

;; Implementation

(define (setup-prog prog)
  (let* ([alt (make-alt prog)]
	 [table (make-alt-table (*pcontext*) alt)]
	 [extracted (atab-all-alts table)])
    (assert (equal? extracted (list alt))
	    #:extra-info (λ () (format "Extracted is ~a, but we gave it ~a"
				       extracted alt)))
    table))

(define (setup-alt-simplified prog)
  (let* ([alt (make-alt prog)]
	 [maybe-simplify (if (flag-set? 'setup 'simplify) simplify-alt identity)]
	 [processed (maybe-simplify alt)])
    processed))

(define (extract-alt table)
  (parameterize ([*pcontext* (atab-context table)])
    (argmin alt-cost
            (argmins (compose errors-score alt-errors)
                     (atab-all-alts table)))))

(define (combine-alts splitpoints alts)
  (define expr
    (for/fold
        ([expr (program-body (alt-program (list-ref alts (sp-cidx (last splitpoints)))))])
        ([splitpoint (cdr (reverse splitpoints))])
      (define test `(<= ,(sp-bexpr splitpoint) ,(sp-point splitpoint)))
      `(if ,test ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint)))) ,expr)))
  (alt `(λ ,(program-variables (*start-prog*)) ,expr)
       (list 'regimes splitpoints) alts))

(define (best-alt alts)
  (argmin alt-cost
	  (argmins (compose errors-score alt-errors)
		   alts)))

(define (simplify-alt altn)
  (apply alt-apply altn (simplify altn)))

(define (completely-simplify-alt altn)
  (let* ([prog (alt-program altn)]
	 [prog* `(λ ,(program-variables prog) ,(parameterize ([*max-egraph-iters* (/ (*max-egraph-iters*) 2)])
						 (simplify-expr (program-body prog))))]
	 [chng (change (rule 'simplify prog prog*) '() (map cons (program-variables prog) (program-variables prog)))])
    (debug "prog is" prog*)
    (alt-add-event (alt-delta prog* chng altn) 'final-simplify)))

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (- ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

(define (taylor-alt altn loc)
  ; BEWARE WHEN EDITING: the free variables of an expression can be null
  (define expr (location-get loc (alt-program altn)))
  (match (type-of expr (for/hash ([var (free-variables expr)]) (values var 'real)))
    ['real
      (for/list ([transform transforms-to-try])
        (match transform
        [(list name f finv)
        (alt
          (location-do loc (alt-program altn)
                       (λ (expr) (let ([fv (free-variables expr)])
                                      (if (null? fv) expr
                                          (approximate expr fv #:transform (map (const (cons f finv)) fv))))))
          `(taylor ,name ,loc)
          (list altn))]))]
    ['complex
      (list altn)]))

(define (split-table orig-table)
  (match-let* ([(list splitpoints altns) (infer-splitpoints (atab-all-alts orig-table))])
    (if (= 1 (length splitpoints)) (list (list orig-table) splitpoints)
	(let* ([preds (splitpoints->point-preds splitpoints (length altns))]
	       [tables* (split-atab orig-table preds)])
	  (list tables* splitpoints)))))

