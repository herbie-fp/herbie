#lang racket

(require "common.rkt")
(require "points.rkt")
(require "alternative.rkt")
(require "localize-error.rkt")
(require "simplify/simplify.rkt")
(require "infer-regimes.rkt")
(require "programs.rkt")
(require "periodicity.rkt")
(require "taylor.rkt")
(require "alt-table.rkt")
(require "matcher.rkt")

(provide remove-pows setup-prog post-process
         split-table extract-alt combine-alts
         best-alt simplify-alt completely-simplify-alt
         taylor-alt zach-alt)

(define initial-fuel '())

;; Implementation

(define (remove-pows altn)
  (alt-event
   `(λ ,(program-variables (alt-program altn))
      ,(let loop ([cur-expr (program-body (alt-program altn))])
	 (cond [(and (list? cur-expr) (eq? 'expt (car cur-expr))
		     (let ([exponent (caddr cur-expr)])
		       (and (not (list? exponent))
                            (not (symbol? exponent))
			    (positive? exponent)
			    (integer? exponent)
			    (exponent . < . 10))))
		(let inner-loop ([pows-left (caddr cur-expr)])
		  (if (pows-left . = . 1)
		      (cadr cur-expr)
		      (list '* (cadr cur-expr) (inner-loop (sub1 pows-left)))))]
	       [(list? cur-expr)
		(cons (car cur-expr) (map loop (cdr cur-expr)))]
	       [#t cur-expr])))
   'removed-pows
   (list altn)))

(define (setup-prog prog fuel)
  (let* ([alt (make-alt prog)]
	 [maybe-simplify ((flag 'setup 'simplify) simplify-alt identity)]
	 [processed (maybe-simplify alt)]
	 [table (make-alt-table (*pcontext*) processed)]
	 [extracted (atab-all-alts table)])
    (assert (equal? extracted (list processed))
	    #:extra-info (λ () (format "Extracted is ~a, but we gave it ~a"
				       extracted processed)))
    table))

(define (extract-alt table)
  (parameterize ([*pcontext* (atab-context table)])
    (argmin alt-history-length
            (argmins alt-cost
                     (argmins (compose errors-score alt-errors)
                              (map simplify-alt (atab-all-alts table)))))))

(define (combine-alts splitpoints alts)
  (let ([rsplits (reverse splitpoints)])
    (make-regime-alt
     `(λ ,(program-variables (*start-prog*))
	,(let loop ([rest-splits (cdr rsplits)]
		    [acc (program-body (alt-program (list-ref alts (sp-cidx (car rsplits)))))])
	   (if (null? rest-splits) acc
	       (loop (cdr rest-splits)
		     (let ([splitpoint (car rest-splits)])
		       `(if (< ,(sp-bexpr splitpoint)
			       ,(sp-point splitpoint))
			    ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
			    ,acc))))))
     alts splitpoints)))

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

(define (post-process table)
  (debug #:from 'progress #:depth 2 "Final touches.")
  (let* ([all-alts (atab-all-alts table)]
	 [num-alts (length all-alts)]
	 [zached-alts 0]
	 [maybe-zach ((flag 'reduce 'zach)
		      (λ (alt locs)
			(debug #:from 'progress #:depth 3 "zaching alt" (add1 zached-alts) "of" num-alts)
			(set! zached-alts (add1 zached-alts))
			(append-map (curry zach-alt alt) locs))
		      (const '()))]
	 [taylored-alts 0]
	 [maybe-taylor ((flag 'reduce 'taylor)
			(λ (alt locs)
			  (debug #:from 'progress #:depth 3 "tayloring alt" (add1 taylored-alts) "of" num-alts)
			  (set! taylored-alts (add1 taylored-alts))
			  (append-map (curry taylor-alt alt) locs))
			(λ (x y) (list x)))]
	 [locss (map (compose localize-error alt-program) all-alts)]
	 [alts*
	  (apply append
		 (for/list ([alt all-alts] [locs locss])
		   (append (maybe-zach alt locs) (maybe-taylor alt locs))))]
	 [num-alts* (length alts*)]
	 [simplified-alts 0]
	 [maybe-simplify ((flag 'reduce 'simplify)
			  (λ (alt)
			    (debug #:from 'progress #:depth 3 "simplifying alt" (add1 simplified-alts) "of" num-alts*)
			    (set! simplified-alts (add1 simplified-alts))
			    (completely-simplify-alt alt))
			  identity)]
	 [table* (atab-add-altns table (map maybe-simplify alts*))])
    table*))

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
  (for/list ([transform transforms-to-try])
    (match transform
      [(list name f finv)
       (alt-add-event
        (make-delta altn
		    (location-do loc (alt-program altn)
				 (λ (expr) (let ([fv (free-variables expr)])
					     (if (null? fv) expr
						 (approximate expr fv #:transform (map (const (cons f finv)) fv))))))
		    'taylor)
        `(taylor ,name ,loc))])))

(define (make-delta old-alt new-prog name)
  (alt-delta new-prog (change (rule name (alt-program old-alt) new-prog) '()
			      (for/list ([var (program-variables new-prog)]) (cons var var)))
	     old-alt))

(define (zach-alt altn loc)
  (let ([sibling (location-sibling loc)]
	[rewrite
         ((flag 'generate 'rm) alt-rewrite-rm alt-rewrite-expression)])
    (if (and sibling
             (= (length (location-get (location-parent loc)
                                      (alt-program altn))) 3))
	(rewrite (alt-add-event altn '(start zaching)) #:root sibling)
        '())))

(define (split-table orig-table)
  (match-let* ([(list splitpoints altns) (infer-splitpoints (atab-all-alts orig-table))])
    (if (= 1 (length splitpoints)) (list (list orig-table) splitpoints)
	(let* ([preds (splitpoints->point-preds splitpoints (length altns))]
	       [tables* (split-atab orig-table preds)])
	  (list tables* splitpoints)))))

