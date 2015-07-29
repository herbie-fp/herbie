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

(provide ;; For the shell
	 (all-defined-out))

; For debugging
(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))
(define initial-fuel '())

;; Implementation

(define (remove-pows altn)
  (define (make-mul expr pow)
    (for/fold ([acc expr])
	([i (in-range (sub1 pow))])
      `(* ,expr ,acc)))
  (alt-event
   (program-induct
    (alt-program altn)
    #:primitive
    (λ (expr)
      (match expr
	[`(expt ,base ,exponent)
	 (if (and (constant? exponent)
		  (positive? exponent)
		  (integer? exponent)
		  (exponent . < . 10))
	     (make-mul base exponent)
	     expr)]
	[_ expr])))
   'removed-pows
   (list altn)))

(define (simplify-alt altn)
  altn)

(define (unfold-lets prog)
  `(λ ,(program-variables prog)
     ,(unfold-lets-expr (program-body prog))))

(define (unfold-lets-expr expr)
  (expression-induct
   expr
   #:let (λ (expr)
	   (match expr
	     [`(let ([,vars ,vals] ...) ,body)
	      (let ([symbol-table (map list vars vals)])
		(expression-induct
		 body
		 #:variable
		 (λ (var)
		   (let ([lookup (assoc var symbol-table)])
		     (if lookup (cadr lookup) var)))))]))))

(define (contains? expr subexpr)
  (let/ec return
    (define (retifm expr) (if (equal? expr subexpr) (return #t) expr))
    (expression-induct expr #:primitive retifm #:variable retifm #:constant retifm)
    #f))

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
;; Takes a function from loop expressions to lists of loop
;; expressions, and an event tag, and produces a list of alts which
;; apply that transformation as an alt event.
(define (rewrite-loops alt f tag)
  (define (loop-locs prog)
    (define acc '())
    (location-induct
     prog
     #:loop (λ (expr location)
              (set! acc (cons location acc))
              expr))
    acc)
  (apply
   append
   (let ([prog (alt-program alt)])
     (for/list ([loc (loop-locs prog)])
       (for/list ([rw-loop (f (location-get loc prog))])
         (let ([rw-prog (location-do loc prog (const rw-loop))])
           (alt-event rw-prog tag (list alt))))))))

(define (completely-simplify-alt altn)
  (let* ([prog (alt-program altn)]
	 [prog* `(λ ,(program-variables prog) ,(parameterize ([*max-egraph-iters* (/ (*max-egraph-iters*) 2)])
						 (simplify-expr (program-body prog))))]
	 [chng (change (rule 'simplify prog prog* '()) '() (map cons (program-variables prog) (program-variables prog)))])
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
			(λ (x y) x))]
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
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

(define (taylor-alt altn loc)
  ; BEWARE WHEN EDITING: the free variables of an expression can be null
  (for/list ([transform transforms-to-try])
    (match transform
      [(list name f finv)
       (alt-event
        (location-do loc (alt-program altn)
                     (λ (expr) (let ([fv (free-variables expr)])
                                 (if (null? fv) expr
                                     (approximate expr fv #:transform (map (const (cons f finv)) fv))))))
        `(taylor ,name ,loc) (list altn))])))

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

(define (splitpoints->point-preds splitpoints num-alts)
  (let* ([expr (sp-bexpr (car splitpoints))]
	 [variables (program-variables (*start-prog*))]
	 [intervals (map cons (cons (sp #f expr -inf.0)
				    (drop-right splitpoints 1))
			 splitpoints)])
    (for/list ([i (in-range num-alts)])
      (let ([p-intervals (filter (λ (interval) (= i (sp-cidx (cdr interval)))) intervals)])
	(debug #:from 'splitpoints "intervals are: " p-intervals)
	(λ (p)
	  (let ([expr-val ((eval-prog `(λ ,variables ,expr) mode:fl) p)])
	    (for/or ([point-interval p-intervals])
	      (let ([lower-bound (sp-point (car point-interval))]
		    [upper-bound (sp-point (cdr point-interval))])
		(and (lower-bound . < . expr-val)
		     (expr-val . <= . upper-bound))))))))))

(define (verify-points-sorted point-lst expr)
  (define (eval-at-point pt)
    ((eval-prog `(λ ,(program-variables (*start-prog*)) ,expr) mode:fl) pt))
  (for ([p1 (drop-right point-lst 1)]
	[p2 (drop point-lst 1)])
    (assert ((eval-at-point p1) . <= . (eval-at-point p2))
	    #:extra-info (const (list p1 p2)))))

;; Verifies that for each splitpoint pred pair, where splitpoints and preds are paired
;; by position in their respective lists:
;; let p be the pred, and s be the splitpoint
;; all points between s and the splitpoint before it satisfy p, and no other points satisfy p.
(define (verify-point-preds splitpoints point-preds)
  (let ([sorted-points (car (sort-context-on-expr (*pcontext*) (sp-bexpr (car splitpoints)) (program-variables (*start-prog*))))])
    (verify-points-sorted sorted-points (sp-bexpr (car splitpoints)))
    (for/fold ([rest-pts sorted-points])
	([split splitpoints])
      (let-values ([(pred) (list-ref point-preds (sp-cidx split))]
		   [(points-before-split points-after-split)
		    (splitf-at rest-pts (λ (p) (<= ((eval-prog `(λ ,(program-variables (*start-prog*)) (sp-bexpr split))) p) (sp-point split))))])
	(assert (not (null? points-before-split)))
	(assert (andmap pred points-before-split)
		#:extra-info (λ _ (map pred points-before-split)))
	(let ([overlapping (for/first ([other-pred (remove pred point-preds)]
				       [other-split (remove split splitpoints)]
				       #:when (ormap other-pred points-before-split))
			     (list split other-split))])
	  (assert (not overlapping) #:extra-info (const overlapping)))
	points-after-split))
    (void)))
