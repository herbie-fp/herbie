#lang racket

;; ================== Dependencies ===================

;; For converting floating point numbers into their ordinal
;; representation, for clustering.
(require math/flonum)

(require "../common.rkt")
(require "../alternative.rkt")
(require "../programs.rkt")
(require "../core/matcher.rkt")
(require "../core/infer-regimes.rkt")
(require "../plot.rkt")
(require "../points.rkt")
(require "../float.rkt")
(require "../main.rkt")
(require "../syntax/syntax.rkt")
(require "../compile/tex.rkt")
(require "../core/localize-error.rkt")

;; ================== Interface =======================

(provide find-best-axis texify-formula make-ranges graph-error
         expand-at-loc make-steps make-combo get-locs)

;; ================== Parameters ======================

;; How many clusters to attempt to cluster points into to determine
;; which axis is best.
(define *num-clusters* (make-parameter 5))
;; The number of trials of k-means scoring to use.
(define *num-scores* (make-parameter 3))
(define *bin-distance* (make-parameter 4))

;; Find the axis that best portrays the error behavior
(define (find-best-axis alt pcontext)
  (let ([bad-points (for/list ([(p ex) (in-pcontext pcontext)]
                               [e (parameterize ([*pcontext* pcontext])
                                    (alt-errors alt))]
                               #:when (> e (expt 2 10)))
                      p)]
        [vars (program-variables (alt-program alt))])
    (if (null? bad-points) ;; If everythings good, just display the
			   ;; first axis.
	(car vars)
	(list-ref
	 vars
	 (argmax (λ (pidx)
		   ;; Rank the variables by how relevant they are
		   ;; to the error.  Higher is more relevant.
		   (cluster-rank (map (compose flonum->ordinal
					       (curryr list-ref pidx))
				      bad-points)))
		 (build-list (length vars) identity))))))
;; Generate the tex for the given prog, with the given locations
;; highlighted and given MathJax ID's
(define (texify-formula expr [locs '()])
  (texify-expr expr
               #:highlight-ops
               (for/list ([loc locs] [idx (in-naturals)])
                 (cons loc idx))))
;; Given a context and an alt and some locations, identify which
;; ranges of error coorespond to which locations along the given axis,
;; and generate list of hash table objects for them.
(define (make-ranges context alt locs axis)
  (let* ([prog (alt-program alt)]
         [vars (program-variables prog)]
         [axis-idx (if (number? axis) axis
                       (lookup-idx axis vars))]
         [pts (for/list ([(p e) (in-pcontext context)]) p)]
         [getpt (curryr list-ref axis-idx)]
         [min-x (apply min (map getpt pts))]
         [max-x (apply max (map getpt pts))])
    (define (get-ranges loc)
      (let* ([subexpr (location-get loc prog)]
             [local-errors 
              (for/list ([p (in-list pts)])
                (let* ([exact-args (for/list ([arg (in-list (cdr subexpr))])
                                     ((eval-exact `(λ ,vars ,arg)) p))]
                       [f-exact (real-op->bigfloat-op (car subexpr))]
                       [f-approx (real-op->float-op (car subexpr))]
                       [exact (->flonum (apply f-exact exact-args))]
                       [approx (apply f-approx (map ->flonum exact-args))]
                       [local-err (add1 (abs (ulp-difference exact approx)))])
                  local-err))])
        (get-clusters axis-idx pts local-errors)))
    (apply append
           (for/list ([loc locs]
                      [loc-id (in-naturals)])
             (for/list ([range (get-ranges loc)])
               (hash
                'start (coord->image-ratio (car range) min-x max-x)
                'end (coord->image-ratio (cdr range) min-x max-x)
                'locid loc-id))))))
                  
;; Draw the graph of average error using the given points for the
;; given alt, along the given axis. If combo is given draw it also on
;; the same graph in a different color.
(define (graph-error context alt axis [combo #f]
                     #:first-time [first-time #f] #:children? [children? #f])
  (let* ([points (for/list ([(p e) (in-pcontext context)]) p)]
	 [vars (program-variables (alt-program alt))]
	 [renderers
	  (parameterize ([*pcontext* context])
	    (reap [sow]
		  (when (alt? combo)
		    (sow (error-avg (alt-errors combo) points #:axis axis
				    #:vars vars #:color
                                    (if first-time *red-theme* *blue-theme*))))
		  (sow (error-avg (alt-errors alt) points #:axis axis
				  #:vars vars
				  #:color (cond [(equal? #t combo)
                                                 *blue-theme*]
                                                [children? *yellow-theme*]
                                                [#t *red-theme*])))
		  ))])
    (λ (out) (apply herbie-plot #:port out #:kind 'png renderers))))
;; Generate at most three or four children for the given alt at the
;; given location.
(define (expand-at-loc alt loc)
  (general-filter
   (map simplify-alt
	(append (taylor-filter (taylor-alt alt loc))
		(rewrite-filter (alt-rewrite-rm alt #:root loc))))
   alt))
;; Generate the list of steps hash objects representing the changes
;; between the parent and the child.
(define (make-steps child parent)
  (let steps-left ([cur child] [steps '()])
    (cond [(equal? (alt-program cur) (alt-program parent))
           steps]
	  [(not cur) (print-history child) (error "The given parent is not a parent of the child!2")]
	  [(alt-event? cur)
	   (steps-left
	    (alt-prev cur)
	    (cons
	     (hash
	      'rule "taylor"
	      'prog (texify-formula (program-body (alt-program cur))))
	     steps))]
          [(not (alt-change cur)) (error "The given parent is not a parent of the child!")]
          [#t (steps-left
               (alt-prev cur)
               (cons
                (hash
		 'rule (let ([rule (change-rule (alt-change cur))])
                          (if (equal? (rule-name rule) 'simplify)
                              "simplify"
                              (let ([rule (change-rule (alt-change cur))])
				(format "~a \\to ~a"
					(texify-formula (rule-input rule))
					(texify-formula (rule-output rule))))))
                 'prog (texify-formula (program-body (alt-program cur))))
                steps))])))

(define (print-history alt)
  (let loop ([cur alt])
    (if (not cur) (void)
	(begin (loop (alt-prev cur))
	       (eprintf (format "~a -> " (alt-program cur)))))))
    
;; Combine the given alternatives into the best combination.
(define (make-combo alts axis)
  (parameterize ([*start-prog* (alt-program (car alts))])
    (match-let ([`(,splitpoints ,involved-alts) (infer-splitpoints alts axis)])
      (if (= (length involved-alts) 1)
          (car involved-alts)
          (combine-alts splitpoints involved-alts)))))

(define (get-locs alt)
  (let ([locs (localize-error (alt-program alt))])
    (if ((length locs) . < . 2) locs
	(take (localize-error (alt-program alt)) 2))))

;; =============== Lower level helper functions =============

(define (lookup-idx item lst)
  (for/first ([i lst] [idx (in-naturals)]
              #:when (equal? item i))
    idx))

;; Filter children
(define (general-filter alts parent)
  (let-values ([(bad-pts bad-exs)
		(for/lists (pts exs)
		    ([(p ex) (in-pcontext (*pcontext*))]
		     [e (alt-errors parent)]
		     #:when (> e (expt 2 10)))
		  (values p ex))])
    (parameterize ([*pcontext* (mk-pcontext bad-pts bad-exs)])
      (take (sort alts < #:key (compose errors-score alt-errors)) 3))))
(define (taylor-filter alts)
  (filter (negate has-nan?) alts))
(define (has-nan? expr)
  (or (and (number? expr) (nan? expr))
      (and (list? expr)
	   (ormap has-nan? (cdr expr)))))

(define *banned-toplevel-rules*
  '(+-commutative
    *-commutative
    sub-neg
    neg-sub0
    *-un-lft-identity
    div-inv
    neg-mul-1
    clear-num
    expt1))
 
(define (rewrite-filter alts)
  (filter
   (λ (alt) (not (member (rule-name (change-rule (alt-change alt)))
			 *banned-toplevel-rules*)))
   alts))

;; Ranks a set of numbers by how well they group into clusters.
(define (cluster-rank xs)
  (for/sum ([idx (in-range (*num-scores*))])
    (k-means-score xs (*num-clusters*))))
;; Scores how well the given numbers can be clustered into
;; num-clusters clusters using k-means.
(define (k-means-score xs num-clusters)
  (let ([initial-means
         (for/list ([idx (in-range num-clusters)])
           (list-ref xs (random (length xs))))])
    (let loop ([means initial-means])
      (let* ([clustered-samples
              (for/list ([x xs])
                (cons x (argmin (λ (mean) (abs (- mean x))) means)))]
             [means* (for/list ([mean means])
                       (let ([cluster-xs (filter (compose (curry equal? mean) cdr) clustered-samples)])
                         (round (/ (apply + (map car cluster-xs)) (length cluster-xs)))))])
        (if (equal? means* means)
            (exact->inexact (/ (apply + (for/list ([sample clustered-samples])
                                          (sqr (- (car sample) (cdr sample)))))))
            (loop means*))))))

(define (coord->image-ratio coord min-x max-x)
  (let ([ord-min (flonum->ordinal min-x)]
        [ord-max (flonum->ordinal max-x)]
        [ord-coord (flonum->ordinal coord)])
    (exact->inexact (/ (- ord-coord ord-min) (- ord-max ord-min)))))

;; Takes an axis, some points, and some value for each of those
;; points, and attempts to break the points into clusters which
;; represent the higher ys, returning pairs of min and maxs for each
;; cluster.
(define (get-clusters axis pts ys)
  (define curve-pow 5)
  (define (bin vec idx)
    (expt (exact->inexact
           (/ (cond
               ;; Handle the ends of the array in a halfway decent way
               [(< idx (*bin-distance*))
                (+ (* (add1 (*bin-distance*)) (vector-ref vec idx))
                   (for/sum ([i (in-range (*bin-distance*))])
                     (expt (vector-ref vec (+ idx (add1 i))) (/ curve-pow))))]
               [(>= (+ idx (*bin-distance*)) (vector-length vec))
                (+ (* (add1 (*bin-distance*)) (vector-ref vec idx))
                   (for/sum ([i (in-range (*bin-distance*))])
                     (expt (vector-ref vec (- idx (add1 i))) (/ curve-pow))))]
               [#t
                (for/sum ([i (in-range (- idx (*bin-distance*)) (+ idx (*bin-distance*)))])
                  (expt (vector-ref vec i) (/ curve-pow)))])
              (add1 (* 2 (*bin-distance*)))))
          curve-pow))
  (let* ([sorted-pairs (sort (map cons (map (curryr list-ref axis) pts) ys) < #:key car)]
         [xs (list->vector (map car sorted-pairs))]
         ;; Shadowing the old definition so we don't accidentally use
         ;; the unsorted ones.
         [ys (list->vector (map cdr sorted-pairs))]
         [binned-ys (for/vector ([idx (in-range (vector-length ys))]) (bin ys idx))]
         [picked-threshold (exact->inexact (/ (for/sum ([y ys]) y) (vector-length ys)))]
         [included-threshold (/ picked-threshold 100)])
    (let loop ([cur-ys binned-ys] [clusters-found '()])
      (let ([picked (car (argmax cdr (for/list ([idx (in-range (vector-length ys))]
                                                [y (in-vector cur-ys)])
                                       (cons idx y))))])
        (if ((vector-ref cur-ys picked) . < . picked-threshold) clusters-found
            (let* ([cluster-range
                    (cons (or (for/first ([idx (in-range picked -1 -1)]
                                          #:when ((vector-ref cur-ys idx) . < . included-threshold))
                                idx)
                              0)
                          (or (for/first ([idx (in-range picked (vector-length cur-ys))]
                                          #:when ((vector-ref cur-ys idx) . < . included-threshold))
                                idx)
                              (sub1 (vector-length cur-ys))))]
                   ;; Take out the items in this cluster from consideration.
                   [cur-ys* (for/vector ([y cur-ys] [idx (in-naturals)])
                              (if (and ((car cluster-range) . <= . idx)
                                       (idx . <= . (cdr cluster-range)))
                                  0 y))])
              (loop cur-ys*
                    (cons (cons (vector-ref xs (car cluster-range))
                                (vector-ref xs (cdr cluster-range)))
                          clusters-found))))))))
