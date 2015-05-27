#lang racket

(require "../common.rkt")
(require "../main.rkt")
(require "../points.rkt")
(require "../localize-error.rkt")

(struct sdat (alts pcontext pcontext-extended children locations chosen-alt-idx cur-combo best-axis))

(define *graph-name* (make-parameter "graph.png"))

;; Starts a session with the herbie-web-viz. Returns a list of two
;; things: a table of objects to respond with mapped to their names,
;; and a session-data object representing the state of their session.
(define (start-session prog)
  (parameterize ([*start-prog* prog])
    (define samplers (map (curryr cons sample-default) (program-variables prog)))
    (define pcontext (parameterize ([*num-points* 64]) (prepare-points prog samplers)))
    (define pcontext-extended (parameterize ([*num-points* 1024]) (prepare-points prog samplers)))
    (parameterize ([*pcontext* context] [*analyze-context* context])
      (define alt (simplify-alt (make-alt prog)))
      (define locs (localize-error alt))
      (define axis (find-best-axis alt))
      (define session-data (sdat (list alt) pcontext pcontext-extended '() locs 0 alt axis))
      (define response
	(hash
	 "formula" (texify-formula prog locs)
	 "error-graph" (*graph-name*)
	 "loc-ranges" (make-ranges alt locs axis)))
      (define content
	(hash
	 "start.json" response
	 (*graph-name*) (graph-error alt axis)))
      (values content session-data))))

(define (select-location data location-idx)
  (let ([loc (list-ref (sdat-locations data) location-idx)]
	[alt (sdat-chosen-alt data)])
    (define children (expand-at-loc alt loc))
    (define response
      (hash
       "selected_formula" (texify-formula prog (list loc))
       "calts" (for/list ([child children] [idx (in-naturals)])
		 (hash
		  "steps" (make-steps child alt)
		  "graph" (format "child~aerror.png" idx)))))
    (define content
      (apply make-hash
	     (cons "select-location.json" response)
	     (for/list ([child children]
			[idx (in-range (length children))])
	       (cons (format "child~aerror.png" idx)
		     (graph-error child (sdat-best-axis data) cur-combo)))))
    (define session-data (sdat (list alt)
			       (sdat-pcontext data) (sdat-pcontext-extended data)
			       '()
			       (list loc)
			       (sdat-chosen-alt-idx data)
			       (sdat-cur-combo data)
			       (sdat-best-axis data)))
    (values content session-data)))

(define (sdat-chosen-alt data)
  (list-ref (sdat-alts data) (sdat-chosen-alt-idx data)))
