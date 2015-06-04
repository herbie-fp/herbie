#lang racket

;;======== Dependencies ===========
(require "../common.rkt")
(require "../main.rkt")
(require "../points.rkt")
(require "../localize-error.rkt")
(require "../programs.rkt")
(require "../alternative.rkt")

(require "tools.rkt")

;;========= Structures and Parameters ===========
(struct sdat (alts pcontext pcontext-extended children locations chosen-alt-idx cur-combo best-axis))
(define (sdat-chosen-alt data)
  (list-ref (sdat-alts data) (sdat-chosen-alt-idx data)))

(define *graph-name* (make-parameter "graph.png"))

;;========= Top Level Interface ============
(provide start-session select-location choose-children pick-next)

;; Starts a session with the herbie-web-viz. Returns a list of two
;; things: a table of objects to respond with mapped to their names,
;; and a session-data object representing the state of their session.
(define (start-session prog)
  (parameterize ([*start-prog* prog])
    (define samplers (map (curryr cons sample-default) (program-variables prog)))
    (define pcontext (parameterize ([*num-points* 64]) (prepare-points prog samplers)))
    (define pcontext-extended (parameterize ([*num-points* 1024]) (prepare-points prog samplers)))
    (parameterize ([*pcontext* pcontext] [*analyze-context* pcontext])
      (define alt (simplify-alt (make-alt prog)))
      (define locs (take (localize-error (alt-program alt)) 2))
      ;; The axis finding procedure is stochastic, and is a lot more
      ;; reliable if you use the full point set.
      (define axis (find-best-axis alt pcontext-extended))
      (define session-data (sdat (list alt) pcontext pcontext-extended '() locs 0 alt axis))
      (define response
	(hash
	 "formula" (texify-formula prog locs)
	 "error-graph" (*graph-name*)
	 "loc-ranges" (make-ranges pcontext alt locs axis)))
      (define content
	(hash
	 "start.json" response
	 (*graph-name*) (graph-error pcontext-extended alt axis)))
      (values content session-data))))

;; Starts phase two, candidate selection. Takes the session from the
;; session, and a location to improve at, and returns two values: the
;; content table to send back to the user as specified above, and the
;; new session state.
(define (select-location data location-idx)
  (let ([loc (list-ref (sdat-locations data) location-idx)]
	[alt (sdat-chosen-alt data)]
        [pcontext-extended (sdat-pcontext-extended data)]
        [cur-combo (sdat-cur-combo data)])
    (define children (expand-at-loc alt loc))
    (define response
      (hash
       "selected_formula" (texify-formula (alt-program alt) (list loc))
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
		     (graph-error pcontext-extended child (sdat-best-axis data) cur-combo)))))
    (define session-data (sdat (list alt)
			       (sdat-pcontext data) pcontext-extended
			       '()
			       (list loc)
			       (sdat-chosen-alt-idx data)
                               cur-combo
			       (sdat-best-axis data)))
    (values content session-data)))

;; Starts phase three, choosing the next alt to work on. Takes in
;; addition to session data a list of indicies into the list of
;; children to keep. Returns as above.
(define (choose-children data child-idxs)
  (let* ([alts* (for/list ([idx child-idxs])
                  (list-ref (sdat-children data) idx))]
         [pcontext* (sdat-pcontext data)]
         [pcontext-extended* (sdat-pcontext-extended data)]
         [children* '()]
         [locations* '()]
         [chosen-alt-idx* '()]
         [cur-combo* (make-combo alts*)]
         [best-axis* (sdat-best-axis data)])
    (define session-data
      (sdat alts* pcontext* pcontext-extended* children* locations*
            chosen-alt-idx* cur-combo* best-axis*))
    (define response
      (hash 
       "combo_graph" (*graph-name*)
       "candidates" (for/list ([alt alts*] [idx (in-naturals)])
                      (hash
                       "id" idx
                       "formula" (texify-formula (alt-program alt))
                       "graph" (format "cand~aerror.png")))))
    (define content
      (apply make-hash
             (cons "choose-children.json" response)
             (for/list ([alt alts*] [idx (in-naturals)])
               (cons (format "cand~aerror.png" idx)
                     (graph-error pcontext-extended* alt best-axis* cur-combo*)))))
    (values content session-data)))

;; Ends phase three and starts over, by picking the next candidate to
;; work on. Takes a candidate index of the candidate to work on, and
;; returns as above.
(define (pick-next data cand-idx)
  (let* ([alt (list-ref (sdat-alts data) cand-idx)]
         [locs* (localize-error alt)]
         [pcontext (sdat-pcontext data)]
         [pcontext-extended (sdat-pcontext-extended data)]
         [axis (sdat-best-axis data)]
         [combo (sdat-cur-combo data)])
    (define session-data
      (sdat (sdat-alts data) pcontext pcontext-extended
            '() locs* cand-idx
            combo axis))
    (define response
      (hash
       "formula" (texify-formula (alt-program alt))
       "error_graph" (*graph-name*)
       "loc_ranges" (make-ranges pcontext alt locs* axis)))
    (define content
      (hash
       "pick-next.json" response
       (*graph-name*) (graph-error (sdat-pcontext-extended data) alt axis combo)))
    (values content session-data)))
