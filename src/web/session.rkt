#lang racket

;;======== Dependencies ===========
(require "../common.rkt")
(require "../glue.rkt")
(require "../float.rkt")
(require "../points.rkt")
(require "../programs.rkt")
(require "../alternative.rkt")
(require "../core/localize.rkt")

(require "tools.rkt")

;;========= Structures and Parameters ===========
(struct sdat (alts pcontext pcontext-extended children locations chosen-alt-idx cur-combo best-axis first-time?))
(define (sdat-chosen-alt data)
  (list-ref (sdat-alts data) (sdat-chosen-alt-idx data)))

(define *graph-name* (make-parameter "graph.png"))

;;========= Top Level Interface ============
(provide start-session select-location choose-children pick-next finish)

;; Starts a session with the herbie-web-viz. Returns a list of two
;; things: a table of objects to respond with mapped to their names,
;; and a session-data object representing the state of their session.
(define (start-session prog)
  (parameterize ([*start-prog* prog])
    (define pcontext-extended (parameterize ([*num-points* 1024]) (prepare-points prog 'TRUE)))
    (define pcontext (random-subsample pcontext-extended 64))
    (parameterize ([*pcontext* pcontext] [*analyze-context* pcontext])
      (define alt (simplify-alt (make-alt prog)))
      (define locs (get-locs alt))
      ;; The axis finding procedure is stochastic, and is a lot more
      ;; reliable if you use the full point set.
      (define axis (find-best-axis alt pcontext-extended))
      (define session-data (sdat (list alt) pcontext pcontext-extended '() locs 0 alt axis #t))
      (define response
	(hash
	 'formula (texify-formula (program-body (alt-program alt)) locs)
	 'error_graph "&embedimage{0}"
	 'axis_label (symbol->string axis)
	 'loc_ranges (make-ranges pcontext alt locs axis)))
      (define images (list (graph-error pcontext-extended alt axis)))
      (values response images session-data))))

;; Starts phase two, candidate selection. Takes the session from the
;; session, and a location to improve at, and returns two values: the
;; content table to send back to the user as specified above, and the
;; new session state.
(define (select-location data location-idx)
  (let ([loc (list-ref (sdat-locations data) location-idx)]
	[alt (sdat-chosen-alt data)]
        [pcontext-extended (sdat-pcontext-extended data)]
        [cur-combo (sdat-cur-combo data)])
    (define children (parameterize ([*pcontext* (sdat-pcontext data)])
		       (expand-at-loc alt loc)))
    (define response
      (hash
       'selected_formula (texify-formula (program-body (alt-program alt)) (list loc))
       'calts (for/list ([child children] [idx (in-naturals)])
		 (hash
		  'steps (make-steps child alt)
		  'graph (format "&embedimage{~a}" idx)
		  'id idx))))
    (define images (for/list ([child children])
		     (graph-error pcontext-extended child
				  (sdat-best-axis data) cur-combo
                                  #:first-time (sdat-first-time? data)
                                  #:children? #t)))
    (define session-data (sdat (sdat-alts data)
			       (sdat-pcontext data) pcontext-extended
			       children
			       (list loc)
			       (sdat-chosen-alt-idx data)
                               cur-combo
			       (sdat-best-axis data)
                               (sdat-first-time? data)))
    (values response
	    images
	    session-data)))

;; Starts phase three, choosing the next alt to work on. Takes in
;; addition to session data a list of indicies into the list of
;; children to keep. Returns as above.
(define (choose-children data child-idxs)
  (let* ([alts*
	  (remove-duplicates
	   (append (sdat-alts data)
		   (for/list ([idx child-idxs])
		     (list-ref (sdat-children data) idx)))
	   #:key alt-program)]
         [pcontext* (sdat-pcontext data)]
         [pcontext-extended* (sdat-pcontext-extended data)]
         [children* '()]
         [locations* '()]
         [chosen-alt-idx* '()]
         [cur-combo* (parameterize ([*pcontext* (random-subsample pcontext-extended* 128)])
		       (make-combo alts* (sdat-best-axis data)))]
         [best-axis* (sdat-best-axis data)])
    (define session-data
      (sdat alts* pcontext* pcontext-extended* children* locations*
            chosen-alt-idx* cur-combo* best-axis* (sdat-first-time? data)))
    (define response
      (hash 
       'combo_graph "&embedimage{0}"
       'candidates (for/list ([alt alts*] [image-idx (in-naturals 1)])
                      (hash
                       'id (sub1 image-idx)
                       'formula (texify-formula (program-body (alt-program alt)))
                       'graph (format "&embedimage{~a}" image-idx)))))
    (define images
      (cons (graph-error pcontext-extended* cur-combo* best-axis* #t)
	    (for/list ([alt alts*])
	      (graph-error pcontext-extended* alt best-axis* cur-combo*))))
    (values response images session-data)))

;; Ends phase three and starts over, by picking the next candidate to
;; work on. Takes a candidate index of the candidate to work on, and
;; returns as above.
(define (pick-next data cand-idx)
  (let ([alt (list-ref (sdat-alts data) cand-idx)]
	[pcontext (sdat-pcontext data)]
	[pcontext-extended (sdat-pcontext-extended data)]
	[axis (sdat-best-axis data)]
	[combo (sdat-cur-combo data)])
    (parameterize ([*pcontext* pcontext]
		   [*analyze-context* pcontext])
      (let([locs* (get-locs alt)])
	(define session-data
	  (sdat (sdat-alts data) pcontext pcontext-extended
		'() locs* cand-idx
		combo axis
                #f))
	(define response
	  (hash
	   'formula (texify-formula (program-body (alt-program alt)) locs*)
	   'error_graph "&embedimage{0}"
	   'axis_label (symbol->string axis)
	   'loc_ranges (make-ranges pcontext alt locs* axis)))
	(define images
	  (list (graph-error (sdat-pcontext-extended data) alt axis combo)))
	(values response images session-data)))))

(define (finish data)
  (let ([final-combo (parameterize ([*pcontext* (random-subsample (sdat-pcontext-extended data) 256)])
                       (make-combo (sdat-alts data) (sdat-best-axis data)))])
    (define response
      (hash
       'formula (texify-formula (program-body (alt-program final-combo)))))
    (define images
      (list (graph-error (sdat-pcontext-extended data) final-combo (sdat-best-axis data) #t)))
    (values response images data)))
