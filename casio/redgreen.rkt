#lang racket

(require casio/points)
(require casio/alternative)
(require casio/rules)
(require casio/programs)
(require casio/common)
(require casio/locations)

(provide green-threshold much-better? green? remove-red)

(define green-threshold (make-parameter 0))

(define (much-better? alt1 alt2)
  (< (errors-diff-score (alt-errors alt1) (alt-errors alt2))
     (green-threshold)))
	     
(define (green? altn)
  ; The initial alternative is not green-tipped by convention
  (and (alt-prev altn) (much-better? altn (alt-prev altn))))

;; Terminology clarification: the "stream" in this metaphor flows from the original program to our passed alternative.
;; "Upstream" and "up" both mean backwards in the change history, "downstream" and "down" both mean forward in the
;; change history.
(define (remove-red altn #:fitness-func [fit? green?])
  
  ;; If we're the first or second alt then we can't be moved back any farther.
  (define (done altn)
    (or (eq? (alt-prev altn) #f) (eq? (alt-prev (alt-prev altn)) #f) (fit? (alt-prev altn))))

  ;; Given that "salmon" is blocked from translating further by
  ;; the change in front of it, try to move that change forward,
  ;; and then try to move the salmon forward again.
  (define (move-dam salmon is-head? dams-hit)
    (let* ([dam (alt-prev salmon)]
	   [new-next (swim-upstream dam #f '())])
      (if (or (eq? new-next dam) (member new-next dams-hit))
	  salmon
	  (swim-upstream (alt-apply new-next (alt-change salmon)) is-head? (cons dam dams-hit)))))
  
  ;; Takes the head of an alternative history and returns the head
  ;; of a new history containing the same changes where the leading
  ;; change has been moved down as far as possible. is-head? is a value
  ;; indicating whether we should throw away the changes that have been
  ;; moved past the current change.
  (define (swim-upstream salmon is-head? dams-hit)
    (debug salmon " is swimming." #:from 'swim-upstream #:tag 'info)
    (if (done salmon) salmon
	(let* ([grandparent (alt-prev (alt-prev salmon))]
	       [upstream-changes (translate #t
					    (alt-change salmon)
					    (alt-change (alt-prev salmon))
					    grandparent)])
	  (if (and upstream-changes (list? upstream-changes))
	      (let ([moved-salmon (apply-changes grandparent upstream-changes)])
		(if is-head?
		    (swim-upstream moved-salmon #t dams-hit)
		    (let ([downstream-changes (translate #f
							 (alt-change (alt-prev salmon))
							 (alt-change salmon)
							 moved-salmon)])
		      (if downstream-changes
			  (let ([new-salmon (swim-upstream moved-salmon #f dams-hit)])
			    (apply-changes new-salmon downstream-changes))
			  (move-dam salmon #f dams-hit)))))
	      (move-dam salmon is-head? dams-hit)))))
  (swim-upstream altn #t '()))

;; Takes a list of location tails, a single location head, and an original change,
;; and returns a list of changes that are identitical to the original change except
;; have locations of a tail appended to a head.
(define (loc-translated-changes loc-tails loc-head original)
  (map (lambda (loc)
	 (debug "Translating location" loc "onto" loc-head "in" original
		#:from 'loc-translated-changes #:tag 'info)
	 (change (change-rule original)
		 (append loc-head loc)
		 (change-bindings original)))
       loc-tails))

;; Translates a change through another change.
;; up? indicates whether you are translating the change up. A value
;;     of false indicates a translation down.
;; cur-change is the change that you want to translate.
;; other-change is the change that you want to translate through.
;; new-parent is the alternative that the translated change will be
;;     applied to.
(define (translate up? cur-change other-change new-parent)
  (debug "Translate" cur-change "across" other-change "onto" new-parent
	 #:from 'translate #:tag 'info)
  (cond [(orthogonal? cur-change other-change)
	 (list cur-change)]
	[(is-inside? (change-location cur-change) (change-location other-change))
	 (let* ([translations (rule-location-translations (change-rule other-change))]
		[relative-location (match-loc (change-location cur-change)
					      (change-location other-change))]
		[new-rel-locs (translate-location relative-location
						  translations
						  (if up? cadr car)
						  (if up? car cadr))])
	   (if new-rel-locs ; if translate-location returns false, we can't translate.
	       (loc-translated-changes new-rel-locs (change-location other-change) cur-change)
	       #f))]
	[(is-inside? (change-location other-change) (change-location cur-change))
	 (let* ([rule* (change-rule cur-change)]
		[loc* (change-location cur-change)]
		[bindings* (pattern-match (rule-input (change-rule cur-change))
					  (location-get (change-location cur-change)
							(alt-program new-parent)))])
	   (if bindings* ; if pattern-match returns false, we can't translate.
	       (list (change rule* loc* bindings*))
	       #f))]))

;;Returns whether or not two changes are orthogonal.
;;Orthogonal changes can be rearranged without changing
;;any of the characteristics of the change object.
(define (orthogonal? change-a change-b)
  (not (match-loc (change-location change-a) (change-location change-b))))
