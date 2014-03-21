#lang racket

(require casio/points)
(require casio/alternative)
(require casio/rules)
(require casio/programs)
(require casio/common)

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
(define (remove-red altn)
  
  ;; If we're the first or second alt then we can't be moved back any farther.
  (define (done altn)
    (or (eq? (alt-prev altn) #f) (eq? (alt-prev (alt-prev altn)) #f) (green? (alt-prev altn))))

  ;; Given that "salmon" is blocked from translating further by
  ;; the change in front of it, try to move that change forward,
  ;; and then try to move the salmon forward again.
  (define (move-dam salmon is-head?)
    (let* ([dam (alt-prev salmon)]
	   [new-next (swim-upstream dam #f)])
      (if (eq? new-next dam)
	  salmon
	  (swim-upstream (alt-apply new-next (alt-change salmon)) is-head?))))
  
  ;; Takes the head of an alternative history and returns the head
  ;; of a new history containing the same changes where the leading
  ;; change has been moved down as far as possible. is-head? is a value
  ;; indicating whether we should throw away the changes that have been
  ;; moved past the current change.
  (define (swim-upstream salmon is-head?)
    (debug salmon " is swimming." #:from 'swim-upstream #:tag 'info)
    (if (done salmon) salmon
	(let* ([grandparent (alt-prev (alt-prev salmon))]
	       [upstream-changes (translate #t
					    (alt-change salmon)
					    (alt-change (alt-prev salmon))
					    grandparent)])
	  (if upstream-changes
	      (let ([moved-salmon (apply-changes grandparent upstream-changes)])
		(if is-head?
		    (swim-upstream moved-salmon #t)
		    (let ([downstream-changes (translate #f
							 (alt-change (alt-prev salmon))
							 (alt-change salmon)
							 moved-salmon)])
		      (if downstream-changes
			  (let ([new-salmon (swim-upstream moved-salmon #f)])
			    (apply-changes new-salmon downstream-changes))
			  (move-dam salmon #f)))))
	      (move-dam salmon is-head?)))))
  (swim-upstream altn #t))

;; Simple location match utility function. If 'a' is a continutation of 'b',
;; such as in a='(cdr cdr car cdr car) b='(cdr cdr car), returns the tail of
;; 'a' after 'b', '(cdr car). Visa-versa for 'b' as a continuation of 'a'. If
;; 'a' and 'b' diverge at some point before the end, returns false.
(define (match-loc a b)
  (cond [(null? a) b]
	[(null? b) a]
	[(eq? (car a) (car b)) (match-loc (cdr a) (cdr b))]
	[#t #f]))

(define (match-loc-fst inside outside)
  (cond [(null? outside) inside]
	[(null? inside) #f]
	[(eq? (car outside) (car inside))
	 (match-loc-fst (cdr inside) (cdr outside))]
	[#t #f]))

;; Returns true if location 'a' is inside location 'b', false otherwise.
(define (is-inside? a b)
  (cond [(null? a) #f]
	[(null? b) #t]
	[(eq? (car a) (car b)) (is-inside? (cdr a) (cdr b))]
	[#t #f]))

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

;; Translates locations from a location within translations specified
;; by from-func to the location specified by to-func.
(define (translate-location loc translations from-func to-func)
  (debug "Translating" loc "in" translations "from" from-func "to" to-func
	 #:from 'translate-location #:tag 'info)

  (define (recurse translations)
    (if (null? translations)
	#f
	(let ([translation (car translations)])
	  (if (= 1 (length (from-func translation))) ;;Is the translation one-to-n?
	      (let ([tail (match-loc-fst loc
				     (car (from-func translation)))])
		(debug "Tail" tail "on" (car (from-func translation))
		       #:from 'translate-location #:tag 'info)
		(if tail
		    (map (lambda (l) (append l tail))
			 (to-func translation))
		    (recurse (cdr translations))))
	      (recurse (cdr translations))))))

  (recurse translations))

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

(define (rule-location-translations rule)
  (define (var-locs pattern loc)
    (cond
     [(list? pattern)
	(apply alist-append (location-map (lambda (x inner-loc)
				      (var-locs x (append loc
							  '(cdr)
							  inner-loc
							  '(car))))
				    (cdr pattern)))]
     [(number? pattern) '()]
     [(symbol? pattern) (list (cons pattern (list loc)))]
     [#t (error "Improper rule: " rule)]))
  (let ([in-locs (var-locs (rule-input rule) '())]
	[out-locs (var-locs (rule-output rule) '())])
    (map (lambda (x)
	   (list (cdr x) (cdr (assoc (car x) out-locs))))
	 in-locs)))

(define (alist-append . args) 
  (define (a-append joe bob)
    (if (null? joe)
	bob
	(a-append (cdr joe) (cons (cons (caar joe)
					(let ([match (assoc (caar joe) bob)])
					  (if match
					      (append (cdr match) (cdar joe))
					      (cdar joe))))
				  bob))))
  (if (< 2 (length args))
      (car args)
      (foldr (lambda (x y) (a-append x y)) '() args)))
