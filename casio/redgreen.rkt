#lang racket

(require casio/points)
(require casio/alternative)
(require casio/rules)
(require casio/programs)
(require casio/common)

(provide green? remove-red green-threshold)

(define green-threshold (make-parameter 0))

(define (error-sum alt)
  (apply + (alt-errors alt)))

(define (real-number? x)
  (not (or (infinite? x) (nan? x))))

(define (diff-errors altn1 altn2)
  (let ((errors1 (alt-errors altn1))
	(errors2 (alt-errors altn2)))
    (let ([diff (for/list ([error1 errors1] [error2 errors2])
		  (cond [(and (real-number? error1) (real-number? error2))
			 (- (abs error1) (abs error2))]
			[(and (not (real-number? error1)) (real-number? error2))
			 +inf.0]
			[(and (real-number? error1) (not (real-number? error2)))
			 -inf.0]
			[#t 0.0]))])
      (letrec ([count (lambda (acc infs diff)
			(cond [(null? diff)
			       (cond [(> 0 infs) -inf.0]
				     [(< 0 infs) +inf.0]
				     [#t acc])]
			      [(real-number? (car diff))
			       (count (+ acc (car diff)) infs (cdr diff))]
			      [#t (count acc
					 (if (> 0 (car diff))
					     (+ 1 infs)
					     (- 1 infs))
					 (cdr diff))]))])
	(count 0 0 diff)))))
	     
(define (green? altn)
  (and (alt-prev altn) ; The initial alternative is not green-tipped by convention
       (> (green-threshold)
	  (diff-errors altn (alt-prev altn)))))

;; Terminology clarification: the "stream" in this metaphor flows from the original program to our passed alternative.
;; "Upstream" and "up" both mean backwards in the change history, "downstream" and "down" both mean forward in the
;; change history.
(define (remove-red altn)
  
  ;; If we're the first or second alt then we can't be moved back any farther.
  (define (done altn)
    (or (eq? (alt-prev altn) #f) (eq? (alt-prev (alt-prev altn)) #f)))

  ;; Returns (alt-change altn) translated up one change. The result is a
  ;; list of changes, since sometimes translating a change splits it.
  (define (get-upstream altn)
    (translated-up (alt-change altn)
		   (alt-change (alt-prev altn))
		   (alt-prev (alt-prev altn))))

  ;; Returns (alt-change (alt-prev altn)) translated down one change.
  ;; The result is a list of changes, since sometimes translating a change
  ;; splits it.
  (define (get-downstream altn)
    (translated-down (alt-change (alt-prev altn))
		     (alt-change altn)
		     (alt-prev (alt-prev altn))))

  ;; Takes the head of an alternative history and returns the head
  ;; of a new history containing the same changes where the leading
  ;; change has been moved down as far as possible.
  (define (swim-upstream salmon)
    (when (*debug*) (println salmon " is swimming."))
    (if (done salmon) salmon
	(let ([upstream-changes (get-upstream salmon)]
	      [downstream-changes (get-downstream salmon)])
	  (if (and upstream-changes downstream-changes)
	      (let* ([moved-salmon (apply-changes (alt-prev (alt-prev salmon)) upstream-changes)]
		     [new-salmon (swim-upstream moved-salmon)]
		     [new-head (apply-changes new-salmon downstream-changes)])
		new-head)
	      (let* ([dam (alt-prev salmon)]
		     [new-next (swim-upstream dam)])
		(if (eq? new-next dam)
		    salmon
		    (swim-upstream (alt-apply new-next (alt-change salmon)))))))))

  ;; Takes the head of an alternative history and returns a history
  ;; head which has the same change as the head passed, but with a
  ;; history that has as many items as possible removed from it without
  ;; compromising the head.
  (define (swim-head-upstream head-salmon)
    (if (done head-salmon) head-salmon
	(let ([upstream-changes (get-upstream head-salmon)])
	  (if upstream-changes
	      (let* ([moved-salmon (apply-changes (alt-prev (alt-prev head-salmon)) upstream-changes)]
		     [new-salmon (swim-head-upstream moved-salmon)])
		new-salmon)
	      (let* ([dam (alt-prev head-salmon)]
		     [new-next (swim-upstream dam)])
		(if (eq? new-next dam)
		    head-salmon
		    (swim-head-upstream (alt-apply new-next (alt-change head-salmon)))))))))
  
  (swim-head-upstream altn))

;; Simple location match utility function. If 'a' is a continutation of 'b',
;; such as in a='(cdr cdr car cdr car) b='(cdr cdr car), returns the tail of
;; 'a' after 'b', '(cdr car). Visa-versa for 'b' as a continuation of 'a'. If
;; 'a' and 'b' diverge at some point before the end, returns false.
(define (match-loc a b)
  (cond [(null? a) b]
	[(null? b) a]
	[(eq? (car a) (car b)) (match-loc (cdr a) (cdr b))]
	[#t #f]))

;; Returns true if location 'a' is inside location 'b', false otherwise.
(define (is-inside? a b)
  (cond [(null? a) #f]
	[(null? b) #t]
	[(eq? (car a) (car b)) (is-inside? (cdr a) (cdr b))]
	[#t #f]))

;; Takes a location translation of the form ((loc loc loc...) (loc loc loc...))
;; and returns true if the translation is one-to-one (of the form ((loc) (loc))).
(define (one-to-one? translation)
  (and (= 1 (length (car translation)))
       (= 1 (length (cadr translation)))))

;; Takes a location translation of the form ((loc loc loc...) (loc loc loc...))
;; and returns true if the translation is one-to-n (of the orm ((loc) (loc loc loc...))).
(define (one-to-n? translation)
  (= 1 (length (car translation))))

(define (n-to-one? translation)
  (= 1 (length (cadr translation))))

;; Takes a list of location tails, a single location head, and an original change,
;; and returns a list of changes that are identitical to the original change except
;; have locations of a tail appended to a head.
(define (loc-translated-changes loc-tails loc-head original)
  (map (lambda (loc)
	 (change (change-rule original)
		 (append loc-head loc)
		 (change-bindings original)))
       loc-tails))

(define (translated-up cur-change prev-change start)
  
  ;; Translates locations for rules that have one to one variable bindings.
  ;; post-rel-loc means post-relative-location, which means it's the location
  ;; after the translation (and we want the one before the translation), and
  ;; it's relative to the location of the rule for which the translations were
  ;; passed.
  (define (simple-translate-loc-up post-rel-loc translations) 
    (if (null? translations)
	#f
	(let ([cur-translation (car translations)])
	  (if (n-to-one? cur-translation)
	      (let ([tail (match-loc post-rel-loc (car (cadr cur-translation)))])
		(if tail
		    (map (lambda (l) (append l tail))
			 (car cur-translation))
		    (simple-translate-loc-up post-rel-loc (cdr translations))))
	      (simple-translate-loc-up post-rel-loc (cdr translations))))))
  
  (cond [(orthogonal? cur-change prev-change)
	 (list cur-change)]
	[(is-inside? (change-location cur-change) (change-location prev-change))
	 (let* ([translations (rule-location-translations (change-rule prev-change))]
		[relative-location (match-loc (change-location cur-change) (change-location prev-change))]
		[new-rel-locs (simple-translate-loc-up relative-location
						       translations)])
	   (if new-rel-locs ;simple-translate-loc returns false if it hits a case it can't translate.
	       (loc-translated-changes new-rel-locs (change-location prev-change) cur-change)
	       #f))]
	[(is-inside? (change-location prev-change) (change-location cur-change))
	 (let* ([rule* (change-rule cur-change)] ;The rule doesn't change
		[loc* (change-location cur-change)] ;The location doesn't change if prev-change is inside cur-change
		[bindings* (pattern-match (rule-input (change-rule cur-change))
					  (location-get (change-location cur-change) (alt-program start)))])
	   (if bindings* ;In this branch, a false value for bindings* is the way we find out we can't translate.
	       (list (change rule* loc* bindings*))
	       #f))]
	[#t (error "Something has gone horribly wrong")])) ;The way orthogonal? and is-inside? are defined, it should be that for all
                                                           ;a and all b, either (orthogonal? a b), (is-inside? a b), or (is-inside? b a)
	    
(define (translated-down cur-change next-change start)
  
  ;; Translates locations for rules that have one to one variable bindings.
  ;; pre-rel-loc means pre-relative-location, which means it's the location
  ;; before the translation (and we want the one after the translation), and
  ;; it's relative to the location of the rule for which the translations were
  ;; passed.
  (define (simple-translate-loc-down pre-rel-loc translations)
    (if (null? translations)
	#f
	(let ([cur-translation (car translations)])
	  (if (one-to-n? cur-translation)
	      (let ([tail (match-loc pre-rel-loc (caar cur-translation))])
		(if tail
		    (map (lambda (l) (append l tail))
			 (cadr cur-translation))
		    (simple-translate-loc-down pre-rel-loc (cdr translations))))
	      (simple-translate-loc-down pre-rel-loc (cdr translations))))))
  
  (cond [(orthogonal? cur-change next-change)
	 (list cur-change)]
	[(is-inside? (change-location cur-change) (change-location next-change))
	 (let* ([translations (rule-location-translations (change-rule next-change))]
		[new-rel-locs (simple-translate-loc-down (match-loc (change-location cur-change)
								   (change-location next-change))
							 translations)])
	   (if new-rel-locs ;simple-translate-loc returns false if it hits a case it can't translate.
	       (loc-translated-changes new-rel-locs (change-location next-change) cur-change)
	       #f))]
	[(is-inside? (change-location next-change) (change-location cur-change))
	 (let* ([rule* (change-rule cur-change)] ;The rule doesn't change
		[loc* (change-location cur-change)] ;The location doesn't change if prev-change is inside cur-change
		[bindings* (pattern-match (rule-input (change-rule cur-change))
					  (location-get (change-location cur-change) (alt-program start)))])
	   (if bindings* ;In this branch, a false value for bindings* is the way we find out we can't translate.
	       (list (change rule* loc* bindings*))
	       #f))]
	[#t (error "Something has gone horribly wrong")])) ;The way orthogonal? and is-inside? are defined, it should be that for all
                                                           ;a and all b, either (orthogonal? a b), (is-inside? a b), or (is-inside? b a)


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

;; Pipes an initial values through a list of funcs.
(define (pipe initial funcs)
  (if (null? funcs)
      initial
      (pipe ((car funcs) initial) (cdr funcs))))

;;Applies a list of changes to an alternative.
(define (apply-changes altn changes)
  (pipe altn (map (lambda (change)
		    (lambda (altn)
		      (alt-apply altn change)))
		  changes)))
