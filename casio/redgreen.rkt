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

(define (green? altn)
  (and (alt-prev altn) ; The initial alternative is not green-tipped by convention
       (< (green-threshold)
          (- (error-sum (alt-prev altn))
             (error-sum altn))))) ;Hmm, this was how I had it originally, but it didn't handle NaN and Inf cases well. Are those cases not there anymore?

;; Terminology clarification: the "stream" in this metaphor flows from the original program to our passed alternative.
;; "Upstream" and "up" both mean backwards in the change history, "downstream" and "down" both mean forward in the
;; change history.
(define (remove-red altn)
  
  ;; If we're the first or second alt then we can't be moved back any farther.
  (define (done altn)
    (or (eq? (alt-prev altn) #f) (eq? (alt-prev (alt-prev altn)) #f)))

  ;; Returns (alt-change altn) translated up one change.
  (define (get-upstream altn)
    (translated-up (alt-change altn)
		   (alt-change (alt-prev altn))
		   (alt-prev (alt-prev altn))))

  ;; Returns (alt-change (alt-prev altn)) translated down one change.
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
	(let ([upstream-change (get-upstream salmon)]
	      [downstream-change (get-downstream salmon)])
	  (if (and upstream-change downstream-change)
	      (let* ([moved-salmon (alt-apply (alt-prev (alt-prev salmon)) upstream-change)]
		     [new-salmon (swim-upstream moved-salmon)]
		     [new-head (alt-apply new-salmon downstream-change)])
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
	(let ([upstream-change (get-upstream head-salmon)])
	  (if upstream-change
	      (let* ([moved-salmon (alt-apply (alt-prev (alt-prev head-salmon)) upstream-change)]
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
;; and returns true if the translation is one-to-one (of the form ((loc) (loc)).
(define (one-to-one? translation)
  (and (= 1 (length (car translation)))
       (= 1 (length (cadr cur-translation)))))
  

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
	  (if (one-to-one? cur-translation);We can only handle the cases of one-to-one bindings currently
	      (let ([tail (match-loc post-rel-loc (car (cadr cur-translation)))])
		(if tail
		    (append (caar cur-translation) tail)
		    (simple-translate-loc-up post-rel-loc (cdr translations))))
	      (simple-translate-loc-up post-rel-loc (cdr translations))))))
  
  (cond [(orthogonal? cur-change prev-change)
	 cur-change]
	[(is-inside? (change-location cur-change) (change-location prev-change))
	 (let* ([translations (rule-location-translations (change-rule prev-change))]
		[relative-location (match-loc (change-location cur-change) (change-location prev-change))]
		[new-rel-loc (simple-translate-loc-up relative-location
						      translations)])
	   (if new-rel-loc ;simple-translate-loc returns false if it hits a case it can't translate.
	       (let ([rule* (change-rule cur-change)] ;The rule doesn't change
		     [loc* (append (change-location prev-change) new-rel-loc)]
		     [bindings* (change-bindings cur-change)]) ;The bindings don't change if cur-change is inside prev-change.
		 (change rule* loc* bindings*))
	       #f))]
	[(is-inside? (change-location prev-change) (change-location cur-change))
	 (let* ([rule* (change-rule cur-change)] ;The rule doesn't change
		[loc* (change-location cur-change)] ;The location doesn't change if prev-change is inside cur-change
		[bindings* (pattern-match (rule-input (change-rule cur-change))
					  (location-get (change-location cur-change) (alt-program start)))])
	   (if bindings* ;In this branch, a false value for bindings* is the way we find out we can't translate.
	       (change rule* loc* bindings*)
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
	  (if (one-to-one? cur-translation) ;We can only handle the cases of one-to-one bindings currently
	      (let ([tail (match-loc post-rel-loc (caar cur-translation))])
		(if tail
		    (append (car (cadr cur-translation)) tail)
		    (simple-translate-loc-down pre-rel-loc (cdr translations))))
	      (simple-translate-loc-down pre-rel-loc (cdr translations))))))
  
  (cond [(orthogonal? cur-change next-change)
	 cur-change]
	[(is-inside? (change-location cur-change) (change-location next-change))
	 (let* ([translations (rule-location-translations (change-rule next-change))]
		[new-rel-loc (simple-translate-loc-down (match-loc (change-location cur-change)
								 (change-location next-change))
						      translations)])
	   (if new-rel-loc ;simple-translate-loc returns false if it hits a case it can't translate.
	       (let ([rule* (change-rule cur-change)] ;The rule doesn't change
		     [loc* (append (change-location next-change) new-rel-loc)]
		     [bindings* (change-bindings cur-change)]) ;The bindings don't change if cur-change is inside prev-change.
		 (change rule* loc* bindings*))
	       #f))]
	[(is-inside? (change-location next-change) (change-location cur-change))
	 (let* ([rule* (change-rule cur-change)] ;The rule doesn't change
		[loc* (change-location cur-change)] ;The location doesn't change if prev-change is inside cur-change
		[bindings* (pattern-match (rule-input (change-rule cur-change))
					  (location-get (change-location cur-change) (alt-program start)))])
	   (if bindings* ;In this branch, a false value for bindings* is the way we find out we can't translate.
	       (change rule* loc* bindings*)
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
      
