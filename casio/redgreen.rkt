#lang racket

(require casio/points)
(require casio/alternative)
(require casio/rules)
(require casio/programs)
(require casio/common)

(provide green? remove-red green-threshold orthogonal?)

(define green-threshold (make-parameter 0))

(define (error-sum alt)
  (apply + (alt-errors alt)))

(define (green? altn)
  (and (alt-prev altn) ; The initial alternative is not green-tipped by convention
       (< (green-threshold)
          (- (error-sum altn)
             (error-sum (alt-prev altn)))))) ;Hmm, this was how I had it originally, but it didn't handle NaN and Inf cases well. Are those cases not there anymore?

;; Eventually this should return an alternative with red changes undone.
(define (remove-red altn)
  (define (swim-upstream salmon)
    (when (*debug*) (println salmon " is swimming."))
    (if (or (eq? (alt-prev salmon) #f ) (eq? (alt-change (alt-prev salmon)) #f)) salmon ;;We've reached the mouth of the river
	(let ([upstream-change (translated-up (alt-prev (alt-prev salmon)) (alt-change salmon) (alt-change (alt-prev salmon)))]
	      [downstream-change (translated-down (alt-prev (alt-prev salmon)) (alt-change (alt-prev salmon)) (alt-change salmon))])
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
  (define (remove-until-green head)
    (if (or (null? head) (eq? #f head) (green? head))
	head
	(remove-until-green (alt-prev head))))
  (if (green? altn)
      (remove-until-green (swim-upstream altn))
      altn))

;;Simple location match utility function. If 'a' is a continutation of 'b',
;;such as in a='(cdr cdr car cdr car) b='(cdr cdr car), returns the tail of
;;'a' after 'b', '(cdr car). Visa-versa for 'b' as a continuation of 'a'. If
;;'a' and 'b' diverge at some point before the end, returns false.
(define (match-loc a b)
  (cond [(null? a) b]
	[(null? b) a]
	[(eq? (car a) (car b)) (match-loc (cdr a) (cdr b))]
	[#t #f]))

(define (translated-up start cur-change prev-change)
  ;;Translates locations for rules that have one to one variable bindings,
  ;;as in each variable only appears once in the input and once in the output.
  (define (simple-translate-loc post-rel-loc translations)
    (if (null? translations)
	#f
	(let ([tail (match-loc post-rel-loc (car (cadr (car translations))))])
	  (if tail
	      (append (caaar translations) tail)
	      (simple-translate-loc post-rel-loc (cdr translations))))))
	
  (if (orthogonal? cur-change prev-change)
      cur-change ;If the changes are orthogonal, we don't need to transform the change to move it up.
      (let ([translations (rule-location-translations (change-rule (prev-change)))])
	(if (apply andmap (map (lambda (x)
				 (and (= 1 (length (car x)))
				      (= 1 (length (cadr x)))))
			       translations)) ;Test if the rule has one to one variable bindings (See simple-translate-loc)
	    (let ([new-rel-loc (simple-translate-loc (match-loc (change-location prev-change)
								(change-location cur-change))
						     translations)])
	      (if new-rel-loc ;simple-translate-loc could still fail if one change is not within the bindings of the other.
		  (change (change-rule cur-change) ;rule of our change hasn't changed.
			  (append (change-location prev-change) new-rel-loc) ;If new-rel-loc isn't null, we need to add it to the
			                                                     ;end of our location.
			  (pattern-match (rule-input (change-rule cur-change)) ;;Get new bindings in case they've changed.
					 (location-get (change-location cur-change)
						       (alt-program start))))
		  #f))
	    #f)))) ;Support for this case could be added in the future, it's just a little more complex and won't always work.
	    
(define (translated-down start cur-change next-change)
  (if (orthogonal? cur-change next-change)
      cur-change
      #f))

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
      
