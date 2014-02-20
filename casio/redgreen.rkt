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
	(let ([upstream-change (translated-up (alt-change salmon) (alt-change (alt-prev salmon)))]
	      [downstream-change (translated-down (alt-change (alt-prev salmon)) (alt-change salmon))])
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

(define (translated-up cur-change prev-change)
  (if (orthogonal? cur-change prev-change)
      cur-change
      #f))
(define (translated-down cur-change next-change)
  (if (orthogonal? cur-change next-change)
      cur-change
      #f))

(define (orthogonal? change-a change-b)
  (define (loc-match? loca locb)
    (if (or (null? loca) (null? locb))
	#t
	(if (eq? (car loca) (car locb))
	    (loc-match? (cdr loca) (cdr locb))
	    #f)))
  (not (loc-match? (change-location change-a) (change-location change-b))))

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
     [(symbol? pattern) (list (cons pattern loc))]
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
	(alist-append (cdr joe) (cons (cons (caar joe)
					    (let ([match (assoc (caar joe) bob)])
					      (if match
						  (append (cdr match) (cdar joe))
						  (cdar joe))))
				      bob))))
  (print "Appending: ")
  (print args)
  (newline)
  (if (< 2 (length args))
      (car args)
      (apply alist-append (append (a-append (car args) (cadr args)) (cddr args)))))
      
