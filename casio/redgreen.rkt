#lang racket

(require casio/points)
(require casio/alternative)
(require casio/rules)
(require casio/programs)

(provide green? remove-red green-threshold orthogonal?)

(define green-threshold (make-parameter 25))

(define (error-sum alt)
  (apply + (alt-errors alt)))

(define (green? altn)
  (and (alt-prev altn) ; The initial alternative is not green-tipped by convention
       (< (green-threshold)
          (- (error-sum altn)
             (error-sum (alt-prev altn)))))) ;Hmm, this was how I had it originally, but it didn't handle NaN and Inf cases well. Are those cases not there anymore?

;; Eventually this should return an alternative with red changes undone.
(define (remove-red altn)
  altn)

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
      
