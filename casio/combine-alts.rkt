#lang racket
(require casio/alternative)
(require casio/programs)
(require casio/rules)
(require casio/points)

(provide (all-defined-out))

(define (combine-alts alt0 alt1 dividor division-var)
  (let* ([split-initial-prog `(lambda ,(program-variables (alt-program alt0))
			       (if (< ,division-var ,dividor)
				   (program-body (alt-initial alt0))
				   (program-body (alt-initial alt1))))]
	 [split-initial-alt (make-alt split-initial-prog)])
    (build-alt split-initial-alt
	       (append (map (lambda (chng)
			      (change (change-rule chng)
				      (append '(cdr cdr car cdr cdr car) (change-location chng))
				      (change-bindings chng)))
			    (alt-changes alt0))
		       (map (lambda (chng)
			      (change (change-rule chng)
				      (append '(cdr cdr car cdr cdr cdr car) (change-location chng))
				      (change-bindings chng)))
			    (alt-changes alt1))))))
    
(define (get-splitpoint alt0 alt1)
  (let* ([err-diff (errors-compare (alt-errors alt0) (alt-errors alt1))]
	 [split-index (splitindex-from-difflist err-diff)])
    (bfind-splitpoint (alt-program alt0) (alt-program alt1) (list-ref *points* split-index) (list-ref *points* (+ 1 split-index)))))

(define (bfind-splitpoint prog1 prog2 start end)
  (if (= start end)
      start
      (let* ([midpoint (/ (+ start end) 2)]
	     [exact (make-exacts prog1 (list midpoint))])
	(if (> (abs (- ((eval-prog prog1 mode:fl) midpoint) exact))
	       (abs (- ((eval-prog prog2 mode:fl) midpoint) exact)))
	    (bfind-splitpoint prog1 prog2 midpoint end)
	    (bfind-splitpoint prog1 prog2 start midpoint)))))

(define (splitindex-from-difflist difflist)
  (let loop ([cur-index 0] [more-count 0] [diff-rest difflist])
    (cond [(null? diff-rest)
	   cur-index]
	  [(eq? (car diff-rest) '=)
	   (loop cur-index more-count (cdr diff-rest))]
	  [(and (eq? (car diff-rest) '<) (> 1 more-count))
	   (loop (- (length difflist) (length diff-rest)) more-count (cdr diff-rest))]
	  [(eq? (car diff-rest) '<)
	   (loop cur-index (- more-count 1) (cdr diff-rest))]
	  [(eq? (car diff-rest) '>)
	   (loop cur-index (+ more-count 1) (cdr diff-rest))])))

(define (get-splitpoints difflist

;; Gets the indices to split a region into. By default the only requirement of these regions is that they be the most accurate
;; regions where no region is less than three points in size, but you can pass in a minimum region size (default three), a
;; maximum number of splitindices, or a function that takes a single argument, a list of regions, and determines whether these
;; regions are general enough. 
(define (get-splitindices difflist #:min-region-size [min-size 3] #:max-splitpoints [max-splits +inf.0] #:fitness-func [fit? (const #t)])
  (let loop ([regions (swallow-regions (compose (curry > min-size) car)
				       (diff-list-to-regions difflist))]
	     [new-min-size (+ 1 min-size)])
    (if (and (< (length (filter (compose (compose not (curry eq? '=)) cdr) regions)) (+ 1 max-splits)) (fit? regions))
	(regions-to-splitindices (swallow-regions (compose (curry eq? '=) cdr) regions))
	(loop (swallow-regions (curry > new-min-size) regions) (+ 1 new-min-size)))))

(define (regions-to-splitindices regions)
  (cdr (reverse (cdr (foldl (lambda (reg acc) (cons (+ (car reg) (car acc)) acc)) '(0) regions)))))
	 
(define (diff-list-to-regions difflist)
  (let loop ([restlist difflist] [cur-region-size 0] [cur-region #f] [acc '()])
    (cond [(null? restlist)
	   (reverse (cons (cons cur-region-size cur-region) acc))]
	  [(eq? (car restlist) cur-region)
	   (loop (cdr restlist) (+ cur-region-size 1) cur-region acc)]
	  [#t (loop (cdr restlist) 1 (car restlist) (if cur-region
							(cons (cons cur-region-size cur-region) acc)
							acc))])))

(define (swallow-regions pred regions)
  (define (merge-into reg1 reg2)
    (cons (+ (car reg1) (car reg2)) (if (eq? '= (cdr reg2))
					(cdr reg1)
					(cdr reg2))))
  
  (define (merge-adjacent-regions regions)
    (reverse (foldl (lambda (reg acc)
		      (cond [(null? acc) (cons reg acc)]
			    [(eq? (cdr reg) (cdr (car acc)))
			     (cons (merge-into reg (car acc)) (cdr acc))]
			    [#t (cons reg acc)]))
		    '() regions)))
  (let loop ([restlist regions] [acc '()])
    (cond [(null? (cdr restlist)) (merge-adjacent-regions (let ([first-pass (reverse (cons (car restlist) acc))])
							    (if (and (pred (car first-pass)) (not (null? (cdr first-pass))))
								(cons (merge-into (car first-pass) (cadr first-pass)) (cddr first-pass))
								first-pass)))]
	  [(pred (cadr restlist))
	   (cond [(null? (cddr restlist))
		  (loop (list (merge-into (cadr restlist) (car restlist))) acc)]
		 [(eq? '= (cdar restlist))
		  (loop (list* (car restlist) (merge-into (cadr restlist) (caddr restlist)) (cdddr restlist)) acc)]
		 [#t (loop (list* (car restlist) (merge-into (cadr restlist) (caddr restlist)) (cdddr restlist))
			   acc)])]
	  [#t (loop (cdr restlist) (cons (car restlist) acc))])))
