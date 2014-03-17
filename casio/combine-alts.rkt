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

(define (build-alt initial changes)
  (if (null? changes)
      initial
      (build-alt (alt-apply initial (car changes)) (cdr changes))))

(define (alt-initial altn)
  (if (alt-prev altn)
      (alt-initial (alt-prev altn))
      altn))

(define (alt-changes altn)
  (let loop ([cur-alt altn] [acc '()])
    (if (alt-prev cur-alt)
	(loop (alt-prev cur-alt) (cons (alt-change cur-alt) acc))
	acc)))
    
(define (get-splitpoint alt0 alt1)
  (let* ([err-diff (errors-compare (alt-errors alt0) (alt-errors alt1))]
	 [split-index (splitindex-from-difflist err-diff)])
    (/ (+ (list-ref *points* split-index) (list-ref *points* (+ 1 split-index))) 2)))

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

(define (split-indicies-from-difflist difflist min-region-size)
  (let loop ([cur-index 0] [regime '=] [opp-count 0]
	     [diff-rest difflist] [acc '()] [cur-region-size 0])
    (cond [(null? diff-rest)
	   (cons cur-index acc)]
	  [(eq? (car diff-rest) '=)
	   (loop cur-index regime opp-count (cdr diff-rest) acc (+ 1 cur-region-size))]
	  [(and (eq? (car diff-rest) regime) (> 1 opp-count) (<= min-region-size cur-region-size))
	   (loop (- (length difflist) (length diff-rest)) regime opp-count (cdr diff-rest) acc (+ 1 cur-region-size))]
	  [(eq? (car diff-rest) regime)
	   (loop cur-index regime (- opp-count 1) (cdr diff-rest) acc (+ 1 cur-region-size))]
	  [(not (eq? (car diff-rest) regime))
	   (if (< min-region-size opp-count)
	       (loop (- (length difflist) (length diff-rest))
		     (if (eq? regime '<) '> '<)
		     0
		     (cdr diff-rest)
		     (cons cur-index acc)
		     opp-count)
	       (loop cur-index
		     regime
		     (+ 1 opp-count)
		     (cdr diff-rest)
		     acc
		     cur-region-size))])))
