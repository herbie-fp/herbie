#lang racket

(require casio/programs)
(require casio/points)
(require casio/rules)
(require casio/common)
(require racket/pretty)


(provide (struct-out alt) make-alt alt-apply alt-rewrite-tree alt-rewrite-expression
	 apply-changes alternative<? alternative<>?)

(struct alt (program errors cost change prev) #:transparent
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt " port)
           (write (alt-program alt) port)
           (display ">" port))]
	#:methods gen:equal+hash
	[(define (equal-proc alt1 alt2 recursive)
	   (recursive (alt-program alt1) (alt-program alt2)))
	 (define (hash-proc altn recursive)
	   (recursive (alt-program altn)))
	 (define (hash2-proc altn recursive)
	   (recursive (alt-program altn)))])

(define (make-alt prog)
 (let* ([errs (errors prog (*points*) (*exacts*))])
    (alt prog errs (program-cost prog) #f #f)))

(define (alt-apply altn cng)
  (let* ([prog (change-apply cng (alt-program altn))]
         [errs (errors prog (*points*) (*exacts*))])
    (alt prog errs (program-cost prog) cng altn)))

;;Applies a list of changes to an alternative.
(define (apply-changes altn changes)
  (pipe altn (map (lambda (change)
		    (lambda (altn)
		      (alt-apply altn change)))
		  changes)))

(define (alt-rewrite-tree alt #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry alt-apply alt) (rewrite-tree subtree #:root root-loc))))

(define (alt-rewrite-expression alt #:destruct [destruct? #f] #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry alt-apply alt)
         (rewrite-expression subtree #:destruct destruct? #:root root-loc))))

(define (alternative<>? alt1 alt2)
  "Compare two alternatives; return if incomparable.
   Compares first by a lattice order on points, then by program cost."

  (let ([comparisons (errors-compare (alt-errors alt1) (alt-errors alt2))])
    (and (member '< comparisons) (member '> comparisons))))

(define (alternative<? alt1 alt2)
  "Compare two alternatives by a lattice order on pointwise error."

  (let ([comparisons (errors-compare (alt-errors alt1) (alt-errors alt2))])
    (andmap (negate (curry eq? '>)) comparisons)))
