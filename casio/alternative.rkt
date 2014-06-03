#lang racket

(require casio/programs)
(require casio/points)
(require casio/matcher)
(require casio/common)
(require racket/pretty)

(provide (struct-out alt) make-alt alt-apply alt-rewrite-tree alt-rewrite-expression
         alt-rewrite-rm apply-changes build-alt
	 alt-initial alt-changes alt-cycles++)

(struct alt (program errors cost change prev cycles) #:transparent
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
    (alt prog errs (program-cost prog) #f #f 0)))

(define (alt-apply altn cng)
  (let* ([prog (change-apply cng (alt-program altn))]
         [errs (errors prog (*points*) (*exacts*))])
    (alt prog errs (program-cost prog) (change-add-hardness cng (alt-cycles altn)) altn 0)))

(define (alt-cycles++ altn)
  (alt (alt-program altn) (alt-errors altn) (alt-cost altn)
       (alt-change altn) (alt-prev altn) (add1 (alt-cycles altn))))

;;Applies a list of changes to an alternative.
(define (apply-changes altn changes)
  (pipe altn (map (lambda (change)
		    (lambda (altn)
		      (alt-apply altn change)))
		  changes)))

;; Builds an alt from an initial alt and a list of changes.
(define (build-alt initial changes)
  (if (null? changes)
      initial
      (build-alt (alt-apply initial (car changes)) (cdr changes))))

;; Gets the initial version of the current alt.
(define (alt-initial altn)
  (if (alt-prev altn)
      (alt-initial (alt-prev altn))
      altn))

;; Get a list of every change that's happened to the current alt, in application order.
(define (alt-changes altn)
  (let loop ([cur-alt altn] [acc '()])
    (if (alt-prev cur-alt)
	(loop (alt-prev cur-alt) (cons (alt-change cur-alt) acc))
	acc)))

(define (alt-rewrite-tree alt #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry alt-apply alt) (rewrite-tree subtree #:root root-loc))))

(define (alt-rewrite-expression alt #:destruct [destruct? #f] #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry alt-apply alt)
         (rewrite-expression subtree #:destruct destruct? #:root root-loc))))

(define (alt-rewrite-rm alt #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry apply-changes alt)
         (map reverse
              (rewrite-expression-head subtree #:root root-loc)))))
