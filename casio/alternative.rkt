#lang racket

(require casio/programs)
(require casio/points)
(require casio/rules)
(require racket/pretty)

(provide (struct-out alt) make-alt alt-apply alt-rewrite-tree alt-rewrite-expression apply-changes)

(struct alt (program errors cost change prev) #:transparent
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt " port)
           (write (alt-program alt) port)
           (display ">" port))])

(define (make-alt prog)
 (let* ([errs (errors prog (*points*) (*exacts*))])
    (alt prog errs (program-cost prog) #f #f)))

(define (alt-apply altn cng)
  (let* ([prog (change-apply cng (alt-program altn))]
         [errs (errors prog (*points*) (*exacts*))])
    (alt prog errs (program-cost prog) cng altn)))

;; Pipes an initial values through a list of funcs.
(define (pipe initial funcs)
  ((apply compose (reverse funcs)) initial))

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
