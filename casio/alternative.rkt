#lang racket

(require casio/programs)
(require casio/points)
(require casio/rules)
(require racket/pretty)

(provide (struct-out alt) make-alt alt-apply alt-rewrite-tree alt-rewrite-expression)

(struct alt (program errors cost change prev) #:transparent
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt ")
           (pretty-print (alt-program alt))
           (display ">\n"))])

(define (make-alt prog)
 (let* ([errs (errors prog (*points*) (*exacts*))])
    (alt prog errs (program-cost prog) #f #f)))

(define (alt-apply alt cng)
  (let* ([prog (change-apply cng (alt-program alt))]
         [errs (errors prog (*points*) (*exacts*))])
    (alt prog errs (program-cost prog) cng alt)))

(define (alt-rewrite-tree alt #:root [root-loc '()])
  (let ([subtree (location-get (alt-program alt) root-loc)])
    (map (curry alt-change alt) (rewrite-tree subtree #:root root-loc))))

(define (alt-rewrite-expression alt #:destruct [destruct? #f] #:root [root-loc '()])
  (let ([subtree (location-get (alt-program alt) root-loc)])
    (map (curry alt-change alt)
         (rewrite-expression subtree #:destruct destruct? #:root root-loc))))
