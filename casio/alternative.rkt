#lang racket

(require casio/programs)
(require casio/points)
(require casio/matcher)
(require casio/common)

(provide (struct-out alt) make-alt alt-apply alt-rewrite-tree alt-rewrite-expression
         alt-errors alt-cost alt-rewrite-rm apply-changes build-alt
	 alt-initial alt-changes alt-history-length)

;; Alts are a lightweight audit trail for Casio.
;; An alt records a low-level view of how Casio got
;; from one program to another.
;; They are a labeled linked list of changes.

(struct alt (program change prev) #:transparent
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt " port)
           (write (alt-program alt) port)
           (display ">" port))])

(define (make-alt prog)
  (alt prog #f #f))

(define (alt-errors altn)
  (errors (alt-program altn) (*points*) (*exacts*)))

(define (alt-cost altn)
  (program-cost (alt-program altn)))

(define (alt-apply altn cng)
  (alt (change-apply cng (alt-program altn)) cng altn))

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

(define (alt-history-length alt)
  (if (alt-prev alt)
      (+ 1 (alt-history-length (alt-prev alt)))
      0))
