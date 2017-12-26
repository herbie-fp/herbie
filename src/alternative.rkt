#lang racket

(require "programs.rkt")
(require "points.rkt")
(require "core/matcher.rkt")
(require "common.rkt")

(provide (struct-out alt-delta) (struct-out alt-event) alternative?
         make-alt alt? alt-program alt-change alt-prev alt-add-event
         make-regime-alt
         alt-apply alt-rewrite-tree alt-rewrite-expression
         alt-errors alt-cost alt-rewrite-rm alt-set-prev
	 alt-initial alt-changes alt-history-length)

;; Alts are a lightweight audit trail.
;; An alt records a low-level view of how Herbie got
;; from one program to another.
;; They are a labeled linked list of changes.

(struct alt-delta (program change prev)
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt-delta " port)
           (write (alt-program alt) port)
           (display ">" port))])

(struct alt-event (program event prevs)
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt-event " port)
           (write (alt-program alt) port)
           (display ">" port))])

(define alternative? (or/c alt-delta? alt-event?))

(define (make-alt prog)
  (alt-event prog 'start '()))

(define (alt? altn)
  (or (alt-delta? altn) (alt-event? altn)))

(define (alt-program altn)
  (match altn
    [(alt-delta prog _ _) prog]
    [(alt-event prog _ _) prog]))

(define (alt-change altn)
  (match altn
    [(alt-delta _ cng _) cng]
    [(alt-event _ _ '()) #f]
    [(alt-event _ _ `(,prev ,_ ...)) (alt-change prev)]))

(define (alt-prev altn)
  (match altn
    [(alt-delta _ _ prev) prev]
    [(alt-event _ _ '()) #f]
    [(alt-event _ _ `(,prev ,_ ...)) (alt-prev prev)]))

(define (alt-errors altn)
  (errors (alt-program altn) (*pcontext*)))

(define (alt-cost altn)
  (program-cost (alt-program altn)))

(define (alt-apply altn . changes)
  (foldl (λ (cng altn)
            (alt-delta (change-apply cng (alt-program altn)) cng altn))
         altn changes))

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
    (map (curry apply alt-apply alt)
         (map reverse
              (rewrite-expression-head subtree #:root root-loc)))))

(define (alt-history-length alt)
  (if (alt-prev alt)
      (+ 1 (alt-history-length (alt-prev alt)))
      0))

(define (alt-set-prev altn prev)
  (alt-delta (alt-program altn) (alt-change altn) prev))

(define (alt-add-event altn event)
  (alt-event (alt-program altn) event (list altn)))

(define (make-regime-alt new-prog altns splitpoints)
  (alt-event new-prog (list 'regimes splitpoints) altns))
