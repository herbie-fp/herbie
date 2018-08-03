#lang racket

(require "programs.rkt")
(require "points.rkt")
(require "core/matcher.rkt")
(require "common.rkt")

(provide alt-delta alt-delta? (struct-out alt)
         make-alt alt? alt-program alt-change
         alt-cost alt-add-event
         alt-apply alt-rewrite-expression alt-rewrite-rm)

;; Alts are a lightweight audit trail.
;; An alt records a low-level view of how Herbie got
;; from one program to another.
;; They are a labeled linked list of changes.

(struct alt (program event prevs)
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt " port)
           (write (alt-program alt) port)
           (display ">" port))])

(define (alt-delta program change prev)
  (alt program (list 'change change) (list prev)))

(define (make-alt prog)
  (alt prog 'start '()))

(define (alt-delta? altn)
  (match (alt-event altn)
    [(list 'change _) true]
    [_ false]))

(define (alt-change altn)
  (match altn
    [(alt _ (list 'change cng) _) cng]
    [(alt _ _ prevs) (ormap alt-change prevs)]))

(define (alt-prev altn)
  (match altn
    [(alt _ (list 'change cng) (list prev)) prev]
    [(alt _ _ '()) #f]
    [(alt _ _ `(,prev ,_ ...)) (alt-prev prev)]))

(define (alt-cost altn)
  (program-cost (alt-program altn)))

(define (alt-apply altn . changes)
  (foldl (λ (cng altn)
            (alt-delta (change-apply cng (alt-program altn)) cng altn))
         altn changes))

(define (alt-rewrite-expression alt #:destruct [destruct? #f] #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry alt-apply alt)
         (rewrite-expression subtree #:destruct destruct? #:root root-loc))))

(define (alt-rewrite-rm alt #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry apply alt-apply alt)
         (map reverse
              (rewrite-expression-head subtree #:root root-loc)))))

(define (alt-add-event altn event)
  (alt (alt-program altn) event (list altn)))
