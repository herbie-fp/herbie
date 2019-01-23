#lang racket

(require "programs.rkt")
(require "common.rkt")

(provide (struct-out alt) make-alt alt? alt-program alt-cost alt-add-event)

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

(define (make-alt prog)
  (alt prog 'start '()))

(define (alt-cost altn)
  (program-cost (alt-program altn)))

(define (alt-add-event altn event)
  (alt (alt-program altn) event (list altn)))
