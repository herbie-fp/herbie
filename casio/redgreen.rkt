#lang racket

(require casio/points)
(require casio/alternative)

(provide green? remove-red green-threshold)

(define green-threshold (make-parameter 0))

(define (error-sum alt)
  (apply + (alt-errors alt)))

(define (green? altn)
  (and (alt-prev altn) ; The initial alternative is not green-tipped by convention
       (< (green-threshold)
          (- (error-sum altn)
             (error-sum (alt-prev altn))))))

;; Eventually this should return an alternative with red changes undone.
(define (remove-red altn)
  altn)
