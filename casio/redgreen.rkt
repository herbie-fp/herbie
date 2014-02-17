#lang racket

(require casio/points)
(require casio/alternative)

(provide green? remove-red green-threshold)

(define green-threshold (make-parameter 0))

(define (error-sum alt)
  (apply + (alt-errors alt)))

(define (green? alt)
  (and (alt-prev alt) ; The initial alternative is not green-tipped by convention
       (< (green-threshold)
          (- (error-sum alt)
             (error-sum (alt-prev alt))))))

;; Eventually this should return an alternative with red changes undone.
(define (remove-red alternative)
  alternative)
