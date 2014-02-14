#lang racket

(require casio/points)

(provide green? remove-red green-threshold)

(define green-threshold (make-parameter 50))

(define (error-sum alt)
  (apply + (change-errors alt)))

(define (green-tipped? alt)
  (or (alt-prev alt) ; The initial alternative is green-tipped by convention
       (< (green-threshold)
          (- (error-sum alt)
             (error-sum (alt-prev alt)))))

;; Eventually this should return an alternative with red changes undone.
(define (remove-red alternative)
  alternative)
