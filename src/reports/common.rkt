#lang racket
(provide format-time format-bits)

(define (format-time ms)
  (cond
   [(< ms 1000) (format "~ams" (round ms))]
   [(< ms 60000) (format "~as" (/ (round (/ ms 100.0)) 10))]
   [(< ms 3600000) (format "~am" (/ (round (/ ms 6000.0)) 10))]
   [else (format "~ahr" (/ (round (/ ms 360000.0)) 10))]))

(define (format-bits r #:sign [sign #f])
  (cond
   [(not r) ""]
   [(and (> r 0) sign) (format "+~ab" (/ (round (* r 10)) 10))]
   [else (format "~ab" (/ (round (* r 10)) 10))]))
