#lang racket

(define-syntax (reap stx)
  "A reap/sow abstraction for filters and maps."
  (syntax-case stx ()
    [(_ [sow] body ...)
     #'(let* ([store '()]
              [sow (λ (elt) (set! store (cons elt store)) elt)])
         body ...
         (reverse store))]))

(define (println . args)
  (for ([val args])
    (if (string? val)
        (display val)
        (print val)))
  (newline)
  (when (not (null? args))
    (car args)))

(provide reap println)
