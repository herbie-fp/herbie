#lang racket

  (require
  "../src/syntax/types.rkt"
  "../src/syntax/types.rkt"
  "../src/syntax/platform.rkt"
  "../src/core/programs.rkt")

(provide get-ctx)

(define (get-ctx expr)
  (define free-vars  (sort (free-variables expr) symbol<?))
  (context
   free-vars
   (get-representation 'binary64)
   (make-list (length free-vars)
              (get-representation 'binary64))))