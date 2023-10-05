#lang racket

(require "points.rkt" "syntax/types.rkt" "core/localize.rkt" "timeline.rkt"
         "common.rkt")

(provide group-errors)

(define (group-errors expr pcontext)  
  (match-define (cons subexprs pt-errorss)
    (flip-lists
     (hash->list (car (compute-local-errors (list expr) (*context*))))))

  (define pt-worst-subexprs
    (for/list ([pt-errors (in-list pt-errorss)])
      (match-define (cons worst-subexpr pt-max-error)
        (argmax cdr (map cons subexprs pt-errors)))
      (if (> pt-max-error 2) worst-subexpr #f)))

  (for/hash ([group (in-list (group-by identity pt-worst-subexprs))])
    (values (first group) (length group))))
