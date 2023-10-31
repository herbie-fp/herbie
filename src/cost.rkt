#lang racket

(require "syntax/types.rkt" "syntax/syntax.rkt")
(provide expr-cost)

(define (operator-cost op)
  (* (representation-total-bits (impl-info op 'otype))
     (match (impl->operator op)
       [(or '+ '- 'neg '* '/ 'abs) 1]
       [(or 'not 'and 'or) 1]
       [(or '== '!= '< '> '<= '>=) 3]
       ['if 3]
       [(? repr-conv?) 2]
       [_ 100])))

(define (expr-cost expr repr)
  (let loop ([expr expr] [repr repr])
    (match expr
     [(list 'if cond ift iff)
      (+ 1 (loop cond repr) (max (loop ift repr) (loop iff repr)))]
     [(list op args ...)
      (define ireprs (impl-info op 'itype))
      (apply + (operator-cost op) (map loop args ireprs))]
     [_ (representation-total-bits repr)])))
