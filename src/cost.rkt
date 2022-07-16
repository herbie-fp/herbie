#lang racket

(require "syntax/types.rkt" "syntax/syntax.rkt")
(provide program-cost expr-cost)

(define (operator-cost op)
  (* (representation-total-bits (operator-info op 'otype))
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
      (define ireprs (operator-info op 'itype))
      (apply + (operator-cost op) (map loop args ireprs))]
     [_ (representation-total-bits repr)])))

(define (program-cost prog repr)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  (expr-cost body repr))
