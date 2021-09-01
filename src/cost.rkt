#lang racket

(require "interface.rkt" "syntax/syntax.rkt" "syntax/sugar.rkt")
(provide program-cost expr-cost)

(define (operator-cost op bits)
  (* bits
    (match (impl->operator op)
     ['+     1]
     ['-     1]
     ['neg   1]
     ['*     1]
     ['/     1]
     ['abs   1]
     ['=     1]
     ['>     3]
     ['<     3]
     ['>=    3]
     ['<=    3]

     ['not       1]
     ['and       1]
     ['or        1]
     ['if        3]

     [(? repr-conv?) 2]
     [_         100])))

(define (expr-cost expr)
  (let loop ([expr expr] [repr (*output-repr*)])
    (match expr
     [(list 'if cond ift iff)
      (+ 1 (loop cond repr) (max (loop ift repr) (loop iff repr)))]
     [(list op args ...)
      (define ireprs (map get-representation (operator-info op 'itype)))
      (apply + (operator-cost op (representation-total-bits repr))
               (map loop args ireprs))]
     [_ (representation-total-bits repr)])))

(define (program-cost prog)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  (expr-cost body))
