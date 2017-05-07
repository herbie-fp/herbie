#lang racket

(require rackunit)
(require "../core/simplify.rkt")

(define test-exprs
  '([1 1]
    [0 0]
    [(+ 1 0) 1]
    [(+ 1 5) 6]
    [(+ x 0) x]
    [(* x 1) x]
    [(- (+ x 1) x) 1]
    [(- (+ x 1) 1) x]
    [(/ (* x 3) x) 3]
    [(- (sqr (sqrt (+ x 1))) (sqr (sqrt x))) 1]))

(define simplify-test
  (test-suite
   "Test that simplification gets some basic things right"
   (for ([entry test-exprs])
     (match-let ([(list orig simpled) entry])
       (check-equal? simpled (simplify-expr orig))))))
