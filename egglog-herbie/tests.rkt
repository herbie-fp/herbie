#lang racket

(require egglog-herbie
         rackunit)

(define (test0)
  (define egg (make-egraph))
  (egraph-run! egg '(datatype M (Var String)))
  (egraph-run! egg
               '(let x (Var
                        "x")
                  ))
  (check-equal? (egraph-extract egg '(x)) '((Var "x")))
  (void))

(define (test1)
  (define egg (make-egraph))
  (egraph-run! egg '(datatype M (Var String) (Add M M)))
  (egraph-run! egg
               '(let x (Add
                        [Var "x"]
                        [Var "y"])
                  ))
  (egraph-run! egg '(rewrite (Add a b) (Add b a)))
  (egraph-run! egg '(run 1))
  (check-equal? (egraph-extract egg '(x) #:variants 2)
                '(((Add (Var "x") (Var "y")) (Add (Var "y") (Var "x")))))
  (void))

(module+ test
  (test0)
  (test1)
  (void))
