#lang racket
(require casio/test)

(casio-test (a b)
  "Arvind's exp-of-sum-of-logs example"
  (exp (+ (log a) (log b)))
  (* a b))

(casio-test (a b)
  "Arvind's quotient-of-sum-of-exps example"
  (/ (exp a) (+ (exp a) (exp b)))
  (/ 1 (+ 1 (exp (- a b)))))

(casio-test (a1 a2 b1 b2)
  "Arvind's quotient-of-prods example"
  (/ (* a1 a2) (* b1 b2))
  (* (/ a1 b1) (/ a2 b2)))
