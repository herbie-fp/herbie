#lang racket
(require "../../herbie/test.rkt")

#|

s = 0.0
for e in l:
  s += e
return s

==>

s = 0.0
c = 0.0
for e in l:
  y = e - c
  t = s + y
  c = (t - s) - y
  s = t
return s

|#

(herbie-test ([lst (list 20 double .25)])
  "Kahan summation"
  (for/fold ([s 0.0])
      ([item lst])
    (+ item s))
  (for/fold ([s 0.0]
	     [c 0.0])
      ([item lst])
    (values (+ s (- item c))
	    (- (- (+ s (- item c)) s) (- item c)))))
