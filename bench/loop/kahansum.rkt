#lang racket
(require "../herbie/test.rkt")

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

(herbie-test (x00 x01 x02 x03 x04 x05 x06 x07 x08 x09)
  "Kahan summation"
  (let ((l '(x00 x01 x02 x03 x04 x05 x06 x07 x08 x09))
	(s 0.0))
    (for ([e l])
      (set! s (+ s e)))
    s)
  (let ((l '(x00 x01 x02 x03 x04 x05 x06 x07 x08 x09))
	(s 0.0)
	(c 0.0))
    (for ([e l])
      (let ((y (- e c))
	    (t (+ s y)))
	(set! c (- (- t s) y))
	(set! s t)))
    s))
