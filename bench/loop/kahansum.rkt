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
  (let* ([l (list x00 x01 x02 x03 x04 x05 x06 x07 x08 x09)])
    (for/fold ([s 0.0])
	([item l])
      (+ item s)))
  (let* ([l (list x00 x01 x02 x03 x04 x05 x06 x07 x08 x09)])
    (first-value
     (for/fold ([s 0.0]
		[c 0.0])
	 ([item l])
       (values (+ s (- item c))
	       (- (- (+ s (- item c)) s) (- item c)))))))
