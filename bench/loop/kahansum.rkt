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

(herbie-test ([lst (list 100 double .25)])
  "Kahan summation"
  (do-list ([s 0.0 (+ item s)])
           ([item lst])
           s)
  (do-list ([s 0.0 (+ s (- item c))]
            [c 0.0 (- (- (+ s (- item c)) s) (- item c))])
           ([item lst])
        (- s c)))
