(herbie-test (r a b)
  "r*sin(b)/cos(a+b), A"
  (/ (* r (sin b)) (cos (+ a b))))

(herbie-test (r a b)
  "r*sin(b)/cos(a+b), B"
  (* r (/ (sin b) (cos (+ a b)))))
