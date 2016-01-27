(herbie-test (r a b)
  "r*sin(b)/cos(a+b)"
  (/ (* r (sin b)) (cos (+ a b))))

(herbie-test (r a b)
  "r*sin(b)/cos(a+b)"
  (* r (/ (sin b) (cos (+ a b)))))
