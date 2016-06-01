(FPCore (r a b) :name "r*sin(b)/cos(a+b)" (/ (* r (sin b)) (cos (+ a b))))
(FPCore (r a b) :name "r*sin(b)/cos(a+b)" (* r (/ (sin b) (cos (+ a b)))))
