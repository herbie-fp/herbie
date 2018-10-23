(FPCore (x y)
  :name "Rump's expression from Stadtherr's award speech"
  :pre (and (= x 77617) (= y 33096))
  :spec -54767/66192
  (+ (* 333.75 (pow y 6))
     (* (* x x)
        (- (* 11 x x y y)
           (pow y 6)
           (* 121 (pow y 4))
           2))
     (* 5.5 (pow y 8))
     (/ x (* 2 y))))
