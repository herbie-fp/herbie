(FPCore
 (x y)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, A"
 :target
 (sqrt (+ x y))
 (sqrt (+ x y)))
(FPCore
 (x y z t)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, B"
 :target
 (/ (- (+ x y) z) (* t 2.0))
 (/ (- (+ x y) z) (* t 2.0)))
(FPCore
 (x y)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, C"
 :target
 (sqrt (fabs (- x y)))
 (sqrt (fabs (- x y))))
(FPCore
 (x y z t)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, D"
 :target
 (- x (+ (* x (/ y t)) (* (- z) (/ y t))))
 (+ x (/ (* y (- z x)) t)))
(FPCore
 (x y z t a)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, E"
 :target
 (if (< y -1.0761266216389975e-10)
   (+ x (/ 1 (/ (/ a (- z t)) y)))
   (if (< y 2.894426862792089e-49)
     (+ x (/ (* y (- z t)) a))
     (+ x (/ y (/ a (- z t))))))
 (+ x (/ (* y (- z t)) a)))
(FPCore
 (x y z t a)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, F"
 :target
 (if (< y -1.0761266216389975e-10)
   (- x (/ 1 (/ (/ a (- z t)) y)))
   (if (< y 2.894426862792089e-49)
     (- x (/ (* y (- z t)) a))
     (- x (/ y (/ a (- z t))))))
 (- x (/ (* y (- z t)) a)))
(FPCore
 (x y z)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, G"
 :target
 (* (+ x y) (+ z 1.0))
 (* (+ x y) (+ z 1.0)))
(FPCore
 (x y z)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, H"
 :target
 (* (+ x y) (- 1.0 z))
 (* (+ x y) (- 1.0 z)))
(FPCore
 (x y z)
 :name
 "Optimisation.CirclePacking:place from circle-packing-0.1.0.4, I"
 :target
 (+ (+ x y) z)
 (+ (+ x y) z))
(FPCore
 (x y z t a b c i j)
 :name
 "Data.Colour.Matrix:determinant from colour-2.3.3, A"
 :target
 (if (< x -1.469694296777705e-64)
   (+
    (-
     (* x (- (* y z) (* t a)))
     (/ (* b (- (sqr (* c z)) (sqr (* t i)))) (+ (* c z) (* t i))))
    (* j (- (* c a) (* y i))))
   (if (< x 3.2113527362226803e-147)
     (- (* (- (* b i) (* x a)) t) (- (* z (* c b)) (* j (- (* c a) (* y i)))))
     (+
      (-
       (* x (- (* y z) (* t a)))
       (/ (* b (- (sqr (* c z)) (sqr (* t i)))) (+ (* c z) (* t i))))
      (* j (- (* c a) (* y i))))))
 (+
  (- (* x (- (* y z) (* t a))) (* b (- (* c z) (* t i))))
  (* j (- (* c a) (* y i)))))
(FPCore
 (x y z t a)
 :name
 "Data.Colour.Matrix:inverse from colour-2.3.3, B"
 :target
 (if (< z -2.468684968699548e+170)
   (- (* (/ y a) x) (* (/ t a) z))
   (if (< z 6.309831121978371e-71)
     (/ (- (* x y) (* z t)) a)
     (- (* (/ y a) x) (* (/ t a) z))))
 (/ (- (* x y) (* z t)) a))
(FPCore
 (x y)
 :name
 "Data.Colour.CIE.Chromaticity:chromaCoords from colour-2.3.3"
 :target
 (- (- 1.0 x) y)
 (- (- 1.0 x) y))
(FPCore
 (x y)
 :name
 "Data.Colour.RGB:hslsv from colour-2.3.3, A"
 :target
 (/ (+ x y) 2.0)
 (/ (+ x y) 2.0))
(FPCore
 (x y z t a)
 :name
 "Data.Colour.RGB:hslsv from colour-2.3.3, B"
 :target
 (+ (/ 60.0 (/ (- z t) (- x y))) (* a 120.0))
 (+ (/ (* 60.0 (- x y)) (- z t)) (* a 120.0)))
(FPCore
 (x y)
 :name
 "Data.Colour.RGB:hslsv from colour-2.3.3, C"
 :target
 (- (/ x (- 2.0 (+ x y))) (/ y (- 2.0 (+ x y))))
 (/ (- x y) (- 2.0 (+ x y))))
(FPCore
 (x y)
 :name
 "Data.Colour.RGB:hslsv from colour-2.3.3, D"
 :target
 (- (/ x (+ x y)) (/ y (+ x y)))
 (/ (- x y) (+ x y)))
(FPCore
 (x y)
 :name
 "Data.Colour.RGB:hslsv from colour-2.3.3, E"
 :target
 (- 1 (/ y x))
 (/ (- x y) x))
(FPCore
 (x y)
 :name
 "Data.Colour.RGBSpace.HSL:hsl from colour-2.3.3, A"
 :target
 (- (+ x y) (* x y))
 (- (+ x y) (* x y)))
(FPCore
 (x y)
 :name
 "Data.Colour.RGBSpace.HSL:hsl from colour-2.3.3, B"
 :target
 (+ x (* x y))
 (* x (+ y 1.0)))
(FPCore
 (x y)
 :name
 "Data.Colour.RGBSpace.HSL:hsl from colour-2.3.3, C"
 :target
 (- (* x 2.0) y)
 (- (* x 2.0) y))
(FPCore
 (x y z)
 :name
 "Data.Colour.RGBSpace.HSL:hsl from colour-2.3.3, D"
 :target
 (+ x (* (* (- y x) 6.0) (- (/ 2.0 3.0) z)))
 (+ x (* (* (- y x) 6.0) (- (/ 2.0 3.0) z))))
(FPCore
 (x y z)
 :name
 "Data.Colour.RGBSpace.HSL:hsl from colour-2.3.3, E"
 :target
 (- x (* (* 6.0 z) (- x y)))
 (+ x (* (* (- y x) 6.0) z)))
(FPCore
 (x)
 :name
 "Data.Colour.RGBSpace.HSL:hsl from colour-2.3.3, F"
 :target
 (+ x (/ 1.0 3.0))
 (+ x (/ 1.0 3.0)))
(FPCore
 (x)
 :name
 "Data.Colour.RGBSpace.HSL:hsl from colour-2.3.3, G"
 :target
 (- x (/ 1.0 3.0))
 (- x (/ 1.0 3.0)))
(FPCore
 (x y)
 :name
 "Data.Colour.RGBSpace.HSV:hsv from colour-2.3.3, H"
 :target
 (* x (- 1.0 y))
 (* x (- 1.0 y)))
(FPCore
 (x y z)
 :name
 "Data.Colour.RGBSpace.HSV:hsv from colour-2.3.3, I"
 :target
 (* x (- 1.0 (* y z)))
 (* x (- 1.0 (* y z))))
(FPCore
 (x y z)
 :name
 "Data.Colour.RGBSpace.HSV:hsv from colour-2.3.3, J"
 :target
 (if (< (* x (- 1.0 (* (- 1.0 y) z))) -1.618195973607049e+50)
   (+ x (* (- 1.0 y) (* (- z) x)))
   (if (< (* x (- 1.0 (* (- 1.0 y) z))) 3.892237649663903e+134)
     (- (* (* x y) z) (- (* x z) x))
     (+ x (* (- 1.0 y) (* (- z) x)))))
 (* x (- 1.0 (* (- 1.0 y) z))))
(FPCore
 (x y)
 :name
 "Data.Colour.SRGB:invTransferFunction from colour-2.3.3"
 :target
 (/ (+ x y) (+ y 1.0))
 (/ (+ x y) (+ y 1.0)))
(FPCore
 (x y)
 :name
 "Data.Colour.SRGB:transferFunction from colour-2.3.3"
 :target
 (- (* (+ x 1.0) y) x)
 (- (* (+ x 1.0) y) x))
(FPCore
 (x)
 :name
 "Data.Colour.CIE:lightness from colour-2.3.3"
 :target
 (- (* x 116.0) 16.0)
 (- (* x 116.0) 16.0))
(FPCore
 (x)
 :name
 "Data.Colour.CIE:cieLABView from colour-2.3.3, A"
 :target
 (+ (* (/ 841.0 108.0) x) (/ 4.0 29.0))
 (+ (* (/ 841.0 108.0) x) (/ 4.0 29.0)))
(FPCore
 (x y)
 :name
 "Data.Colour.CIE:cieLABView from colour-2.3.3, B"
 :target
 (* 500.0 (- x y))
 (* 500.0 (- x y)))
(FPCore
 (x y)
 :name
 "Data.Colour.CIE:cieLABView from colour-2.3.3, C"
 :target
 (* 200.0 (- x y))
 (* 200.0 (- x y)))
(FPCore
 (x y)
 :name
 "Data.Colour.CIE:cieLAB from colour-2.3.3, A"
 :target
 (* y (- (* x 3.0) 0.41379310344827586))
 (* (* (- x (/ 16.0 116.0)) 3.0) y))
(FPCore
 (x)
 :name
 "Data.Colour.CIE:cieLAB from colour-2.3.3, B"
 :target
 (/ (+ x 16.0) 116.0)
 (/ (+ x 16.0) 116.0))
(FPCore
 (x y)
 :name
 "Data.Colour.CIE:cieLAB from colour-2.3.3, C"
 :target
 (+ x (/ y 500.0))
 (+ x (/ y 500.0)))
(FPCore
 (x y)
 :name
 "Data.Colour.CIE:cieLAB from colour-2.3.3, D"
 :target
 (- x (/ y 200.0))
 (- x (/ y 200.0)))
(FPCore
 (x y z t a)
 :name
 "Diagrams.Solve.Tridiagonal:solveTriDiagonal from diagrams-solve-0.1, A"
 :target
 (if (< z -32113435955957344.0)
   (- (/ x (- t (* a z))) (/ y (- (/ t z) a)))
   (if (< z 3.5139522372978296e-86)
     (* (- x (* y z)) (/ 1 (- t (* a z))))
     (- (/ x (- t (* a z))) (/ y (- (/ t z) a)))))
 (/ (- x (* y z)) (- t (* a z))))
(FPCore
 (x y z t)
 :name
 "Diagrams.Solve.Tridiagonal:solveTriDiagonal from diagrams-solve-0.1, B"
 :target
 (if (< x -1.618195973607049e+50)
   (/ 1 (- (/ y x) (* (/ z x) t)))
   (if (< x 2.1378306434876444e+131)
     (/ x (- y (* z t)))
     (/ 1 (- (/ y x) (* (/ z x) t)))))
 (/ x (- y (* z t))))
(FPCore
 (x y z)
 :name
 "Diagrams.Solve.Tridiagonal:solveTriDiagonal from diagrams-solve-0.1, C"
 :target
 (/ (+ x (* y z)) (/ (+ x (* y z)) (- x (* y z))))
 (- x (* y z)))
(FPCore
 (x y z)
 :name
 "Diagrams.Solve.Tridiagonal:solveCyclicTriDiagonal from diagrams-solve-0.1, A"
 :target
 (if (< z -4.262230790519429e-138)
   (/ (* x y) z)
   (if (< z 1.7042130660650472e-164) (/ x (/ z y)) (* (/ x z) y)))
 (/ (* x y) z))
(FPCore
 (x y z t a b)
 :name
 "Diagrams.Solve.Tridiagonal:solveCyclicTriDiagonal from diagrams-solve-0.1, B"
 :target
 (if (< t -1.3659085366310088e-271)
   (* 1 (* (+ x (* (/ y t) z)) (/ 1 (+ (+ a 1.0) (* (/ y t) b)))))
   (if (< t 3.036967103737246e-130)
     (/ z b)
     (* 1 (* (+ x (* (/ y t) z)) (/ 1 (+ (+ a 1.0) (* (/ y t) b)))))))
 (/ (+ x (/ (* y z) t)) (+ (+ a 1.0) (/ (* y b) t))))
(FPCore
 (x y z)
 :name
 "Diagrams.Solve.Polynomial:quadForm from diagrams-solve-0.1, A"
 :target
 (- x (* (* y 4.0) z))
 (- x (* (* y 4.0) z)))
(FPCore
 (x y z)
 :name
 "Diagrams.Solve.Polynomial:quadForm from diagrams-solve-0.1, B"
 :target
 (* (/ 1.0 2.0) (+ x (* y (sqrt z))))
 (* (/ 1.0 2.0) (+ x (* y (sqrt z)))))
(FPCore
 (x y)
 :name
 "Diagrams.Solve.Polynomial:quadForm from diagrams-solve-0.1, C"
 :target
 (/ x (* y 2.0))
 (/ x (* y 2.0)))
(FPCore
 (x y z t a b)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, A"
 :target
 (if (< y 7.590524218811189e-161)
   (+ (- (* x 2.0) (* (* (* y 9.0) z) t)) (* a (* 27.0 b)))
   (+ (- (* x 2.0) (* 9.0 (* y (* t z)))) (* (* a 27.0) b)))
 (+ (- (* x 2.0) (* (* (* y 9.0) z) t)) (* (* a 27.0) b)))
(FPCore
 (x y z)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, B"
 :target
 (- (* x (* 3.0 y)) z)
 (- (* (* x 3.0) y) z))
(FPCore
 (x y)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, C"
 :target
 (/ (/ x y) 3.0)
 (/ x (* y 3.0)))
(FPCore
 (x y z t)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, D"
 :target
 (/ (acos (* (/ (/ x 27.0) (* y z)) (/ (sqrt t) (/ 2.0 3.0)))) 3.0)
 (* (/ 1.0 3.0) (acos (* (/ (* 3.0 (/ x (* y 27.0))) (* z 2.0)) (sqrt t)))))
(FPCore
 (x y z t a b c i j k)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, E"
 :target
 (if (< t -1.6210815397541398e-69)
   (-
    (- (* (* 18.0 t) (* (* x y) z)) (* (+ (* a t) (* i x)) 4.0))
    (- (* (* k j) 27.0) (* c b)))
   (if (< t 165.68027943805222)
     (+
      (- (* (* 18.0 y) (* x (* z t))) (* (+ (* a t) (* i x)) 4.0))
      (- (* c b) (* 27.0 (* k j))))
     (-
      (- (* (* 18.0 t) (* (* x y) z)) (* (+ (* a t) (* i x)) 4.0))
      (- (* (* k j) 27.0) (* c b)))))
 (-
  (-
   (+ (- (* (* (* (* x 18.0) y) z) t) (* (* a 4.0) t)) (* b c))
   (* (* x 4.0) i))
  (* (* j 27.0) k)))
(FPCore
 (x y)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, F"
 :target
 (* (* x 27.0) y)
 (* (* x 27.0) y))
(FPCore
 (x y)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, G"
 :target
 (/ (+ x y) 2.0)
 (* (/ 1.0 2.0) (+ x y)))
(FPCore
 (x y z t)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, H"
 :target
 (+ (- x (/ y (* z 3.0))) (/ (/ t (* z 3.0)) y))
 (+ (- x (/ y (* z 3.0))) (/ t (* (* z 3.0) y))))
(FPCore
 (x y z t a)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, I"
 :target
 (if (< a -2.090464557976709e+86)
   (- (* 0.5 (/ (* y x) a)) (* 4.5 (/ t (/ a z))))
   (if (< a 2.144030707833976e+99)
     (/ (- (* x y) (* z (* 9.0 t))) (* a 2.0))
     (- (* (/ y a) (* x 0.5)) (* (/ t a) (* z 4.5)))))
 (/ (- (* x y) (* (* z 9.0) t)) (* a 2.0)))
(FPCore
 (x y z t a b c)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, J"
 :target
 (if (<
      (/ (+ (- (* (* x 9.0) y) (* (* (* z 4.0) t) a)) b) (* z c))
      -1.100156740804105e-171)
   (/ (+ (- (* (* x 9.0) y) (* (* z 4.0) (* t a))) b) (* z c))
   (if (< (/ (+ (- (* (* x 9.0) y) (* (* (* z 4.0) t) a)) b) (* z c)) -0.0)
     (/ (/ (+ (- (* (* x 9.0) y) (* (* (* z 4.0) t) a)) b) z) c)
     (if (<
          (/ (+ (- (* (* x 9.0) y) (* (* (* z 4.0) t) a)) b) (* z c))
          1.1708877911747488e-53)
       (/ (+ (- (* (* x 9.0) y) (* (* z 4.0) (* t a))) b) (* z c))
       (if (<
            (/ (+ (- (* (* x 9.0) y) (* (* (* z 4.0) t) a)) b) (* z c))
            2.876823679546137e+130)
         (-
          (+ (* (* 9.0 (/ y c)) (/ x z)) (/ b (* c z)))
          (* 4.0 (/ (* a t) c)))
         (if (<
              (/ (+ (- (* (* x 9.0) y) (* (* (* z 4.0) t) a)) b) (* z c))
              1.3838515042456319e+158)
           (/ (+ (- (* (* x 9.0) y) (* (* z 4.0) (* t a))) b) (* z c))
           (-
            (+ (* 9.0 (* (/ y (* c z)) x)) (/ b (* c z)))
            (* 4.0 (/ (* a t) c))))))))
 (/ (+ (- (* (* x 9.0) y) (* (* (* z 4.0) t) a)) b) (* z c)))
(FPCore
 (x y z t a b)
 :name
 "Diagrams.Solve.Polynomial:cubForm  from diagrams-solve-0.1, K"
 :target
 (if (< z -1.3793337487235141e+129)
   (-
    (* (* 2.0 (sqrt x)) (cos (- (/ 1 y) (/ (/ 0.3333333333333333 z) t))))
    (/ (/ a 3.0) b))
   (if (< z 3.516290613555987e+106)
     (- (* (* (sqrt x) 2.0) (cos (- y (* (/ t 3.0) z)))) (/ (/ a 3.0) b))
     (-
      (* (cos (- y (/ (/ 0.3333333333333333 z) t))) (* 2.0 (sqrt x)))
      (/ (/ a b) 3.0))))
 (- (* (* 2.0 (sqrt x)) (cos (- y (/ (* z t) 3.0)))) (/ a (* b 3.0))))
(FPCore
 (x y)
 :name
 "Diagrams.Solve.Polynomial:quartForm  from diagrams-solve-0.1, A"
 :target
 (- x (* (/ 3.0 8.0) y))
 (- x (* (/ 3.0 8.0) y)))
(FPCore
 (x y z t)
 :name
 "Diagrams.Solve.Polynomial:quartForm  from diagrams-solve-0.1, B"
 :target
 (- (+ (/ x 8.0) t) (* (/ z 2.0) y))
 (+ (- (* (/ 1.0 8.0) x) (/ (* y z) 2.0)) t))
(FPCore
 (x y z t a b c)
 :name
 "Diagrams.Solve.Polynomial:quartForm  from diagrams-solve-0.1, C"
 :target
 (+ (- (+ (* x y) (/ (* z t) 16.0)) (/ (* a b) 4.0)) c)
 (+ (- (+ (* x y) (/ (* z t) 16.0)) (/ (* a b) 4.0)) c))
(FPCore
 (x y z)
 :name
 "Diagrams.Solve.Polynomial:quartForm  from diagrams-solve-0.1, D"
 :target
 (- (/ (* x y) 2.0) (/ z 8.0))
 (- (/ (* x y) 2.0) (/ z 8.0)))
(FPCore
 (x y)
 :name
 "Diagrams.Solve.Polynomial:quartForm  from diagrams-solve-0.1, E"
 :target
 (- x (/ y 4.0))
 (- x (/ y 4.0)))
(FPCore
 (x y)
 :name
 "Text.Parsec.Token:makeTokenParser from parsec-3.1.9, A"
 :target
 (/ (+ x y) 10.0)
 (/ (+ x y) 10.0))
(FPCore
 (x y z)
 :name
 "Text.Parsec.Token:makeTokenParser from parsec-3.1.9, B"
 :target
 (* (+ x y) z)
 (* (+ x y) z))
(FPCore
 (x y)
 :name
 "Numeric.Interval.Internal:bisect from intervals-0.7.1, A"
 :target
 (* 0.5 (+ x y))
 (+ x (/ (- y x) 2.0)))
(FPCore
 (x y)
 :name
 "Numeric.Interval.Internal:scale from intervals-0.7.1, B"
 :target
 (/ (* x y) 2.0)
 (/ (* x y) 2.0))
(FPCore
 (x y z t)
 :name
 "Linear.V2:$cdot from linear-1.19.1.3, A"
 :target
 (+ (* x y) (* z t))
 (+ (* x y) (* z t)))
(FPCore
 (x y z t)
 :name
 "Linear.V3:cross from linear-1.19.1.3"
 :target
 (- (* x y) (* z t))
 (- (* x y) (* z t)))
(FPCore
 (x y z t a b)
 :name
 "Linear.V3:$cdot from linear-1.19.1.3, B"
 :target
 (+ (+ (* x y) (* z t)) (* a b))
 (+ (+ (* x y) (* z t)) (* a b)))
(FPCore
 (x y z t a b c i)
 :name
 "Linear.V4:$cdot from linear-1.19.1.3, C"
 :target
 (+ (+ (+ (* x y) (* z t)) (* a b)) (* c i))
 (+ (+ (+ (* x y) (* z t)) (* a b)) (* c i)))
(FPCore
 (x y z)
 :name
 "Main:bigenough2 from A"
 :target
 (+ x (* y (+ z x)))
 (+ x (* y (+ z x))))
(FPCore (x) :name "Main:bigenough1 from B" :target (+ (sqr x) x) (+ x (* x x)))
(FPCore
 (x)
 :name
 "Main:bigenough3 from C"
 :target
 (/ 1.0 (+ (sqrt (+ x 1.0)) (sqrt x)))
 (- (sqrt (+ x 1.0)) (sqrt x)))
(FPCore
 (x y z)
 :name
 "Linear.Quaternion:$c/ from linear-1.19.1.3, A"
 :target
 (+ (* (* 3 z) z) (* y x))
 (+ (+ (+ (* x y) (* z z)) (* z z)) (* z z)))
(FPCore
 (x y z)
 :name
 "Linear.Quaternion:$c/ from linear-1.19.1.3, B"
 :target
 (* (- x z) y)
 (+ (- (- (* x y) (* y z)) (* y y)) (* y y)))
(FPCore
 (x y z)
 :name
 "Linear.Quaternion:$c/ from linear-1.19.1.3, C"
 :target
 (* (- x z) y)
 (- (- (+ (* x y) (* y y)) (* y z)) (* y y)))
(FPCore
 (x y z)
 :name
 "Linear.Quaternion:$c/ from linear-1.19.1.3, D"
 :target
 (* (- x z) y)
 (- (+ (- (* x y) (* y y)) (* y y)) (* y z)))
(FPCore
 (x y)
 :name
 "Linear.Quaternion:$c/ from linear-1.19.1.3, E"
 :target
 (+ (sqr x) (* y (+ y (+ y y))))
 (+ (+ (+ (* x x) (* y y)) (* y y)) (* y y)))
(FPCore
 (x y)
 :name
 "Graphics.Rasterific.Linear:$cquadrance from Rasterific-0.6.1"
 :target
 (+ (sqr x) (sqr y))
 (+ (* x x) (* y y)))
(FPCore
 (x y z)
 :name
 "Linear.Quaternion:$ctanh from linear-1.19.1.3"
 :target
 (if (< z -4.2173720203427147e-29)
   (/ (* x (/ 1 (/ y (sin y)))) z)
   (if (< z 4.446702369113811e+64)
     (/ x (* z (/ y (sin y))))
     (/ (* x (/ 1 (/ y (sin y)))) z)))
 (/ (* x (/ (sin y) y)) z))
(FPCore
 (x y)
 :name
 "Linear.Quaternion:$ccosh from linear-1.19.1.3"
 :target
 (* (sin x) (/ (sinh y) x))
 (/ (* (sin x) (sinh y)) x))
(FPCore
 (x y)
 :name
 "Linear.Quaternion:$csinh from linear-1.19.1.3"
 :target
 (/ (* (cosh x) (sin y)) y)
 (* (cosh x) (/ (sin y) y)))
(FPCore
 (x y z)
 :name
 "Linear.Quaternion:$ctan from linear-1.19.1.3"
 :target
 (if (< y -4.618902267687042e-52)
   (* (/ (/ y z) x) (cosh x))
   (if (< y 1.038530535935153e-39)
     (/ (/ (* (cosh x) y) x) z)
     (* (/ (/ y z) x) (cosh x))))
 (/ (* (cosh x) (/ y x)) z))
(FPCore
 (x y)
 :name
 "Linear.Quaternion:$ccos from linear-1.19.1.3"
 :target
 (* (sin x) (/ (sinh y) y))
 (* (sin x) (/ (sinh y) y)))
(FPCore
 (x y)
 :name
 "Linear.Quaternion:$csin from linear-1.19.1.3"
 :target
 (* (cos x) (/ (sinh y) y))
 (* (cos x) (/ (sinh y) y)))
(FPCore
 (x y)
 :name
 "Linear.Quaternion:$clog from linear-1.19.1.3"
 :target
 (if (< x -1.5097698010472593e+153)
   (- (+ (* 1/2 (/ y x)) x))
   (if (< x 5.582399551122541e+57) (sqrt (+ (sqr x) y)) (+ (* 1/2 (/ y x)) x)))
 (sqrt (+ (* x x) y)))
(FPCore
 (x y)
 :name
 "Linear.Quaternion:$cexp from linear-1.19.1.3"
 :target
 (* x (/ (sin y) y))
 (* x (/ (sin y) y)))
(FPCore
 (x y z t a b c i j k y0 y1 y2 y3 y4 y5)
 :name
 "Linear.Matrix:det44 from linear-1.19.1.3"
 :target
 (if (< y4 -7.206256231996481e+60)
   (-
    (-
     (* (- (* b a) (* i c)) (- (* y x) (* t z)))
     (-
      (* (- (* j x) (* k z)) (- (* y0 b) (* i y1)))
      (* (- (* j t) (* k y)) (- (* y4 b) (* y5 i)))))
    (-
     (/ (- (* y2 t) (* y3 y)) (/ 1 (- (* y4 c) (* y5 a))))
     (* (- (* y2 k) (* y3 j)) (- (* y4 y1) (* y5 y0)))))
   (if (< y4 -3.364603505246317e-66)
     (+
      (-
       (- (- (* (* t c) (* i z)) (* (* a t) (* b z))) (* (* y c) (* i x)))
       (* (- (* b y0) (* i y1)) (- (* j x) (* k z))))
      (-
       (* (- (* y0 c) (* a y1)) (- (* x y2) (* z y3)))
       (-
        (* (- (* t y2) (* y y3)) (- (* y4 c) (* a y5)))
        (* (- (* y1 y4) (* y5 y0)) (- (* k y2) (* j y3))))))
     (if (< y4 -1.2000065055686116e-105)
       (+
        (+
         (-
          (* (- (* j t) (* k y)) (- (* y4 b) (* y5 i)))
          (* (* y3 y) (- (* y5 a) (* y4 c))))
         (+
          (* (* y5 a) (* t y2))
          (* (- (* k y2) (* j y3)) (- (* y4 y1) (* y5 y0)))))
        (-
         (* (- (* x y2) (* z y3)) (- (* c y0) (* a y1)))
         (-
          (* (- (* b y0) (* i y1)) (- (* j x) (* k z)))
          (* (- (* y x) (* z t)) (- (* b a) (* i c))))))
       (if (< y4 6.718963124057495e-279)
         (+
          (-
           (-
            (- (* (* k y) (* y5 i)) (* (* y b) (* y4 k)))
            (* (* y5 t) (* i j)))
           (-
            (* (- (* y2 t) (* y3 y)) (- (* y4 c) (* y5 a)))
            (* (- (* y2 k) (* y3 j)) (- (* y4 y1) (* y5 y0)))))
          (-
           (* (- (* b a) (* i c)) (- (* y x) (* t z)))
           (-
            (* (- (* j x) (* k z)) (- (* y0 b) (* i y1)))
            (* (- (* y2 x) (* y3 z)) (- (* c y0) (* y1 a))))))
         (if (< y4 4.77962681403792e-222)
           (+
            (+
             (-
              (* (- (* j t) (* k y)) (- (* y4 b) (* y5 i)))
              (* (* y3 y) (- (* y5 a) (* y4 c))))
             (+
              (* (* y5 a) (* t y2))
              (* (- (* k y2) (* j y3)) (- (* y4 y1) (* y5 y0)))))
            (-
             (* (- (* x y2) (* z y3)) (- (* c y0) (* a y1)))
             (-
              (* (- (* b y0) (* i y1)) (- (* j x) (* k z)))
              (* (- (* y x) (* z t)) (- (* b a) (* i c))))))
           (if (< y4 2.2852241541266835e-175)
             (+
              (-
               (-
                (- (* (* k y) (* y5 i)) (* (* y b) (* y4 k)))
                (* (* y5 t) (* i j)))
               (-
                (* (- (* y2 t) (* y3 y)) (- (* y4 c) (* y5 a)))
                (* (- (* y2 k) (* y3 j)) (- (* y4 y1) (* y5 y0)))))
              (-
               (* (- (* b a) (* i c)) (- (* y x) (* t z)))
               (-
                (* (- (* j x) (* k z)) (- (* y0 b) (* i y1)))
                (* (- (* y2 x) (* y3 z)) (- (* c y0) (* y1 a))))))
             (+
              (-
               (+
                (+
                 (-
                  (* (- (* x y) (* z t)) (- (* a b) (* c i)))
                  (-
                   (* k (* i (* z y1)))
                   (+ (* j (* i (* x y1))) (* y0 (* k (* z b))))))
                 (-
                  (* z (* y3 (* a y1)))
                  (+ (* y2 (* x (* a y1))) (* y0 (* z (* c y3))))))
                (* (- (* t j) (* y k)) (- (* y4 b) (* y5 i))))
               (* (- (* t y2) (* y y3)) (- (* y4 c) (* y5 a))))
              (* (- (* k y2) (* j y3)) (- (* y4 y1) (* y5 y0))))))))))
 (+
  (-
   (+
    (+
     (-
      (* (- (* x y) (* z t)) (- (* a b) (* c i)))
      (* (- (* x j) (* z k)) (- (* y0 b) (* y1 i))))
     (* (- (* x y2) (* z y3)) (- (* y0 c) (* y1 a))))
    (* (- (* t j) (* y k)) (- (* y4 b) (* y5 i))))
   (* (- (* t y2) (* y y3)) (- (* y4 c) (* y5 a))))
  (* (- (* k y2) (* j y3)) (- (* y4 y1) (* y5 y0)))))
(FPCore
 (x y z t a b c i j)
 :name
 "Linear.Matrix:det33 from linear-1.19.1.3"
 :target
 (if (< t -8.120978919195912e-33)
   (-
    (* x (- (* z y) (* a t)))
    (- (* b (- (* z c) (* a i))) (* (- (* c t) (* y i)) j)))
   (if (< t -4.712553818218485e-169)
     (+
      (- (* x (- (* y z) (* t a))) (* b (- (* c z) (* i a))))
      (/ (* j (- (sqr (* c t)) (sqr (* i y)))) (+ (* c t) (* i y))))
     (if (< t -7.633533346031584e-308)
       (-
        (* x (- (* z y) (* a t)))
        (- (* b (- (* z c) (* a i))) (* (- (* c t) (* y i)) j)))
       (if (< t 1.0535888557455487e-139)
         (+
          (- (* x (- (* y z) (* t a))) (* b (- (* c z) (* i a))))
          (/ (* j (- (sqr (* c t)) (sqr (* i y)))) (+ (* c t) (* i y))))
         (-
          (* x (- (* z y) (* a t)))
          (- (* b (- (* z c) (* a i))) (* (- (* c t) (* y i)) j)))))))
 (+
  (- (* x (- (* y z) (* t a))) (* b (- (* c z) (* i a))))
  (* j (- (* c t) (* i y)))))
(FPCore
 (x y)
 :name
 "Linear.Matrix:fromQuaternion from linear-1.19.1.3, A"
 :target
 (* (* x 2.0) (- x y))
 (* 2.0 (- (* x x) (* x y))))
(FPCore
 (x y)
 :name
 "Linear.Matrix:fromQuaternion from linear-1.19.1.3, B"
 :target
 (* (* x 2.0) (+ x y))
 (* 2.0 (+ (* x x) (* x y))))
(FPCore
 (x y z t)
 :name
 "Linear.Projection:inverseInfinitePerspective from linear-1.19.1.3"
 :target
 (if (< t -9.231879582886777e-80)
   (* (* y t) (- x z))
   (if (< t 2.543067051564877e+83) (* y (* t (- x z))) (* (* y (- x z)) t)))
 (* (- (* x y) (* z y)) t))
(FPCore
 (x y z t)
 :name
 "Linear.Projection:infinitePerspective from linear-1.19.1.3, A"
 :target
 (if (< (/ (* x 2.0) (- (* y z) (* t z))) -2.559141628295061e-13)
   (* (/ x (* (- y t) z)) 2.0)
   (if (< (/ (* x 2.0) (- (* y z) (* t z))) 1.045027827330126e-269)
     (/ (* (/ x z) 2.0) (- y t))
     (* (/ x (* (- y t) z)) 2.0)))
 (/ (* x 2.0) (- (* y z) (* t z))))
(FPCore
 (x y)
 :name
 "Linear.Projection:inversePerspective from linear-1.19.1.3, B"
 :target
 (- (/ 0.5 y) (/ 0.5 x))
 (/ (- x y) (* (* x 2.0) y)))
(FPCore
 (x y)
 :name
 "Linear.Projection:inversePerspective from linear-1.19.1.3, C"
 :target
 (+ (/ 0.5 x) (/ 0.5 y))
 (/ (+ x y) (* (* x 2.0) y)))
(FPCore
 (x y)
 :name
 "Linear.Projection:perspective from linear-1.19.1.3, A"
 :target
 (/ 1 (- (/ x (+ x y)) (/ y (+ x y))))
 (/ (+ x y) (- x y)))
(FPCore
 (x y)
 :name
 "Linear.Projection:perspective from linear-1.19.1.3, B"
 :target
 (if (< x -1.7210442634149447e+81)
   (* (/ (* 2.0 x) (- x y)) y)
   (if (< x 8.364504563556443e+16)
     (/ (* x 2.0) (/ (- x y) y))
     (* (/ (* 2.0 x) (- x y)) y)))
 (/ (* (* x 2.0) y) (- x y)))
(FPCore
 (x y)
 :name
 "Physics.ForceLayout:coulombForce from force-layout-0.4.0.2"
 :target
 (/ (/ x y) y)
 (/ x (* y y)))
(FPCore
 (x y)
 :name
 "Codec.Picture.Types:toneMapping from JuicyPixels-3.2.6.1"
 :target
 (* (/ x 1) (/ (+ (/ x y) 1.0) (+ x 1.0)))
 (/ (* x (+ (/ x y) 1.0)) (+ x 1.0)))
(FPCore
 (x y z t a b)
 :name
 "Codec.Picture.Jpg.FastDct:referenceDct from JuicyPixels-3.2.6.1"
 :target
 (* x (cos (* (/ b 16.0) (/ t (+ (- 1.0 (* a 2.0)) (sqr (* a 2.0)))))))
 (*
  (* x (cos (/ (* (* (+ (* y 2.0) 1.0) z) t) 16.0)))
  (cos (/ (* (* (+ (* a 2.0) 1.0) b) t) 16.0))))
(FPCore
 (x)
 :name
 "Main:i from "
 :target
 (+ (+ (+ (+ x x) x) x) x)
 (+ (+ (+ (+ x x) x) x) x))
(FPCore
 (x y z t)
 :name
 "Main:z from "
 :target
 (+
  (+
   (+
    (/ 1.0 (+ (sqrt (+ x 1.0)) (sqrt x)))
    (/ 1.0 (+ (sqrt (+ y 1.0)) (sqrt y))))
   (/ 1.0 (+ (sqrt (+ z 1.0)) (sqrt z))))
  (- (sqrt (+ t 1.0)) (sqrt t)))
 (+
  (+
   (+ (- (sqrt (+ x 1.0)) (sqrt x)) (- (sqrt (+ y 1.0)) (sqrt y)))
   (- (sqrt (+ z 1.0)) (sqrt z)))
  (- (sqrt (+ t 1.0)) (sqrt t))))
(FPCore
 (x y z t a b c i)
 :name
 "Diagrams.ThreeD.Shapes:frustum from diagrams-lib-1.3.0.3, A"
 :target
 (* 2.0 (- (+ (* x y) (* z t)) (* (+ a (* b c)) (* c i))))
 (* 2.0 (- (+ (* x y) (* z t)) (* (* (+ a (* b c)) c) i))))
(FPCore
 (x y z)
 :name
 "Diagrams.ThreeD.Shapes:frustum from diagrams-lib-1.3.0.3, B"
 :target
 (+ x (* (- y x) z))
 (+ x (* (- y x) z)))
(FPCore
 (x y z)
 :name
 "Diagrams.ThreeD.Transform:aboutY from diagrams-lib-1.3.0.3"
 :target
 (+ (* x (cos y)) (* z (sin y)))
 (+ (* x (cos y)) (* z (sin y))))
(FPCore
 (x y z)
 :name
 "Diagrams.ThreeD.Transform:aboutX from diagrams-lib-1.3.0.3, A"
 :target
 (- (* x (cos y)) (* z (sin y)))
 (- (* x (cos y)) (* z (sin y))))
(FPCore
 (x y z)
 :name
 "Diagrams.ThreeD.Transform:aboutX from diagrams-lib-1.3.0.3, B"
 :target
 (+ (* x (sin y)) (* z (cos y)))
 (+ (* x (sin y)) (* z (cos y))))
(FPCore
 (x y z)
 :name
 "Diagrams.Color.HSV:lerp  from diagrams-contrib-1.3.0.5"
 :target
 (- y (* x (- y z)))
 (+ (* (- 1.0 x) y) (* x z)))
(FPCore
 (x y z)
 :name
 "Diagrams.TwoD.Segment.Bernstein:evaluateBernstein from diagrams-lib-1.3.0.3"
 :target
 (if (< x -2.71483106713436e-162)
   (- (* (+ 1 y) (/ x z)) x)
   (if (< x 3.874108816439546e-197)
     (* (* x (+ (- y z) 1.0)) (/ 1 z))
     (- (* (+ 1 y) (/ x z)) x)))
 (/ (* x (+ (- y z) 1.0)) z))
(FPCore
 (x y)
 :name
 "Diagrams.Segment:$catParam from diagrams-lib-1.3.0.3, A"
 :target
 (* (* x 3.0) (* x y))
 (* (* (* x 3.0) x) y))
(FPCore
 (x y)
 :name
 "Diagrams.Segment:$catParam from diagrams-lib-1.3.0.3, B"
 :target
 (* (* x (* 3.0 y)) y)
 (* (* (* x 3.0) y) y))
(FPCore
 (x)
 :name
 "Diagrams.Segment:$catParam from diagrams-lib-1.3.0.3, C"
 :target
 (* x (* x x))
 (* (* x x) x))
(FPCore
 (x)
 :name
 "Diagrams.Tangent:$catParam from diagrams-lib-1.3.0.3, D"
 :target
 (+ 3.0 (- (* (* 9.0 x) x) (* 12.0 x)))
 (* 3.0 (+ (- (* (* x 3.0) x) (* x 4.0)) 1.0)))
(FPCore
 (x)
 :name
 "Diagrams.Tangent:$catParam from diagrams-lib-1.3.0.3, E"
 :target
 (- (* 6.0 x) (* 9.0 (sqr x)))
 (* (* 3.0 (- 2.0 (* x 3.0))) x))
(FPCore
 (x)
 :name
 "Diagrams.Tangent:$catParam from diagrams-lib-1.3.0.3, F"
 :target
 (* (* 3.0 x) x)
 (* (* x 3.0) x))
(FPCore
 (x y z t)
 :name
 "Diagrams.Trail:splitAtParam  from diagrams-lib-1.3.0.3, A"
 :target
 (/ (+ x (- (/ y (- t (/ x z))) (/ x (- (* t z) x)))) (+ x 1.0))
 (/ (+ x (/ (- (* y z) x) (- (* t z) x))) (+ x 1.0)))
(FPCore
 (x y)
 :name
 "Diagrams.Trail:splitAtParam  from diagrams-lib-1.3.0.3, B"
 :target
 (if (< y -3693.8482788297247)
   (- (/ x (sqr y)) (- (/ x y) x))
   (if (< y 6799310503.41891)
     (/ (* x y) (+ y 1.0))
     (- (/ x (sqr y)) (- (/ x y) x))))
 (/ (* x y) (+ y 1.0)))
(FPCore
 (x y)
 :name
 "Diagrams.Trail:splitAtParam  from diagrams-lib-1.3.0.3, C"
 :target
 (/ (- x y) (- 1.0 y))
 (/ (- x y) (- 1.0 y)))
(FPCore
 (x y)
 :name
 "Diagrams.Trail:splitAtParam  from diagrams-lib-1.3.0.3, D"
 :target
 (if (< y -3693.8482788297247)
   (- (/ 1.0 y) (- (/ x y) x))
   (if (< y 6799310503.41891)
     (- 1.0 (/ (* (- 1.0 x) y) (+ y 1.0)))
     (- (/ 1.0 y) (- (/ x y) x))))
 (- 1.0 (/ (* (- 1.0 x) y) (+ y 1.0))))
(FPCore
 (x y)
 :name
 "Diagrams.TwoD.Arc:bezierFromSweepQ1 from diagrams-lib-1.3.0.3"
 :target
 (* (/ (- 1.0 x) y) (/ (- 3.0 x) 3.0))
 (/ (* (- 1.0 x) (- 3.0 x)) (* y 3.0)))
(FPCore
 (x y)
 :name
 "Diagrams.TwoD.Arc:arcBetween from diagrams-lib-1.3.0.3"
 :target
 (if (<
      (/ (- (* x x) (* (* y 4.0) y)) (+ (* x x) (* (* y 4.0) y)))
      0.9743233849626781)
   (-
    (/ (sqr x) (+ (sqr x) (* (sqr y) 4.0)))
    (/ (* (sqr y) 4.0) (+ (sqr x) (* (sqr y) 4.0))))
   (-
    (sqr (/ x (sqrt (+ (sqr x) (* (sqr y) 4.0)))))
    (/ (* (sqr y) 4.0) (+ (sqr x) (* (sqr y) 4.0)))))
 (/ (- (* x x) (* (* y 4.0) y)) (+ (* x x) (* (* y 4.0) y))))
(FPCore
 (x)
 :name
 "Diagrams.TwoD.Ellipse:ellipse from diagrams-lib-1.3.0.3"
 :target
 (sqrt (- 1.0 (sqr x)))
 (sqrt (- 1.0 (* x x))))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Chart.Axis.Types:invLinMap from Chart-1.5.3"
 :target
 (if (< z -1.2536131056095036e+188)
   (- t (* (/ y z) (- t x)))
   (if (< z 4.446702369113811e+64)
     (+ x (/ (- y z) (/ (- a z) (- t x))))
     (- t (* (/ y z) (- t x)))))
 (+ x (/ (* (- y z) (- t x)) (- a z))))
(FPCore
 (x y z)
 :name
 "Diagrams.TwoD.Segment:bezierClip from diagrams-lib-1.3.0.3"
 :target
 (- z (* (- z x) y))
 (+ (* x y) (* z (- 1.0 y))))
(FPCore
 (x y)
 :name
 "Data.Octree.Internal:octantDistance  from Octree-0.5.4.2"
 :target
 (if (< x -1.1236950826599826e+145)
   (- x)
   (if (< x 1.116557621183362e+93) (sqrt (+ (sqr x) (sqr y))) x))
 (sqrt (+ (* x x) (* y y))))
(FPCore
 (x y)
 :name
 "Graphics.Rasterific.Shading:$sradialGradientWithFocusShader from Rasterific-0.6.1"
 :target
 (- x (sqr y))
 (- x (* y y)))
(FPCore
 (x y)
 :name
 "Diagrams.TwoD.Path.Metafont.Internal:hobbyF from diagrams-contrib-1.3.0.5"
 :target
 (/
  (+
   2.0
   (*
    (log
     (exp
      (*
       (* (sqrt 2.0) (- (sin x) (/ (sin y) 16.0)))
       (- (sin y) (/ (sin x) 16.0)))))
    (- (cos x) (cos y))))
  (*
   3.0
   (+
    (+ 1.0 (* (/ (- (sqrt 5.0) 1.0) 2.0) (cos x)))
    (* (/ (- 3.0 (sqrt 5.0)) 2.0) (cos y)))))
 (/
  (+
   2.0
   (*
    (*
     (* (sqrt 2.0) (- (sin x) (/ (sin y) 16.0)))
     (- (sin y) (/ (sin x) 16.0)))
    (- (cos x) (cos y))))
  (*
   3.0
   (+
    (+ 1.0 (* (/ (- (sqrt 5.0) 1.0) 2.0) (cos x)))
    (* (/ (- 3.0 (sqrt 5.0)) 2.0) (cos y))))))
(FPCore
 (x y)
 :name
 "Diagrams.TwoD.Layout.CirclePacking:approxRadius from diagrams-contrib-1.3.0.5"
 :target
 (if (< y -1.2303690911306994e+114)
   1.0
   (if (< y -9.102852406811914e-222)
     (/
      (sin (/ x (* y 2.0)))
      (* (sin (/ x (* y 2.0))) (log (exp (cos (/ x (* y 2.0)))))))
     1.0))
 (/ (tan (/ x (* y 2.0))) (sin (/ x (* y 2.0)))))
(FPCore
 (x y z)
 :name
 "Diagrams.TwoD.Apollonian:descartes from diagrams-contrib-1.3.0.5"
 :target
 (if (< z 7.636950090573675e+176)
   (* 2.0 (sqrt (+ (* (+ x y) z) (* x y))))
   (*
    (sqr
     (+
      (* 1/4 (* (* (pow y -3/4) (* (pow z -3/4) x)) (+ y z)))
      (* (pow z 1/4) (pow y 1/4))))
    2.0))
 (* 2.0 (sqrt (+ (+ (* x y) (* x z)) (* y z)))))
(FPCore
 (x y z)
 :name
 "Diagrams.TwoD.Apollonian:initialConfig from diagrams-contrib-1.3.0.5, A"
 :target
 (- (* y 0.5) (* (* (/ 0.5 y) (+ z x)) (- z x)))
 (/ (- (+ (* x x) (* y y)) (* z z)) (* y 2.0)))
(FPCore
 (x y z)
 :name
 "Diagrams.TwoD.Apollonian:initialConfig from diagrams-contrib-1.3.0.5, B"
 :target
 (if (< y 2.5816096488251695e-278)
   (- (* x y))
   (* x (* (sqrt (+ y z)) (sqrt (- y z)))))
 (* x (sqrt (- (* y y) (* z z)))))
(FPCore
 (x y z)
 :name
 "Diagrams.Backend.Rasterific:rasterificRadialGradient from diagrams-rasterific-1.3.1.3"
 :target
 (- (+ y (/ x z)) (/ y (/ z x)))
 (/ (+ x (* y (- z x))) z))
(FPCore
 (x y z t)
 :name
 "Data.HashTable.ST.Basic:computeOverhead from hashtables-1.2.0.2"
 :target
 (- (/ (+ (/ 2.0 z) 2.0) t) (- 2.0 (/ x y)))
 (+ (/ x y) (/ (+ 2.0 (* (* z 2.0) (- 1.0 t))) (* t z))))
(FPCore
 (x y z t)
 :name
 "Language.Haskell.HsColour.ColourHighlight:unbase from hscolour-1.23"
 :target
 (+ (* (+ (* x y) z) y) t)
 (+ (* (+ (* x y) z) y) t))
(FPCore
 (x)
 :name
 "System.Random.MWC.Distributions:blocks from mwc-random-0.13.3.2"
 :target
 (* (* 0.5 x) x)
 (* (* x 0.5) x))
(FPCore
 (x y)
 :name
 "System.Random.MWC.Distributions:standard from mwc-random-0.13.3.2"
 :target
 (* 0.5 (- (sqr x) y))
 (* 0.5 (- (* x x) y)))
(FPCore
 (x y z)
 :name
 "SynthBasics:oscSampleBasedAux from YampaSynth-0.2"
 :target
 (+ x (* y (- z x)))
 (+ x (* y (- z x))))
(FPCore
 (x y z t)
 :name
 "System.Random.MWC.Distributions:truncatedExp from mwc-random-0.13.3.2"
 :target
 (if (< z -2.8874623088207947e+119)
   (-
    (- x (/ (/ (- 0.5) (* y t)) (sqr z)))
    (* (/ (- 0.5) (* y t)) (/ (/ 2.0 z) (sqr z))))
   (- x (/ (log (+ 1.0 (* z y))) t)))
 (- x (/ (log (+ (- 1.0 y) (* y (exp z)))) t)))
(FPCore
 (x y z)
 :name
 "System.Random.MWC.Distributions:gamma from mwc-random-0.13.3.2"
 :target
 (- (+ y (* 0.5 x)) (* y (- z (log z))))
 (+ (* x 0.5) (* y (+ (- 1.0 z) (log z)))))
(FPCore
 (x y)
 :name
 "AI.Clustering.Hierarchical.Internal:average from clustering-0.2.1, A"
 :target
 (/ x (+ x y))
 (/ x (+ x y)))
(FPCore
 (x)
 :name
 "Numeric.Integration.TanhSinh:nonNegative from integration-0.2.1"
 :target
 (/ x (- 1.0 x))
 (/ x (- 1.0 x)))
(FPCore
 (x y z)
 :name
 "Graphics.Rasterific.QuadraticFormula:discriminant from Rasterific-0.6.1"
 :target
 (- (sqr x) (* z (* y 4.0)))
 (- (* x x) (* (* y 4.0) z)))
(FPCore
 (x y z t a b)
 :name
 "Graphics.Rasterific.CubicBezier:cachedBezierAt from Rasterific-0.6.1"
 :target
 (if (< z -1.1820553527347888e+19)
   (+ (* z (+ (* b a) y)) (+ x (* t a)))
   (if (< z 4.7589743188364287e-122)
     (+ (* (+ (* b z) t) a) (+ (* z y) x))
     (+ (* z (+ (* b a) y)) (+ x (* t a)))))
 (+ (+ (+ x (* y z)) (* t a)) (* (* a z) b)))
(FPCore
 (x)
 :name
 "Graphics.Rasterific.CubicBezier:isSufficientlyFlat from Rasterific-0.6.1"
 :target
 (* (* 16.0 x) x)
 (* (* x 16.0) x))
(FPCore
 (x y z)
 :name
 "Graphics.Rasterific.Shading:$sgradientColorAt from Rasterific-0.6.1"
 :target
 (- (/ x (- z y)) (/ y (- z y)))
 (/ (- x y) (- z y)))
(FPCore
 (x)
 :name
 "Graphics.Rasterific.Shading:$sradialGradientWithFocusShader from Rasterific-0.6.1, A"
 :target
 (+ (sqr x) 1.0)
 (+ (* x x) 1.0))
(FPCore
 (x y z t)
 :name
 "Graphics.Rasterific.Shading:$sradialGradientWithFocusShader from Rasterific-0.6.1, B"
 :target
 (- (sqr x) (* 4.0 (* y (- (sqr z) t))))
 (- (* x x) (* (* y 4.0) (- (* z z) t))))
(FPCore
 (x y)
 :name
 "Data.Number.Erf:$dmerfcx from erf-2.0.0.0"
 :target
 (* x (pow (exp y) y))
 (* x (exp (* y y))))
(FPCore
 (x y z t)
 :name
 "Data.Number.Erf:$cinvnormcdf from erf-2.0.0.0, A"
 :target
 (* (* (- (* x 0.5) y) (sqrt (* z 2.0))) (pow (exp 1) (/ (* t t) 2.0)))
 (* (* (- (* x 0.5) y) (sqrt (* z 2.0))) (exp (/ (* t t) 2.0))))
(FPCore
 (x y)
 :name
 "Data.Number.Erf:$cinvnormcdf from erf-2.0.0.0, B"
 :target
 (- x (/ y (+ 1.0 (/ (* x y) 2.0))))
 (- x (/ y (+ 1.0 (/ (* x y) 2.0)))))
(FPCore
 (x y z t)
 :name
 "Numeric.AD.Rank1.Halley:findZero from ad-4.2.4"
 :target
 (- x (/ 1 (- (/ z y) (/ (/ t 2.0) z))))
 (- x (/ (* (* y 2.0) z) (- (* (* z 2.0) z) (* y t)))))
(FPCore
 (x y z)
 :name
 "Crypto.Random.Test:calculate from crypto-random-0.0.9"
 :target
 (+ x (* y (/ y z)))
 (+ x (/ (* y y) z)))
(FPCore
 (x)
 :name
 "Numeric.Log:$cexpm1 from log-domain-0.10.2.1, A"
 :target
 (* (* 2.0 x) x)
 (* (* x 2.0) x))
(FPCore
 (x y)
 :name
 "Numeric.Log:$cexpm1 from log-domain-0.10.2.1, B"
 :target
 (+ (+ (* x y) x) y)
 (+ (+ (* x y) x) y))
(FPCore
 (x y)
 :name
 "Numeric.Log:$clog1p from log-domain-0.10.2.1, A"
 :target
 (+ (sqr y) (+ (* 2.0 x) (sqr x)))
 (+ (+ (* x 2.0) (* x x)) (* y y)))
(FPCore
 (x)
 :name
 "Numeric.Log:$clog1p from log-domain-0.10.2.1, B"
 :target
 (/ x (+ 1.0 (sqrt (+ x 1.0))))
 (/ x (+ 1.0 (sqrt (+ x 1.0)))))
(FPCore
 (x)
 :name
 "Data.Approximate.Numerics:blog from approximate-0.2.2.1"
 :target
 (/ 6.0 (/ (+ (+ x 1.0) (* 4.0 (sqrt x))) (- x 1.0)))
 (/ (* 6.0 (- x 1.0)) (+ (+ x 1.0) (* 4.0 (sqrt x)))))
(FPCore
 (x)
 :name
 "Graphics.Rasterific.Svg.PathConverter:segmentToBezier from rasterific-svg-0.2.3.1, A"
 :target
 (/ (/ (* 8.0 (sin (* x 0.5))) 3.0) (/ (sin x) (sin (* x 0.5))))
 (/ (* (* (/ 8.0 3.0) (sin (* x 0.5))) (sin (* x 0.5))) (sin x)))
(FPCore
 (x y z)
 :name
 "Graphics.Rasterific.Svg.PathConverter:segmentToBezier from rasterific-svg-0.2.3.1, B"
 :target
 (- (+ x (cos y)) (* z (sin y)))
 (- (+ x (cos y)) (* z (sin y))))
(FPCore
 (x y z)
 :name
 "Graphics.Rasterific.Svg.PathConverter:segmentToBezier from rasterific-svg-0.2.3.1, C"
 :target
 (+ (+ x (sin y)) (* z (cos y)))
 (+ (+ x (sin y)) (* z (cos y))))
(FPCore
 (x y z t)
 :name
 "Graphics.Rasterific.Svg.PathConverter:arcToSegments from rasterific-svg-0.2.3.1"
 :target
 (+ (sqr (/ x y)) (sqr (/ z t)))
 (+ (/ (* x x) (* y y)) (/ (* z z) (* t t))))
(FPCore
 (x)
 :name
 "Development.Shake.Profile:generateTrace from shake-0.15.5"
 :target
 0
 (* 1000000.0 (- x x)))
(FPCore
 (x y z t a b)
 :name
 "Development.Shake.Progress:decay from shake-0.15.5"
 :target
 (- (/ (+ (* z t) (* y x)) (+ y (* z (- b y)))) (/ a (+ (- b y) (/ y z))))
 (/ (+ (* x y) (* z (- t a))) (+ y (* z (- b y)))))
(FPCore
 (x y)
 :name
 "Development.Shake.Progress:message from shake-0.15.5"
 :target
 (* (/ x 1) (/ 100.0 (+ x y)))
 (/ (* x 100.0) (+ x y)))
(FPCore
 (x y z)
 :name
 "Diagrams.Backend.Rasterific:$crender from diagrams-rasterific-1.3.1.3"
 :target
 (+ (* x y) (* (- 1.0 x) z))
 (+ (* x y) (* (- 1.0 x) z)))
(FPCore
 (x y z t)
 :name
 "Numeric.Histogram:binBounds from Chart-1.5.3"
 :target
 (if (< x -9.025511195533005e-135)
   (- x (* (/ z t) (- x y)))
   (if (< x 4.275032163700715e-250)
     (+ x (* (/ (- y x) t) z))
     (+ x (/ (- y x) (/ t z)))))
 (+ x (/ (* (- y x) z) t)))
(FPCore
 (x y z)
 :name
 "Graphics.Rendering.Chart.Drawing:drawTextsR from Chart-1.5.3"
 :target
 (+ (* x y) (* (- x 1.0) z))
 (+ (* x y) (* (- x 1.0) z)))
(FPCore
 (x y)
 :name
 "Graphics.Rendering.Chart.Axis.Types:hBufferRect from Chart-1.5.3"
 :target
 (- (* 1.5 x) (* 0.5 y))
 (+ x (/ (- x y) 2.0)))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Chart.Axis.Types:linMap from Chart-1.5.3"
 :target
 (if (< a -1.6153062845442575e-142)
   (+ x (* (/ (- y x) 1) (/ (- z t) (- a t))))
   (if (< a 3.774403170083174e-182)
     (- y (* (/ z t) (- y x)))
     (+ x (* (/ (- y x) 1) (/ (- z t) (- a t))))))
 (+ x (/ (* (- y x) (- z t)) (- a t))))
(FPCore
 (x y)
 :name
 "Graphics.Rendering.Chart.Plot.Vectors:renderPlotVectors from Chart-1.5.3"
 :target
 (- (* y x) (- y 1.0))
 (+ x (* (- 1.0 x) (- 1.0 y))))
(FPCore
 (x y)
 :name
 "Graphics.Rendering.Chart.Plot.AreaSpots:renderSpotLegend from Chart-1.5.3"
 :target
 (+ x (/ (fabs (- y x)) 2.0))
 (+ x (/ (fabs (- y x)) 2.0)))
(FPCore
 (x y z t)
 :name
 "Graphics.Rendering.Chart.Plot.AreaSpots:renderAreaSpots4D from Chart-1.5.3"
 :target
 (/ x (/ (- t z) (- y z)))
 (/ (* x (- y z)) (- t z)))
(FPCore
 (x y)
 :name
 "Graphics.Rendering.Chart.Plot.Pie:renderPie from Chart-1.5.3"
 :target
 (- y 0)
 (- (+ x y) x))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Chart.SparkLine:renderSparkLine from Chart-1.5.3"
 :target
 (- x (* (/ (- y z) (+ (- t z) 1.0)) a))
 (- x (/ (- y z) (/ (+ (- t z) 1.0) a))))
(FPCore
 (x y z)
 :name
 "Graphics.Rendering.Chart.Backend.Diagrams:calcFontMetrics from Chart-diagrams-1.5.1, A"
 :target
 (if (< y -3.7429310762689856e+171)
   (* (/ (+ y x) (- y)) z)
   (if (< y 3.5534662456086734e+168)
     (/ (+ x y) (- 1.0 (/ y z)))
     (* (/ (+ y x) (- y)) z)))
 (/ (+ x y) (- 1.0 (/ y z))))
(FPCore
 (x y z t)
 :name
 "Graphics.Rendering.Chart.Backend.Diagrams:calcFontMetrics from Chart-diagrams-1.5.1, B"
 :target
 (if (< (/ (* (/ y z) t) t) -1.20672205123045e+245)
   (/ y (/ z x))
   (if (< (/ (* (/ y z) t) t) -5.907522236933906e-275)
     (* x (/ y z))
     (if (< (/ (* (/ y z) t) t) 5.658954423153415e-65)
       (/ y (/ z x))
       (if (< (/ (* (/ y z) t) t) 2.0087180502407133e+217)
         (* x (/ y z))
         (/ (* y x) z)))))
 (* x (/ (* (/ y z) t) t)))
(FPCore
 (x y)
 :name
 "AI.Clustering.Hierarchical.Internal:average from clustering-0.2.1, B"
 :target
 (/ x (+ y x))
 (/ x (+ y x)))
(FPCore
 (x y z t a b)
 :name
 "AI.Clustering.Hierarchical.Internal:ward from clustering-0.2.1"
 :target
 (if (<
      (/ (- (+ (* (+ x y) z) (* (+ t y) a)) (* y b)) (+ (+ x t) y))
      -3.5813117084150564e+153)
   (- (+ z a) b)
   (if (<
        (/ (- (+ (* (+ x y) z) (* (+ t y) a)) (* y b)) (+ (+ x t) y))
        1.2285964308315609e+82)
     (/ 1 (/ (+ (+ x t) y) (- (+ (* (+ x y) z) (* (+ t y) a)) (* y b))))
     (- (+ z a) b)))
 (/ (- (+ (* (+ x y) z) (* (+ t y) a)) (* y b)) (+ (+ x t) y)))
(FPCore
 (x y z)
 :name
 "Numeric.SpecFunctions:invErfc from math-functions-0.1.5.2, A"
 :target
 (+ x (/ 1 (- (* (/ 1.1283791670955126 y) (exp z)) x)))
 (+ x (/ y (- (* 1.1283791670955126 (exp z)) (* x y)))))
(FPCore
 (x)
 :name
 "Numeric.SpecFunctions:invErfc from math-functions-0.1.5.2, B"
 :target
 (*
  0.70711
  (- (/ (+ 2.30753 (* x 0.27061)) (+ 1.0 (* x (+ 0.99229 (* x 0.04481))))) x))
 (*
  0.70711
  (- (/ (+ 2.30753 (* x 0.27061)) (+ 1.0 (* x (+ 0.99229 (* x 0.04481))))) x)))
(FPCore
 (x y)
 :name
 "Numeric.SpecFunctions:logGamma from math-functions-0.1.5.2, A"
 :target
 (+ (- (* x (- y 1.0)) (* y 0.5)) 0.918938533204673)
 (+ (- (* x (- y 1.0)) (* y 0.5)) 0.918938533204673))
(FPCore
 (x y z)
 :name
 "Numeric.SpecFunctions:logGamma from math-functions-0.1.5.2, B"
 :target
 (if (< z -8120153.652456675)
   (-
    (* (+ (/ 0.07512208616047561 z) 0.0692910599291889) y)
    (- (/ (* 0.40462203869992125 y) (sqr z)) x))
   (if (< z 6.576118972787377e+20)
     (+
      x
      (*
       (*
        y
        (+
         (* (+ (* z 0.0692910599291889) 0.4917317610505968) z)
         0.279195317918525))
       (/ 1 (+ (* (+ z 6.012459259764103) z) 3.350343815022304))))
     (-
      (* (+ (/ 0.07512208616047561 z) 0.0692910599291889) y)
      (- (/ (* 0.40462203869992125 y) (sqr z)) x))))
 (+
  x
  (/
   (*
    y
    (+
     (* (+ (* z 0.0692910599291889) 0.4917317610505968) z)
     0.279195317918525))
   (+ (* (+ z 6.012459259764103) z) 3.350343815022304))))
(FPCore
 (x y z)
 :name
 "Numeric.SpecFunctions:logGamma from math-functions-0.1.5.2, C"
 :target
 (if (< x -3.326128725870005e+62)
   (- (+ (/ y (sqr x)) (* 4.16438922228 x)) 110.1139242984811)
   (if (< x 9.429991714554673e+55)
     (*
      (/ (- x 2.0) 1)
      (/
       (+
        (*
         (+
          (* (+ (* (+ (* x 4.16438922228) 78.6994924154) x) 137.519416416) x)
          y)
         x)
        z)
       (+
        (*
         (+
          (+ (* 263.505074721 x) (+ (* 43.3400022514 (sqr x)) (* x (* x x))))
          313.399215894)
         x)
        47.066876606)))
     (- (+ (/ y (sqr x)) (* 4.16438922228 x)) 110.1139242984811)))
 (/
  (*
   (- x 2.0)
   (+
    (*
     (+ (* (+ (* (+ (* x 4.16438922228) 78.6994924154) x) 137.519416416) x) y)
     x)
    z))
  (+
   (* (+ (* (+ (* (+ x 43.3400022514) x) 263.505074721) x) 313.399215894) x)
   47.066876606)))
(FPCore
 (x y z t a b)
 :name
 "Numeric.SpecFunctions:logGamma from math-functions-0.1.5.2, D"
 :target
 (if (< z -6.499344996252632e+53)
   (+
    x
    (* (+ (- 3.13060547623 (/ 36.527041698806414 z)) (/ t (sqr z))) (/ y 1)))
   (if (< z 7.066965436914287e+59)
     (+
      x
      (/
       y
       (/
        (+
         (*
          (+ (* (+ (* (+ z 15.234687407) z) 31.4690115749) z) 11.9400905721)
          z)
         0.607771387771)
        (+
         (* (+ (* (+ (* (+ (* z 3.13060547623) 11.1667541262) z) t) z) a) z)
         b))))
     (+
      x
      (*
       (+ (- 3.13060547623 (/ 36.527041698806414 z)) (/ t (sqr z)))
       (/ y 1)))))
 (+
  x
  (/
   (*
    y
    (+ (* (+ (* (+ (* (+ (* z 3.13060547623) 11.1667541262) z) t) z) a) z) b))
   (+
    (* (+ (* (+ (* (+ z 15.234687407) z) 31.4690115749) z) 11.9400905721) z)
    0.607771387771))))
(FPCore
 (x)
 :name
 "Numeric.SpecFunctions:$slogFactorial from math-functions-0.1.5.2, A"
 :target
 (/ (/ 1.0 x) x)
 (/ 1.0 (* x x)))
(FPCore
 (x y z)
 :name
 "Numeric.SpecFunctions:$slogFactorial from math-functions-0.1.5.2, B"
 :target
 (+
  (+ (+ (* (- x 0.5) (log x)) (- 0.91893853320467 x)) (/ 0.083333333333333 x))
  (* (/ z x) (- (* z (+ y 0.0007936500793651)) 0.0027777777777778)))
 (+
  (+ (- (* (- x 0.5) (log x)) x) 0.91893853320467)
  (/
   (+
    (* (- (* (+ y 0.0007936500793651) z) 0.0027777777777778) z)
    0.083333333333333)
   x)))
(FPCore
 (x y z t a)
 :name
 "Numeric.SpecFunctions:logGammaL from math-functions-0.1.5.2"
 :target
 (+ (log (+ x y)) (+ (- (log z) t) (* (- a 0.5) (log t))))
 (+ (- (+ (log (+ x y)) (log z)) t) (* (- a 0.5) (log t))))
(FPCore
 (x)
 :name
 "Numeric.SpecFunctions:logGammaCorrection from math-functions-0.1.5.2"
 :target
 (- (* (sqr x) 2.0) 1.0)
 (- (* (* x x) 2.0) 1.0))
(FPCore
 (x y)
 :name
 "Numeric.SpecFunctions:log1p from math-functions-0.1.5.2, A"
 :target
 (* x (- 1.0 (* x y)))
 (* x (- 1.0 (* x y))))
(FPCore
 (x)
 :name
 "Numeric.SpecFunctions:log1p from math-functions-0.1.5.2, B"
 :target
 (* x (- 1.0 (* x 0.5)))
 (* x (- 1.0 (* x 0.5))))
(FPCore
 (x y z t a b)
 :name
 "Numeric.SpecFunctions:logBeta from math-functions-0.1.5.2, A"
 :target
 (+ (+ (+ x y) (/ (* (- 1 (sqr (log t))) z) (+ 1 (log t)))) (* (- a 0.5) b))
 (+ (- (+ (+ x y) z) (* z (log t))) (* (- a 0.5) b)))
(FPCore
 (x y z t a b c i)
 :name
 "Numeric.SpecFunctions:logBeta from math-functions-0.1.5.2, B"
 :target
 (+ (+ (+ (+ (+ (* x (log y)) z) t) a) (* (- b 0.5) (log c))) (* y i))
 (+ (+ (+ (+ (+ (* x (log y)) z) t) a) (* (- b 0.5) (log c))) (* y i)))
(FPCore
 (x y z)
 :name
 "Numeric.SpecFunctions:choose from math-functions-0.1.5.2"
 :target
 (/ x (/ z (+ y z)))
 (/ (* x (+ y z)) z))
(FPCore
 (x y z)
 :name
 "Numeric.SpecFunctions:stirlingError from math-functions-0.1.5.2"
 :target
 (- (- (+ y x) z) (* (+ y 0.5) (log y)))
 (- (+ (- x (* (+ y 0.5) (log y))) y) z))
(FPCore
 (x y z t)
 :name
 "Numeric.SpecFunctions:incompleteGamma from math-functions-0.1.5.2, A"
 :target
 (+ (- (- (* x (log y)) y) z) (log t))
 (+ (- (- (* x (log y)) y) z) (log t)))
(FPCore
 (x y)
 :name
 "Numeric.SpecFunctions:incompleteGamma from math-functions-0.1.5.2, B"
 :target
 (* 3.0 (+ (* y (sqrt x)) (* (- (/ 1.0 (* x 9.0)) 1.0) (sqrt x))))
 (* (* 3.0 (sqrt x)) (- (+ y (/ 1.0 (* x 9.0))) 1.0)))
(FPCore
 (x)
 :name
 "Numeric.SpecFunctions:invIncompleteGamma from math-functions-0.1.5.2, A"
 :target
 (- 1.0 (* x (+ 0.253 (* x 0.12))))
 (- 1.0 (* x (+ 0.253 (* x 0.12)))))
(FPCore
 (x y)
 :name
 "Numeric.SpecFunctions:invIncompleteGamma from math-functions-0.1.5.2, B"
 :target
 (if (< y -81284752.61947241)
   (- 1.0 (log (- (/ x (sqr y)) (- (/ 1.0 y) (/ x y)))))
   (if (< y 3.0094271212461764e+25)
     (log (/ (exp 1.0) (- 1.0 (/ (- x y) (- 1.0 y)))))
     (- 1.0 (log (- (/ x (sqr y)) (- (/ 1.0 y) (/ x y)))))))
 (- 1.0 (log (- 1.0 (/ (- x y) (- 1.0 y))))))
(FPCore
 (x)
 :name
 "Numeric.SpecFunctions:invIncompleteGamma from math-functions-0.1.5.2, C"
 :target
 (- (/ (+ 2.30753 (* x 0.27061)) (+ 1.0 (* x (+ 0.99229 (* x 0.04481))))) x)
 (- (/ (+ 2.30753 (* x 0.27061)) (+ 1.0 (* x (+ 0.99229 (* x 0.04481))))) x))
(FPCore
 (x y)
 :name
 "Numeric.SpecFunctions:invIncompleteGamma from math-functions-0.1.5.2, D"
 :target
 (- (- 1.0 (/ (/ 1.0 x) 9.0)) (/ y (* 3.0 (sqrt x))))
 (- (- 1.0 (/ 1.0 (* x 9.0))) (/ y (* 3.0 (sqrt x)))))
(FPCore
 (x y)
 :name
 "Numeric.SpecFunctions:incompleteBetaApprox from math-functions-0.1.5.2, A"
 :target
 (/ (/ (/ x (+ (+ y 1) x)) (+ y x)) (/ 1 (/ y (+ y x))))
 (/ (* x y) (* (* (+ x y) (+ x y)) (+ (+ x y) 1.0))))
(FPCore
 (x y z t a b)
 :name
 "Numeric.SpecFunctions:incompleteBetaApprox from math-functions-0.1.5.2, B"
 :target
 (* x (exp (+ (* y (- (log z) t)) (* a (- (log (- 1.0 z)) b)))))
 (* x (exp (+ (* y (- (log z) t)) (* a (- (log (- 1.0 z)) b))))))
(FPCore
 (x y z t a b)
 :name
 "Numeric.SpecFunctions:incompleteBetaWorker from math-functions-0.1.5.2, A"
 :target
 (if (< t -0.8845848504127471)
   (/ (* x (/ (pow a (- t 1.0)) y)) (- (+ b 1) (* y (log z))))
   (if (< t 852031.2288374073)
     (/ (* (/ x y) (pow a (- t 1.0))) (exp (- b (* (log z) y))))
     (/ (* x (/ (pow a (- t 1.0)) y)) (- (+ b 1) (* y (log z))))))
 (/ (* x (exp (- (+ (* y (log z)) (* (- t 1.0) (log a))) b))) y))
(FPCore
 (x y z t)
 :name
 "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, B"
 :target
 (-
  (*
   (- z)
   (+ (+ (* 0.5 (sqr y)) y) (* (/ 1/3 (* 1.0 (* 1.0 1.0))) (* y (* y y)))))
  (- t (* x (log y))))
 (- (+ (* x (log y)) (* z (log (- 1.0 y)))) t))
(FPCore
 (x y z t)
 :name
 "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, C"
 :target
 (if (< (* x (- (/ y z) (/ t (- 1.0 z)))) -7.623226303312042e-196)
   (* x (- (/ y z) (* t (/ 1 (- 1.0 z)))))
   (if (< (* x (- (/ y z) (/ t (- 1.0 z)))) 1.4133944927702302e-211)
     (+ (/ (* y x) z) (- (/ (* t x) (- 1.0 z))))
     (* x (- (/ y z) (* t (/ 1 (- 1.0 z)))))))
 (* x (- (/ y z) (/ t (- 1.0 z)))))
(FPCore
 (x)
 :name
 "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, D"
 :target
 (- x (/ (+ 2.30753 (* x 0.27061)) (+ 1.0 (* (+ 0.99229 (* x 0.04481)) x))))
 (- x (/ (+ 2.30753 (* x 0.27061)) (+ 1.0 (* (+ 0.99229 (* x 0.04481)) x)))))
(FPCore
 (x y)
 :name
 "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, E"
 :target
 (+ (- 1.0 x) (* y (sqrt x)))
 (+ (- 1.0 x) (* y (sqrt x))))
(FPCore
 (x y)
 :name
 "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, F"
 :target
 (if (< y -3.7311844206647956e+94)
   (/ (exp (/ -1 y)) x)
   (if (< y 2.817959242728288e+37)
     (/ (pow (/ x (+ y x)) x) x)
     (if (< y 2.347387415166998e+178)
       (log (exp (/ (pow (/ x (+ y x)) x) x)))
       (/ (exp (/ -1 y)) x))))
 (/ (exp (* x (log (/ x (+ x y))))) x))
(FPCore
 (x y z)
 :name
 "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, G"
 :target
 (if (< (/ y (+ z y)) 7.1154157597908e-315)
   (+ x (/ (exp (/ -1 z)) y))
   (+ x (/ (exp (log (pow (/ y (+ y z)) y))) y)))
 (+ x (/ (exp (* y (log (/ y (+ z y))))) y)))
(FPCore
 (x)
 :name
 "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, H"
 :target
 (/ (- (sqr x) 3.0) 6.0)
 (/ (- (* x x) 3.0) 6.0))
(FPCore
 (x y z t a b c)
 :name
 "Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, I"
 :target
 (if (< t -2.118326644891581e-50)
   (/
    x
    (+ x (* y (exp (* 2.0 (- (+ (* a c) (* 0.8333333333333334 c)) (* a b)))))))
   (if (< t 5.196588770651547e-123)
     (/
      x
      (+
       x
       (*
        y
        (exp
         (*
          2.0
          (/
           (-
            (* (* z (sqrt (+ t a))) (* (* 3.0 t) (- a (/ 5.0 6.0))))
            (*
             (- (* (+ (/ 5.0 6.0) a) (* 3.0 t)) 2.0)
             (* (- a (/ 5.0 6.0)) (* (- b c) t))))
           (* (* (sqr t) 3.0) (- a (/ 5.0 6.0)))))))))
     (/
      x
      (+
       x
       (*
        y
        (exp
         (*
          2.0
          (-
           (/ (* z (sqrt (+ t a))) t)
           (* (- b c) (- (+ a (/ 5.0 6.0)) (/ 2.0 (* t 3.0))))))))))))
 (/
  x
  (+
   x
   (*
    y
    (exp
     (*
      2.0
      (-
       (/ (* z (sqrt (+ t a))) t)
       (* (- b c) (- (+ a (/ 5.0 6.0)) (/ 2.0 (* t 3.0)))))))))))
(FPCore
 (x y z)
 :name
 "Numeric.SpecFunctions.Extra:bd0 from math-functions-0.1.5.2"
 :target
 (if (< y 7.595077799083773e-308)
   (- (* x (log (/ x y))) z)
   (- (* x (- (log x) (log y))) z))
 (- (* x (log (/ x y))) z))
(FPCore
 (x y z t a b c i)
 :name
 "Numeric.SpecFunctions:logGamma from math-functions-0.1.5.2"
 :target
 (/
  (+ (* (+ (* (+ (* (+ (* x y) z) y) 27464.7644705) y) 230661.510616) y) t)
  (+ (* (+ (* (+ (* (+ y a) y) b) y) c) y) i))
 (/
  (+ (* (+ (* (+ (* (+ (* x y) z) y) 27464.7644705) y) 230661.510616) y) t)
  (+ (* (+ (* (+ (* (+ y a) y) b) y) c) y) i)))
(FPCore
 (x y z t a)
 :name
 "Statistics.Math.RootFinding:ridders from math-functions-0.1.5.2"
 :target
 (if (< z -3.1921305903852764e+46)
   (- (* y x))
   (if (< z 5.976268120920894e+90)
     (/ (* x z) (/ (sqrt (- (sqr z) (* a t))) y))
     (* y x)))
 (/ (* (* x y) z) (sqrt (- (* z z) (* t a)))))
(FPCore
 (x y z)
 :name
 "Statistics.Distribution.Poisson.Internal:probability from math-functions-0.1.5.2"
 :target
 (exp (+ (- x z) (* (log y) y)))
 (exp (- (+ x (* y (log y))) z)))
(FPCore
 (x)
 :name
 "Statistics.Distribution.Binomial:directEntropy from math-functions-0.1.5.2"
 :target
 (* x (log x))
 (* x (log x)))
(FPCore
 (x)
 :name
 "Statistics.Correlation.Kendall:numOfTiesBy from math-functions-0.1.5.2"
 :target
 (- (sqr x) x)
 (* x (- x 1.0)))
(FPCore
 (x y z)
 :name
 "Statistics.Sample:robustSumVarWeighted from math-functions-0.1.5.2"
 :target
 (+ x (* (* y z) z))
 (+ x (* (* y z) z)))
(FPCore
 (x y z)
 :name
 "Statistics.Sample:$swelfordMean from math-functions-0.1.5.2"
 :target
 (+ x (/ (- y x) z))
 (+ x (/ (- y x) z)))
(FPCore
 (x y)
 :name
 "Statistics.Sample:$skurtosis from math-functions-0.1.5.2"
 :target
 (- (/ (/ x y) y) 3.0)
 (- (/ x (* y y)) 3.0))
(FPCore
 (x y z t a b)
 :name
 "Statistics.Distribution.Beta:$centropy from math-functions-0.1.5.2"
 :target
 (+ (- (- x (* (- y 1.0) z)) (* (- t 1.0) a)) (* (- (+ y t) 2.0) b))
 (+ (- (- x (* (- y 1.0) z)) (* (- t 1.0) a)) (* (- (+ y t) 2.0) b)))
(FPCore
 (x y z)
 :name
 "Statistics.Distribution.Beta:$cvariance from math-functions-0.1.5.2"
 :target
 (if (< z 249.6182814532307)
   (/ (* y (/ x z)) (+ z (sqr z)))
   (/ (* (/ (/ y z) (+ 1 z)) x) z))
 (/ (* x y) (* (* z z) (+ z 1.0))))
(FPCore
 (x y z t)
 :name
 "Statistics.Distribution.Beta:$cdensity from math-functions-0.1.5.2"
 :target
 (- (+ (* (- x 1.0) (log y)) (* (- z 1.0) (log (- 1.0 y)))) t)
 (- (+ (* (- x 1.0) (log y)) (* (- z 1.0) (log (- 1.0 y)))) t))
(FPCore
 (x y)
 :name
 "Statistics.Distribution.Binomial:$cvariance from math-functions-0.1.5.2"
 :target
 (* (* x y) (- 1.0 y))
 (* (* x y) (- 1.0 y)))
(FPCore
 (x y z)
 :name
 "Statistics.Distribution.Poisson:$clogProbability from math-functions-0.1.5.2"
 :target
 (- (- (* x (log y)) z) y)
 (- (- (* x (log y)) z) y))
(FPCore
 (x y z)
 :name
 "Statistics.Distribution.CauchyLorentz:$cdensity from math-functions-0.1.5.2"
 :target
 (if (< (* y (+ 1.0 (* z z))) -inf.0)
   (/ (/ 1.0 y) (* (+ 1.0 (sqr z)) x))
   (if (< (* y (+ 1.0 (* z z))) 8.680743250567252e+305)
     (/ (/ 1.0 x) (* (+ 1.0 (sqr z)) y))
     (/ (/ 1.0 y) (* (+ 1.0 (sqr z)) x))))
 (/ (/ 1.0 x) (* y (+ 1.0 (* z z)))))
(FPCore
 (x y)
 :name
 "Examples.Basics.BasicTests:f3 from sbv-4.4"
 :target
 (+ (sqr x) (+ (sqr y) (* 2 (* y x))))
 (* (+ x y) (+ x y)))
(FPCore
 (x y)
 :name
 "Examples.Basics.BasicTests:f2 from sbv-4.4"
 :target
 (- (sqr x) (sqr y))
 (- (* x x) (* y y)))
(FPCore
 (x y)
 :name
 "Examples.Basics.BasicTests:f1 from sbv-4.4"
 :target
 (* (+ x y) (- x y))
 (* (+ x y) (- x y)))
(FPCore
 (x y)
 :name
 "Examples.Basics.ProofTests:f4 from sbv-4.4"
 :target
 (+ (sqr x) (+ (sqr y) (* (* x y) 2.0)))
 (+ (+ (* x x) (* (* x 2.0) y)) (* y y)))
(FPCore
 (x y)
 :name
 "Numeric.LinearAlgebra.Util:formatSparse from hmatrix-0.16.1.5"
 :target
 (/ (fabs (- x y)) (fabs y))
 (/ (fabs (- x y)) (fabs y)))
(FPCore
 (x y)
 :name
 "Data.Random.Distribution.Normal:normalF from random-fu-0.2.6.2"
 :target
 (exp (* (* x y) y))
 (exp (* (* x y) y)))
(FPCore
 (x y)
 :name
 "Data.Random.Distribution.Normal:normalTail from random-fu-0.2.6.2"
 :target
 (+ (+ y y) (sqr x))
 (+ (+ (* x x) y) y))
(FPCore
 (x)
 :name
 "Data.Random.Distribution.Normal:doubleStdNormalZ from random-fu-0.2.6.2"
 :target
 (- (+ x x) 1.0)
 (- (+ x x) 1.0))
(FPCore
 (x y)
 :name
 "Data.Random.Distribution.T:$ccdf from random-fu-0.2.6.2"
 :target
 (+ (* 1/2 (/ x y)) 1/2)
 (/ (+ x y) (+ y y)))
(FPCore
 (x y z t)
 :name
 "Data.Random.Distribution.Triangular:triangularCDF from random-fu-0.2.6.2, A"
 :target
 (- 1.0 (/ x (* (- y z) (- y t))))
 (- 1.0 (/ x (* (- y z) (- y t)))))
(FPCore
 (x y z t)
 :name
 "Data.Random.Distribution.Triangular:triangularCDF from random-fu-0.2.6.2, B"
 :target
 (if (< (/ x (* (- y z) (- t z))) 0.0)
   (/ (/ x (- y z)) (- t z))
   (* x (/ 1 (* (- y z) (- t z)))))
 (/ x (* (- y z) (- t z))))
(FPCore
 (x)
 :name
 "Data.Random.Dice:roll from dice-0.1"
 :target
 (- (sqr x) 1.0)
 (- (* x x) 1.0))
(FPCore
 (x)
 :name
 "Prelude:atanh from fay-base-0.20.0.1"
 :target
 (/ (+ x 1.0) (- 1.0 x))
 (/ (+ x 1.0) (- 1.0 x)))
(FPCore
 (x)
 :name
 "ReportTypes:explainFloat from gipeda-0.1.2.1"
 :target
 0
 (* 100.0 (/ (- x x) x)))
(FPCore
 (x y z t a)
 :name
 "Hakyll.Web.Tags:renderTagCloud from hakyll-4.7.2.3"
 :target
 (+ x (* (/ (- y z) (- (+ t 1.0) z)) (- a x)))
 (+ x (* (/ (- y z) (- (+ t 1.0) z)) (- a x))))
(FPCore
 (x y z)
 :name
 "Data.Histogram.Bin.BinF:$cfromIndex from histogram-fill-0.8.4.1"
 :target
 (+ (+ (/ x 2.0) (* y x)) z)
 (+ (+ (/ x 2.0) (* y x)) z))
(FPCore
 (x y)
 :name
 "Data.Histogram.Bin.LogBinD:$cbinSizeN from histogram-fill-0.8.4.1"
 :target
 (- (* x y) x)
 (- (* x y) x))
(FPCore
 (x y z t a)
 :name
 "Numeric.Signal:interpolate   from hsignal-0.2.7.1"
 :target
 (+ x (* (- y z) (/ (- t x) (- a z))))
 (+ x (* (- y z) (/ (- t x) (- a z)))))
(FPCore
 (x y z t)
 :name
 "Numeric.Signal.Multichannel:$cget from hsignal-0.2.7.1"
 :target
 (if (< z 2.759456554562692e-282)
   (+ (* (/ x y) (- z t)) t)
   (if (< z 2.326994450874436e-110)
     (+ (* x (/ (- z t) y)) t)
     (+ (* (/ x y) (- z t)) t)))
 (+ (* (/ x y) (- z t)) t))
(FPCore
 (x y z t)
 :name
 "Numeric.Signal.Multichannel:$cput from hsignal-0.2.7.1"
 :target
 (/ t (/ (- z y) (- x y)))
 (* (/ (- x y) (- z y)) t))
(FPCore
 (x y)
 :name
 "Data.HyperLogLog.Config:hll from hyperloglog-0.3.4"
 :target
 (* (* x y) y)
 (* (* x y) y))
(FPCore
 (x y)
 :name
 "Data.HyperLogLog.Type:size from hyperloglog-0.3.4, A"
 :target
 (* x (log (- 1.0 (/ y x))))
 (* (* x 1.0) (log (- 1.0 (/ y x)))))
(FPCore
 (x y)
 :name
 "Data.HyperLogLog.Type:size from hyperloglog-0.3.4, B"
 :target
 (if (< y 1.2973149052617803e-303)
   (* x (log (/ x y)))
   (/ x (/ 1 (- (log x) (log y)))))
 (* x (log (/ x y))))
(FPCore
 (x y z)
 :name
 "Diagrams.Backend.Cairo.Internal:setTexture from diagrams-cairo-1.3.0.3"
 :target
 (if (< z -2.060202331921739e+104)
   (- x (/ (* z x) y))
   (if (< z 1.6939766013828526e+213) (/ x (/ y (- y z))) (* (- y z) (/ x y))))
 (/ (* x (- y z)) y))
(FPCore
 (x y)
 :name
 "Numeric.Integration.TanhSinh:simpson  from integration-0.2.1"
 :target
 (* x (+ y y))
 (* x (+ y y)))
(FPCore
 (x y)
 :name
 "Numeric.Integration.TanhSinh:everywhere from integration-0.2.1"
 :target
 (+ x (* (* x y) y))
 (* x (+ 1.0 (* y y))))
(FPCore
 (x y z t)
 :name
 "Data.Metrics.Snapshot:quantile from metrics-0.3.0.2"
 :target
 (+ x (+ (* t (- y z)) (* (- x) (- y z))))
 (+ x (* (- y z) (- t x))))
(FPCore
 (x y)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Legend:renderLegendOutside from plot-0.2.3.4, A"
 :target
 (+ y (* 2 x))
 (+ (+ x y) x))
(FPCore
 (x y z t)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Legend:renderLegendOutside from plot-0.2.3.4, B"
 :target
 (+ (* x (+ (+ (+ (+ y z) z) y) t)) (* y 5.0))
 (+ (* x (+ (+ (+ (+ y z) z) y) t)) (* y 5.0)))
(FPCore
 (x y z)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Legend:renderLegendInside from plot-0.2.3.4"
 :target
 (+ (+ (+ (+ (+ x y) y) x) z) x)
 (+ (+ (+ (+ (+ x y) y) x) z) x))
(FPCore
 (x y z)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Legend:renderLegendOutside from plot-0.2.3.4, C"
 :target
 (+ (* (+ x 5.0) z) (* x y))
 (+ (* x (+ y z)) (* z 5.0)))
(FPCore
 (x y z t)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Axis:tickPosition from plot-0.2.3.4"
 :target
 (if (< (* (- y x) (/ z t)) -1013646692435.8867)
   (+ x (/ (- y x) (/ t z)))
   (if (< (* (- y x) (/ z t)) -0.0)
     (+ x (/ (* (- y x) z) t))
     (+ x (/ (- y x) (/ t z)))))
 (+ x (* (- y x) (/ z t))))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Axis:renderAxisLine from plot-0.2.3.4, A"
 :target
 (+ x (/ y (/ (- z a) (- z t))))
 (+ x (* y (/ (- z t) (- z a)))))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Axis:renderAxisLine from plot-0.2.3.4, B"
 :target
 (if (< y -8.508084860551241e-17)
   (+ x (* y (/ (- z t) (- a t))))
   (if (< y 2.894426862792089e-49)
     (+ x (* (* y (- z t)) (/ 1 (- a t))))
     (+ x (* y (/ (- z t) (- a t))))))
 (+ x (* y (/ (- z t) (- a t)))))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Axis:renderAxisTick from plot-0.2.3.4, A"
 :target
 (if (< t -1.0682974490174067e-39)
   (+ x (* (/ (- y z) (- a z)) t))
   (if (< t 3.9110949887586375e-141)
     (+ x (/ (* (- y z) t) (- a z)))
     (+ x (* (/ (- y z) (- a z)) t))))
 (+ x (/ (* (- y z) t) (- a z))))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Axis:renderAxisTick from plot-0.2.3.4, B"
 :target
 (if (< (- (+ x y) (/ (* (- z t) y) (- a t))) -1.3664970889390727e-07)
   (- (+ y x) (* (* (- z t) (/ 1 (- a t))) y))
   (if (< (- (+ x y) (/ (* (- z t) y) (- a t))) 1.4754293444577233e-239)
     (/ (- (* y (- a z)) (* x t)) (- a t))
     (- (+ y x) (* (* (- z t) (/ 1 (- a t))) y))))
 (- (+ x y) (/ (* (- z t) y) (- a t))))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Axis:renderAxisTicks from plot-0.2.3.4, A"
 :target
 (+ x (/ y (/ (- z a) (- z t))))
 (+ x (/ (* y (- z t)) (- z a))))
(FPCore
 (x y z t a)
 :name
 "Graphics.Rendering.Plot.Render.Plot.Axis:renderAxisTicks from plot-0.2.3.4, B"
 :target
 (+ x (/ y (/ (- a t) (- z t))))
 (+ x (/ (* y (- z t)) (- a t))))
(FPCore
 (x y z)
 :name
 "Data.Array.Repa.Algorithms.Pixel:doubleRmsOfRGB8 from repa-algorithms-3.4.0.1"
 :target
 (if (< z -6.396479394109776e+136)
   (/ (- z) (sqrt 3.0))
   (if (< z 7.320293694404182e+117)
     (/ (sqrt (+ (+ (sqr z) (sqr x)) (sqr y))) (sqrt 3.0))
     (* (sqrt 0.3333333333333333) z)))
 (sqrt (/ (+ (+ (* x x) (* y y)) (* z z)) 3.0)))
(FPCore
 (x y z)
 :name
 "Data.Array.Repa.Algorithms.ColorRamp:rampColorHotToCold from repa-algorithms-3.4.0.1, A"
 :target
 (+ 1.0 (/ (* 4.0 (- (+ x (* y 0.75)) z)) y))
 (+ 1.0 (/ (* 4.0 (- (+ x (* y 0.75)) z)) y)))
(FPCore
 (x y z)
 :name
 "Data.Array.Repa.Algorithms.ColorRamp:rampColorHotToCold from repa-algorithms-3.4.0.1, B"
 :target
 (- (* 4.0 (/ x z)) (+ 2.0 (* 4.0 (/ y z))))
 (/ (* 4.0 (- (- x y) (* z 0.5))) z))
(FPCore
 (x y z)
 :name
 "Data.Array.Repa.Algorithms.ColorRamp:rampColorHotToCold from repa-algorithms-3.4.0.1, C"
 :target
 (+ 1.0 (/ (* 4.0 (- (+ x (* y 0.25)) z)) y))
 (+ 1.0 (/ (* 4.0 (- (+ x (* y 0.25)) z)) y)))
(FPCore
 (x)
 :name
 "Data.Spline.Key:interpolateKeys from smoothie-0.4.0.2"
 :target
 (* x (* x (- 3.0 (* x 2.0))))
 (* (* x x) (- 3.0 (* x 2.0))))
(FPCore
 (x y z)
 :name
 "FRP.Yampa.Vector3:vector3Rho from Yampa-0.10.2"
 :target
 (if (< z -6.396479394109776e+136)
   (- z)
   (if (< z 7.320293694404182e+117) (sqrt (+ (+ (sqr z) (sqr x)) (sqr y))) z))
 (sqrt (+ (+ (* x x) (* y y)) (* z z))))
(FPCore
 (x y z t)
 :name
 "SynthBasics:moogVCF from YampaSynth-0.2"
 :target
 (+ x (* y (* z (- (tanh (/ t y)) (tanh (/ x y))))))
 (+ x (* (* y z) (- (tanh (/ t y)) (tanh (/ x y))))))
