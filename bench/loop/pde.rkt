

(herbie-test (x00 x10 x20 x30 x40 eps k (N (integer-range 0 100)))
             "Solve the wave equation on five points"
             (do ([t 0 (+ t 1)]
                  [x0 x00 (let ([df (+ x1 (* -2 x0) x1)]) (+ x0 (* eps k df)))]
                  [x1 x10 (let ([df (+ x0 (* -2 x1) x2)]) (+ x1 (* eps k df)))]
                  [x2 x20 (let ([df (+ x1 (* -2 x2) x3)]) (+ x2 (* eps k df)))]
                  [x3 x30 (let ([df (+ x2 (* -2 x3) x4)]) (+ x3 (* eps k df)))]
                  [x4 x40 (let ([df (+ x3 (* -2 x4) x3)]) (+ x4 (* eps k df)))])
                 (<= t N)
               x2))
