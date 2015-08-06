
; Heat equation D[u, t] = k D[u, {x, 2}]
; First-order (1 -2 1) stencil for the spacial derivative
; First-order euler method
; Discretize space into 5 points
(herbie-test (x00 x10 x20 x30 x40 eps k [N 100])
             "Solve the heat equation on five points"
             (do ([t 0 (+ t 1)]
                  [x0 x00 (let ([df (+ x1 (* -2 x0) x1)]) (+ x0 (* eps k df)))]
                  [x1 x10 (let ([df (+ x0 (* -2 x1) x2)]) (+ x1 (* eps k df)))]
                  [x2 x20 (let ([df (+ x1 (* -2 x2) x3)]) (+ x2 (* eps k df)))]
                  [x3 x30 (let ([df (+ x2 (* -2 x3) x4)]) (+ x3 (* eps k df)))]
                  [x4 x40 (let ([df (+ x3 (* -2 x4) x3)]) (+ x4 (* eps k df)))])
                 (<= t N)
               x2))

; Wave equation D[u, {t, 2}] = c^2 D[u, {x, 2}]
; First-order (1 -2 1) stencil for the spacial derivative
; First-order euler method
; Discretize space into 5 points
(herbie-test (x00 x10 x20 x30 x40 v00 v10 v20 v30 v40 eps c [N 100])
             "Solve the wave equation on five points"
             (do ([t 0 (+ t 1)]

                  [v0 v00 (let ([df (+ x1 (* -2 x0) x1))]) (+ v0 (* eps c df)))]
                  [v1 v10 (let ([df (+ x0 (* -2 x1) x2))]) (+ v1 (* eps c df)))]
                  [v2 v20 (let ([df (+ x1 (* -2 x2) x3))]) (+ v2 (* eps c df)))]
                  [v3 v30 (let ([df (+ x2 (* -2 x3) x4))]) (+ v3 (* eps c df)))]
                  [v4 v40 (let ([df (+ x3 (* -2 x4) x3))]) (+ v4 (* eps c df)))]

                  [x0 x00 (+ x0 (* eps c v0))]
                  [x1 x10 (+ x1 (* eps c v1))]
                  [x2 x20 (+ x2 (* eps c v2))]
                  [x3 x30 (+ x3 (* eps c v3))]
                  [x4 x40 (+ x4 (* eps c v4))])
                 (<= t N)
               x2))

; Euler-Tricomi equation D[u, {x, 2}] = x D[u, {y, 2}]
; First-order (1 -2 1) stencil for the y derivative
; First-order euler method
; Discretize y into five points
(herbie-test (y00 y10 y20 y30 y40 v00 v10 v20 v30 v40 eps [N 100])
             "Solve the Euler-Tricomi equation on five points"
             (do ([x 0 (+ x 1)]

                  [v0 v00 (let ([dy (+ y1 (* -2 y0) y1))]) (+ v0 (* eps x dy)))]
                  [v1 v10 (let ([dy (+ y0 (* -2 y1) y2))]) (+ v1 (* eps x dy)))]
                  [v2 v20 (let ([dy (+ y1 (* -2 y2) y3))]) (+ v2 (* eps x dy)))]
                  [v3 v30 (let ([dy (+ y2 (* -2 y3) y4))]) (+ v3 (* eps x dy)))]
                  [v4 v40 (let ([dy (+ y3 (* -2 y4) y3))]) (+ v4 (* eps x dy)))]

                  [y0 y00 (+ y0 (* eps v0))]
                  [y1 y10 (+ y1 (* eps v1))]
                  [y2 y20 (+ y2 (* eps v2))]
                  [y3 y30 (+ y3 (* eps v3))]
                  [y4 y40 (+ y4 (* eps v4))])
                 (<= x N)
               y2))

; Dym equation D[u, t] = u^3 D[u, {x, 3}]
; First-order (1 -2 1) stencil for the x derivative
; First-order euler method
; Discretize x into five points
(herbie-test (x00 x10 x20 x30 x40 eps [N 100])
             "Solve the Dym equation on five points"
             (do ([t 0 (+ t 1)]

                  [x0 x00 (let ([df (+ x1 (* -2 x0) x1)]) (+ x0 (* eps (expt x0 3) df)))]
                  [x1 x10 (let ([df (+ x0 (* -2 x1) x2)]) (+ x1 (* eps (expt x1 3) df)))]
                  [x2 x20 (let ([df (+ x1 (* -2 x2) x3)]) (+ x2 (* eps (expt x2 3) df)))]
                  [x3 x30 (let ([df (+ x2 (* -2 x3) x4)]) (+ x3 (* eps (expt x3 3) df)))]
                  [x4 x40 (let ([df (+ x3 (* -2 x4) x3)]) (+ x4 (* eps (expt x4 3) df)))])
                 (<= t N)
               x2))
