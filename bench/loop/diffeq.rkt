
(herbie-test ([x0 double] [y0 double] [d double] [N 100])
  "Solve diff eq for (f x) = (- y)"
  (do ([x x0 (let ([m (- y)])
               (+ x (/ d (sqrt (+ (sqr m) 1)))))]
       [y y0 (let* ([m (- y)]
                    [dx (/ d (sqrt (+ (sqr m) 1)))])
               (+ y (* dx m)))]
       [i 1.0 (+ i 1)])
      (<= i N)
    y))

(herbie-test ([x0 double] [y0 double] [d double] [N 100])
  "Solve diff eq for (f x) = (/ 1 x)"
  (do ([x x0 (let ([m (/ 1 x)])
               (+ x (/ d (sqrt (+ (sqr m) 1)))))]
       [y y0 (let* ([m (/ 1 x)]
                    [dx (/ d (sqrt (+ (sqr m) 1)))])
               (+ y (* dx m)))]
       [i 1.0 (+ i 1)])
      (<= i N)
    y))

(herbie-test ([x0 double] [y0 double] [d double] [N 100])
  "Solve diff eq for (f x) = (- x y)"
  (do ([x x0 (let ([m (- x y)])
               (+ x (/ d (sqrt (+ (sqr m) 1)))))]
       [y y0 (let* ([m (- x y)]
                    [dx (/ d (sqrt (+ (sqr m) 1)))])
               (+ y (* dx m)))]
       [i 1.0 (+ i 1)])
      (<= i N)
    y))
