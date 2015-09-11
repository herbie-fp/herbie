
(herbie-test (x0 y0 width [N 100])
  "Pixel transformation for Arnolds Catmap, in continous space"
  (do ([x x0 (mod (+ x y) width)]
       [y y0 (mod (+ x (* 2 y)) width)]
       [i 0 (+ i 1)])
      (< i N)
    x))

(herbie-test ([x0 (< (< 0 double) 1)] [y0 (< (< 0 double) 1)] [N 100])
  "Pixel transformation for Baker's Map, in continous space"
  (do ([x x0 (if (< x .5) (* 2 x) (- 2 (* 2 x)))]
       [y y0 (if (< x .5) (/ y 2) (- 1 (/ y 2)))]
       [i 0 (+ i 1)])
      (< i N)
    x))

(herbie-test ([theta0 (< (< 0 double) 1)] [p0 (< (< 0 double) 1)]
              [eps (< (< 0 double) 2)] [ohm (< (< 0 double) 1)]
              [N 100])
  "Angle transformation for Chirikov standard map"
  (do ([theta theta0 (+ theta p (* eps (sin (* 2 pi theta))))]
       [p p0 (+ p (* eps (sin (* 2 pi theta))))]
       [i 0 (+ i 1)])
      (< i N)
    theta))
