
(herbie-test ([a double] [delta (positive double)] [N 100])
  "Integrate (f x) = x"
  (do ([sum 0.0 (let* ([width (/ delta N)]
                       [x1 (+ a (* (- i 1) width))]
                       [x2 (+ a (* i width))]
                       [fx1 x1]
                       [fx2 x2])
                  (+ sum (/ (+ fx1 fx2) 2)))]
       [i 1.0 (+ i 1)])
      (<= i N)
    (* sum (/ delta N))))

(herbie-test ([a double] [delta (positive double)] [N 100])
  "Integrate (f x) = (exp x)"
  (do ([sum 0.0 (let* ([width (/ delta N)]
                       [x1 (+ a (* (- i 1) width))]
                       [x2 (+ a (* i width))]
                       [fx1 (exp x1)]
                       [fx2 (exp x2)])
                  (+ sum (/ (+ fx1 fx2) 2)))]
       [i 1.0 (+ i 1)])
      (<= i N)
    (* sum (/ delta N))))

(herbie-test ([a double] [delta (positive double)] [N 100])
  "Integrate (f x) = (/ 1 x)"
  (do ([sum 0.0 (let* ([width (/ delta N)]
                       [x1 (+ a (* (- i 1) width))]
                       [x2 (+ a (* i width))]
                       [fx1 (/ 1 x1)]
                       [fx2 (/ 1 x2)])
                  (+ sum (/ (+ fx1 fx2) 2)))]
       [i 1.0 (+ i 1)])
      (<= i N)
    (* sum (/ delta N))))
