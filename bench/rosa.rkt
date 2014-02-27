#lang racket
(require casio/test)

; All test cases come from the EPFL Rosa project, available at
;
; https://github.com/malyzajko/rosa
;
; This is also the subject of the POPL'14 paper
;
; E. Darulova, V. Kuncak, "Sound Compilation of Reals", POPL'14

(casio-bench (u v t)
  "Test from DopplerBench.scala from Rosa"
  (/ (* (- t1) v) (* (+ t1 u) (+ t1 u))))

(casio-bench (x)
  "Test from Benchmark.scala from Rosa"
  (- (* 0.954929658551372 x) (* 0.12900613773279798 (* (* x x) x))))

(casio-bench (x1 x2)
  "Test from FloatVsDoubleBenchmark.scala from Rosa"
  (let* ([t (- (+ (* 3 x1 x1) (* 2 x2)) x1)]
         [t* (- (- (* 3 x1 x1) (* 2 x2)) x1)]
         [d (+ (* x1 x1) 1)]
         [s (/ t d)]
         [s* (/ t* d)])

    (+ x1
       (+
        (* (+ (* 2 x1 s (- s 3)) (* x1 x1 (- (* 4 s) 6))) d)
        (* 3 x1 x1 s)
        (* x1 x1 x1)
        x1
        (* 3 s*)))))

(casio-bench (v w r)
  "Test from TurbineBenchmark.scala from Rosa"
  (- (+ 3 (/ 2 (* r r))) (/ (* 0.125 (- 3 (* 2 v)) (* w w r r)) (- 1 v)) 4.5))
