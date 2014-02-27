#lang racket
(require casio/test)

(casio-bench (alpha beta)
  "Code from GNU Octave 3.8, CollocWt.cc, jcobi"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)])
    (/ (+ (/ ad (+ ab 2.0)) 1.0) 2.0)))

(casio-bench (alpha beta i)
  "Code from GNU Octave 3.8, CollocWt.cc, jcobi"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z (+ ab (* 2 i))])
    (/ (+ (/ (* ab ad) z (+ z 2.0)) 1.0) 2.0)))

(casio-bench (alpha beta)
  "Code from GNU Octave 3.8, CollocWt.cc, jcobi"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z (+ ab (* 2 i))])
    (/ (+ ab ap 1.0) z z (+ z 1.0))))

(casio-bench (alpha beta i)
  "Code from GNU Octave 3.8, CollocWt.cc, jcobi"
  (let* ([ab (+ alpha beta)]
         [ad (- beta alpha)]
         [ap (* beta alpha)]
         [z (+ ab (* 2 i))]
         [z* (* z z)]
         [y (* i (+ ab i))]
         [y* (* y (+ ap y))])
    (/ y* z* (- z* 1.0))))

