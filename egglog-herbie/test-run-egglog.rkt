#lang racket

(require "main-run-egglog.rkt")

; Define  Egglog program as a list of S-expressions
(define raw-egglog-program
  '((datatype M
     (Num Rational))
    (datatype MTy
     (Numf64 Rational))
    (function typed-id (M String) MTy)
    (rule ((= e (Num n)))
          ((let ty "binary64")
           (let ety (Numf64 n))
           (union (typed-id e ty) ety)))
    (let e (Num (rational 1 1)))
    (run 3)
    (extract e)))

; Create an instance of the egglog-program struct
(define my-egglog-program (egglog-program raw-egglog-program))

(displayln (run-egglog my-egglog-program))
