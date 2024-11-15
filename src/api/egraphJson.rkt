#lang racket

(provide startIteration
         endIteration
         addExprs)

(define iter-num 1)
(define localize 0);0=localizeCosts 1=localizeErrors
(define egraphJson (make-hash))

(define startIteration (hash-set! egraphJson iter-num (make-hash)))

(define endIteration (set! iter-num (+ iter-num 1)))

(define (startLocalize))

(define getIteration (hash-ref egraphJson iter-num))

(define (addExprs key exprs)
  (hash-set! (getIteration) key exprs))

(define (addSubexprs expr final-subexprs)

)