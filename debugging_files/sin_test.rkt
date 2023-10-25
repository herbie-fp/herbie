#lang racket

(require "../src/rival.rkt" math/bigfloat)

(define (close-enough? lo hi)
    (let ([lo* (bigfloat->flonum lo)] [hi* (bigfloat->flonum hi)])
      (list lo* hi*)))


(time
 (for/list ([i (in-range 1000)])
   (bf-precision 1024)
   (define x (ival (bf 1e100)))
   (define e (ival (bf 1e-100)))
 
   (define a (ival-add x e))

 ;(define res_divmod (ival-sin-mixed a))
   
   ((close-enough->ival close-enough?)(ival-sin-modified a))))
 

 ;((close-enough->ival close-enough?) res_divmod)
; ((close-enough->ival close-enough?) res_regular)

;(time (ival-sin-mixed a))



