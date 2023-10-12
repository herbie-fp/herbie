#lang racket

(require "src/rival.rkt" math/bigfloat)

(define (close-enough? lo hi)
    (let ([lo* (bigfloat->flonum lo)] [hi* (bigfloat->flonum hi)])
      (list lo* hi*)))

(bf-precision 64)
(define x (ival (bf 1e100)))
(define e (ival (bf 1e-100)))

(bf-precision 512)
(define a (ival-add x e))

;; k, s = divmod(a)
(bf-precision 512)
(define remainder (ival-div a (ival-pi)))
(define div_res (ival-sub remainder (ival (bf 0.5) (bf 0.5))))

(define k (ival-floor div_res))

(define x2 (ival-mult
            (ival
             (bffrac (ival-lo remainder))
             (bffrac (ival-hi remainder)))
            (ival-pi)))
x2

(bf-precision 512)
(define res_divmod (ival-sin x2))

(bf-precision 512)
(define res_regular (ival-sin a))

((close-enough->ival close-enough?) res_divmod)
((close-enough->ival close-enough?) res_regular)



