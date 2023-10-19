#lang racket

(require math/bigfloat "src/rival.rkt" math/flonum math/base)
(require (only-in math/base random-bits))

(define (ival->list-flo iv)
  (let ([lo (ival-lo iv)] [hi (ival-hi iv)])
    (let ([lo* (bigfloat->flonum lo)] [hi* (bigfloat->flonum hi)])
      (list lo* hi*))))

(define (ival-fl-cmp x-lo x-hi y-lo y-hi)
  (and (equal? x-lo y-lo) (equal? x-hi y-hi)))

(define (sin-correctness)
  ; Random number in double-precision
  (define flo-num-lo (bit-field->flonum (random-bits 64)))
  (define flo-num-hi (flstep flo-num-lo (random-integer 1 1000)))
  ;(define flo-num-hi (flstep flo-num-lo 1))
  ; Make an ival out of these floats
  (define iv (ival (bf flo-num-lo) (bf flo-num-hi)))
  
  (match-define (list reg-lo reg-hi) (ival->list-flo (ival-cos iv)))
  (match-define (list mix-lo mix-hi) (ival->list-flo (ival-cos-mixed iv)))
  (cond
    [(not (ival-fl-cmp reg-lo reg-hi mix-lo mix-hi))
     (printf "interval that caused rounding errors ~a\n" iv)
     (printf "regular sin in double precision: ~a\n" (ival->list-flo (ival-cos iv)))
     (printf "mixed sin in double precision  : ~a\n\n" (ival->list-flo (ival-cos-mixed iv)))]))

(bf-precision 512)
(sin-correctness)

(for/list ([i (in-range 10000)]) (sin-correctness))



