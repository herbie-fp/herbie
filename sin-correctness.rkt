#lang racket

(require math/bigfloat "src/rival.rkt" math/flonum math/base)
(require (only-in math/base random-bits))

(define (ival->list-flo iv)
  (let ([lo (ival-lo iv)] [hi (ival-hi iv)])
    (let ([lo* (bigfloat->flonum lo)] [hi* (bigfloat->flonum hi)])
      (list lo* hi*))))

(define (ival-fl-cmp x-lo x-hi y-lo y-hi)
  (and (equal? x-lo y-lo) (equal? x-hi y-hi)))

(define (ival-bf-cmp x-lo x-hi y-lo y-hi)
  (and (bf= x-lo y-lo) (bf= x-hi y-hi)))

(define (interval-generator n)
  (cond
    [(zero? n) '()]
    [else
     (define flo-num-lo (bit-field->flonum (random-bits 64)))
     (define flo-num-hi (flstep flo-num-lo (random-integer 1 1000)))
     (cons (ival (bf flo-num-lo) (bf flo-num-hi)) (interval-generator (- n 1)))]))

(define (sin-correctness)
  ; Random number in double-precision
  (define flo-num-lo (bit-field->flonum (random-bits 64)))
  (define flo-num-hi (flstep flo-num-lo (random-integer 1 1000)))
  
  ; Make an ival out of these floats
  (define iv (ival (bf flo-num-lo) (bf flo-num-hi)))
  
  (match-define (list reduced-lo reduced-hi) (ival->list-flo (ival-sin-reduced iv)))
  (match-define (list reg-lo reg-hi) (ival->list-flo (ival-sin iv)))
  (cond
    [(not (ival-fl-cmp reg-lo reg-hi reduced-lo reduced-hi))
     (printf "interval that caused rounding errors ~a\n" iv)
     (printf "regular sin in double precision: ~a\n" (ival->list-flo (ival-sin iv)))
     (printf "mixed sin in double precision  : ~a\n\n" (ival->list-flo (ival-sin-reduced iv)))])
  (void))
  

(bf-precision 2048)
;(sin-correctness)
;(for/list ([i (in-range 10000)]) (sin-correctness))

(define intervals (interval-generator 10000))

(parameterize ([bf-precision 128])
  (time (map
         (lambda x
           (ival-sin-reduced (first x))
           (void))
         intervals)))

(parameterize ([bf-precision 2048])
  (time (map
         (lambda x
           (ival-sin (first x))
           (void))
         intervals)))


  