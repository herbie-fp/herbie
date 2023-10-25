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

(define (sin-correctness x)
  ; Random number in double-precision
  ;(define flo-num-lo (bit-field->flonum (random-bits 64)))
  ;(define flo-num-hi (flstep flo-num-lo (random-integer 1 1000)))
  
  ; Make an ival out of these floats
  ;(define iv (ival (bf flo-num-lo) (bf flo-num-hi)))
  
  (ival-sin x)
  (void))
  ;(match-define (list mix-lo mix-hi) (ival->list-flo (ival-sin-mixed iv)))
  #;(cond
    [(not (ival-fl-cmp reg-lo reg-hi mix-lo mix-hi))
     (printf "interval that caused rounding errors ~a\n" iv)
     (printf "regular sin in double precision: ~a\n" (ival->list-flo (ival-sin iv)))
     (printf "mixed sin in double precision  : ~a\n\n" (ival->list-flo (ival-sin-mixed iv)))])

(define (sin-correctness-modified x)
  ; Random number in double-precision
  ;(define flo-num-lo (bit-field->flonum (random-bits 64)))
  ;(define flo-num-hi (flstep flo-num-lo (random-integer 1 1000)))
  
  ; Make an ival out of these floats
  ;(define iv (ival (bf flo-num-lo) (bf flo-num-hi)))
  
  (ival-sin-modified x)
  (void))

(bf-precision 1024)
;(sin-correctness)
;(for/list ([i (in-range 100000)]) (sin-correctness))

(define intervals (interval-generator 1000))
(time (map sin-correctness intervals))
(time (map sin-correctness-modified intervals))


