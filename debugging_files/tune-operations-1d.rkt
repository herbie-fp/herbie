#lang racket/base

(require math/bigfloat)
(require racket/list math/flonum)
(require (only-in math/base random-bits))
(require math/base)

(define (bf->prec-bf prec x)
  (parameterize ([bf-precision prec] [bf-rounding-mode 'nearest])
    (bf+ x 0.bf)))

; return a random FP value in 8192bits of precision
; the range of the input values is somewhere within [0, 1e400]
(define (random-number)  
  (parameterize ([bf-precision 8192])
    (bf/
     (bf- (bfrandom) (bf 0.5))
     (bf (random-bits (random-integer 1 1000))))
    ;(bf/ (bf- (bfrandom) (bf 0.5)) (bf 1e300))
    #;(bf (random-bits 8192) (random -1000 1000))))

; Input:
;    x:  a random FP in 8192 bits of precision,
;    op: a 1d function, for example '(bfsin x)' or '(bfsin (bfcos x))'
;    output-prec: the output-precision for this 1d function.
; Output:
;    A precision that is x needed to be, to produce the same results as 8192bits rounded to output-prec
(define (define-prec x op output-prec)
  ; true result: 1d function under 8192bits with 8192bits input, rounded to output-prec with nearest mode
  (define true-res
    (bf->prec-bf output-prec (parameterize
                                 ([bf-precision 8192])
                               (op x))))

  ; target-prec: a miminal found precision for x that would give the same good results as true-res
  (define target-prec (for/list ([i (in-range 24 8192 1)]
                                 #:break
                                 (parameterize ([bf-precision (+ 10 output-prec)])
                                   (bf=
                                    (bf->prec-bf output-prec             ; cast to output-prec precision
                                                 (parameterize ([bf-rounding-mode 'up])
                                                   ; execute 1d function with (+ 10 output-prec) bits of precision
                                                   (op                   
                                                    (bf->prec-bf i x)))) ; cast x to i-precision
                                    (bf->prec-bf output-prec
                                                 (parameterize ([bf-rounding-mode 'down])
                                                   (op
                                                    (bf->prec-bf i x))))
                                    true-res)))
                        i))
  (cond
    [(empty? target-prec) 24]
    [else (+ (last target-prec) 1)]))

(define (spinner op output-prec)
  (define x (random-number))

  ; x-exp = ((precision x)+(exponent x))
  (define x-exp (+ (bigfloat-exponent x) 8192)) ; here x has precision 8192.

  (define target-prec (define-prec x op output-prec))
  
  (define prediction (+ x-exp output-prec 15))
  #;(printf "x-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
             x-exp
             target-prec
             prediction
             (abs (- target-prec prediction)))
  (cond
    [(> target-prec prediction)
     (printf "x-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
             x-exp
             target-prec
             prediction
             (abs (- target-prec prediction)))]
    [(< 30 (abs (- target-prec prediction)))
     (printf "x-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
             x-exp
             target-prec
             prediction
             (abs (- target-prec prediction)))])
  )

(define (op x) (bfsin x))
(define output-prec 128)
(for/list ([i (in-range 10)]) (spinner op output-prec))
