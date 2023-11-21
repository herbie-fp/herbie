#lang racket/base

(require math/bigfloat)
(require racket/list math/flonum)
(require (only-in math/base random-bits))
(require math/base)

(define (random-number)
  (parameterize ([bf-precision 1024])
   (bfacos (bf/
            (bf- (bfrandom) (bf 0.5))
            (bf (random-bits (random-integer 1 1000)))))))

(define (bf->prec-bf prec x)
  (parameterize ([bf-precision prec] [bf-rounding-mode 'nearest])
    (bf+ x 0.bf)))

(define (define-prec x)
  (define true-res (parameterize ([bf-precision 10000]) (bfround (bf/ x pi.bf))))
  (define target-prec (for/list ([i (in-range 53 8192 20)]
                                 #:break
                                 (bf=
                                  (parameterize ([bf-precision i] [bf-rounding-mode 'up])
                                    (bfround (bf/ x pi.bf)))
                                  (parameterize ([bf-precision i] [bf-rounding-mode 'down])
                                    (bfround (bf/ x pi.bf)))
                                  true-res))
                        i))
  (cond
    [(empty? target-prec) 53]
    [else (+ (last target-prec) 20)]))

(define (spinner out-prec)
  
  (define x (random-number))
      
  (define x-exp (+ (bigfloat-exponent x) 1024))
  
  (define target-prec (define-prec x))
  (define prediction (max 54 (+ x-exp 25)))
  (define diff (- (bigfloat-significand x)
                  (parameterize ([bf-precision 1024]) (bigfloat-significand pi.bf))))
  (printf "x-sign - pi-sign = ~a\n" diff)
  (printf "you need ~a bits\n" (if (> diff 0)
                                   (- 1024 (fllog2 (fl diff)))
                                   54))
          
          
  ;(printf "x=~a\nx-exp=~a\ntarget-prec=~a\n\n" (bf->prec-bf 24 x) x-exp target-prec)
  (cond
    [(> target-prec prediction)
     (printf "x=~a\nx-exp=~a\npred=~a\ntarget-prec=~a\nabs-diff=~a\n\n"
             (bf->prec-bf 24 x)
             x-exp
             prediction
             target-prec
             (abs (- target-prec prediction)))]
    [(< 30 (abs (- target-prec prediction)))
     (printf "x=~a\nx-exp=~a\npred=~a\ntarget-prec=~a\nabs-diff=~a\n\n"
             (bf->prec-bf 24 x)
             x-exp
             prediction
             target-prec
             (abs (- target-prec prediction)))])
  )

(define out-prec 53)
(for/list ([i (in-range 100)]) (spinner out-prec))