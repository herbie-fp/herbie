#lang racket/base

(require math/bigfloat)
(require racket/list math/flonum)
(require (only-in math/base random-bits))

(define (define-prec x y)
  (define true-res (parameterize ([bf-precision 8192]) (bf+ x y)))
  (define target-prec (for/list ([i (in-range 24 8192 1)]
                                 #:break
                                 (parameterize ([bf-precision 8192])
                                   (bf=
                                    (parameterize ([bf-precision i])
                                      (bf+ x y))
                                    true-res)))
                        i))
  (cond
    [(empty? target-prec) 24]
    [else (+ (last target-prec) 1)]))

(define (spinner prec)
  (parameterize ([bf-precision prec])
    (define x (bf (random-bits prec) (random -12000 12000)))
    (define x-exp (bigfloat-exponent x))
    
    (define y (bf (random-bits prec) (random -12000 12000)))
    ;(define y (bfprev x))
    (define y-exp (bigfloat-exponent y))
    
    (define target-prec (define-prec x y))
    (define abs-diff (- target-prec x-exp))
    (define prediction (if (< 8192 (abs (- x-exp y-exp)))
                           prec
                           (min 8192 (+ 10 (abs (- x-exp y-exp)) prec))))
    ;(printf "x-exp=~a\ny-exp=~a\ntarget-prec=~a\npred=~a\n\n" x-exp y-exp target-prec prediction)
    (cond
      [(> target-prec prediction)
       (printf "x-exp=~a\ny-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
               x-exp
               y-exp
               target-prec
               prediction
               (abs (- x-exp y-exp)))]
      [(< 50 (abs (- target-prec prediction)))
       (printf "x-exp=~a\ny-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
               x-exp
               y-exp
               target-prec
               prediction
               (abs (- x-exp y-exp)))])
  ))

(define prec 512)
(for/list ([i (in-range 100000)]) (spinner prec))