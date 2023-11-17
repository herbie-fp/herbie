#lang racket/base

(require math/bigfloat)
(require racket/list math/flonum)
(require (only-in math/base random-bits))
(require math/base)

(define (bf->current-prec-bf x)
  (bf+ x 0.bf))

(define (random-number prec)
  (parameterize ([bf-precision prec])
    (bf/
     (bf- (bfrandom) (bf 0.5))
     (bf (random-bits (random-integer 1 1000))))
    #;(bf (random-bits prec) (random
                            -2000
                            -1200)))) ;-1200


(define (define-prec x y op [output-prec 256])
  (define true-res (parameterize ([bf-precision 16384]) (op x y)))
  (define target-prec (for/list ([i (in-range 24 8192 1)]
                                 #:break
                                 (parameterize ([bf-precision output-prec])
                                   (bf=
                                    (bf->current-prec-bf (parameterize ([bf-precision i])
                                           (parameterize ([bf-rounding-mode 'up])
                                             (op x y))))
                                    (bf->current-prec-bf (parameterize ([bf-precision i])
                                            (parameterize ([bf-rounding-mode 'down])
                                              (op x y))))
                                    (bf->current-prec-bf true-res))))
                        i))
  (cond
    [(empty? target-prec) 24]
    [else (+ (last target-prec) 1)]))

(define (spinner prec)
  (parameterize ([bf-precision (random 10 512)])
    (define x (random-number (bf-precision)))
    (define x-exp (bigfloat-exponent x))
    (println x)

    (parameterize ([bf-precision (+ 10 (bf-precision))])
      (define y (random-number (bf-precision)))
      (define y-exp (bigfloat-exponent y))

      (define op bf-)
      (define output-prec 53)
      (define target-prec (define-prec x (bfsin x) op output-prec))
    
      (define abs-diff (- target-prec x-exp))
      (define prediction (+ 20 output-prec))
      (printf "x-exp=~a\nx-prec=~a\ny-exp=~a\ny-prec=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
              x-exp
              (bigfloat-precision x)
              y-exp
              (bigfloat-precision y)
              target-prec
              prediction
              (abs (- x-exp y-exp)))
      #;(cond
          [(> target-prec prediction)
           (printf "x-exp=~a\ny-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
                   x-exp
                   y-exp
                   target-prec
                   prediction
                   (abs (- x-exp y-exp)))]
          [(< 20 (abs (- target-prec prediction)))
           (printf "x-exp=~a\ny-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
                   x-exp
                   y-exp
                   target-prec
                   prediction
                   (abs (- x-exp y-exp)))])
  )))

(define prec 256)
(for/list ([i (in-range 10)]) (spinner prec))