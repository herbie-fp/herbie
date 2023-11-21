#lang racket/base

(require math/bigfloat "RA/herbie/src/rival.rkt")

(define (bf->prec-bf prec x)
  (parameterize ([bf-precision prec] [bf-rounding-mode 'nearest])
    (bf+ x 0.bf)))

(bf-precision 8192)

(define x (bf -6.55361597430678e-216))
(define input-exp (+ (bigfloat-exponent x) (bigfloat-precision x)))

(define acos-res (bfacos x))
(define acos-exp (+ (bigfloat-exponent acos-res) (bigfloat-precision acos-res)))

(define tan-res (bftan acos-res))
(define tan-exp (+ (bigfloat-exponent tan-res) (bigfloat-precision tan-res)))

(printf "input exponent=~a,\nacos exponent=~a,\ntan-exp=~a,\n\n" input-exp acos-exp tan-exp)

(printf "tan-exp should be in 53bits and fixed
\tthe precision for tan-exp=63\n\tinput precision for tan should be ~a\n\n" (+ acos-exp 63 15))


(define output-prec 512)
(parameterize ([bf-precision output-prec])
  (ival-add (ival 0.bf 0.bf)
            (parameterize ([bf-precision (+ output-prec 15)])
              (ival-tan
               (parameterize ([bf-precision (+ (+ (bigfloat-exponent acos-res)
                                                  (max (+ acos-exp (+ output-prec 15) 25)
                                                       (+ (+ acos-exp (+ output-prec 15) 15) input-exp 15)))
                                               (+ output-prec 15) 15)])
                 (ival-acos
                  (parameterize ([bf-precision (max (+ acos-exp (+ output-prec 15) 25)
                                                    (+ (+ acos-exp (+ output-prec 15) 15) input-exp 15))])
                    (ival (bf+ 0.bf x) (bf+ 0.bf x)))))))))

(bf->prec-bf 24
             (parameterize ([bf-precision 64] [bf-rounding-mode 'up])
               (bfacos x)))
(bf->prec-bf 53
             (parameterize ([bf-precision 64] [bf-rounding-mode 'down])
               (bfacos x)))
; bfacos -> bftan -> 53bits
