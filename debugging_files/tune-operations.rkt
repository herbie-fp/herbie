#lang racket/base

(require math/bigfloat)
(require racket/list math/flonum)
(require (only-in math/base random-bits))
(require math/base)

(define *max-prec* (make-parameter 8192))

(define (bf->prec-bf prec x)
  (parameterize ([bf-precision prec] [bf-rounding-mode 'nearest])
    (bf+ x 0.bf)))


(define (random-number)
  (parameterize ([bf-precision (*max-prec*)])
    (bf/
     (bf- (bfrandom) (bf 0.5))
     (bf (random-bits (random-integer 1 1000))))))


(define (define-output-prec x y op output-prec)
  ; True result in output-prec
  (define true-res (bf->prec-bf output-prec
                                (parameterize ([bf-precision (*max-prec*)])
                                  (op x y))))
  ; Two points in a given input precision
  (define x* (bf->prec-bf (bf-precision) x))
  (define y* (bf->prec-bf (bf-precision) y))
  (define target-prec (for/list ([i (in-range 24 (*max-prec*))]
                                 #:break
                                 (parameterize ([bf-precision output-prec])
                                   (bf=
                                    (bf->prec-bf output-prec (parameterize ([bf-precision i])
                                                               (parameterize ([bf-rounding-mode 'up])
                                                                 (op x* y*))))
                                    (bf->prec-bf output-prec (parameterize ([bf-precision i])
                                                               (parameterize ([bf-rounding-mode 'down])
                                                                 (op x* y*))))
                                    true-res)))
                        i))
  (cond
    [(empty? target-prec) 24]
    [else (+ (last target-prec) 1)]))


; We are given output-prec and points
#;(define (define-input-prec x y op output-prec)
  (define true-res (bf->prec-bf output-prec
                                (parameterize ([bf-precision (*max-prec*)])
                                  (op x y)))))

(define (spinner prec)
  (parameterize ([bf-precision prec])
    ; Generating a random x-point with 8192bits of prec
    (define x (random-number))
    (define x-exp (+ (bigfloat-exponent x) (bigfloat-precision x)))
    
    ; Generating a random y-point with 8192bits of prec
    (define y (random-number))
    (define y-exp (+ (bigfloat-exponent y) (bigfloat-precision y)))

    (define op bf-)
    (define output-prec 128) ; Set the output precision the we want the result to be fixed in
    ; Define precision for 'op' that would produce an interval that will be fixed in
    ;                                                               'output-prec' bits of precision
    (define op-prec (define-output-prec x y op output-prec)) 
    
    (define prediction (+ 20 (abs (- x-exp y-exp)) prec))
    #;(printf "x-exp=~a\nx-prec=~a\ny-exp=~a\ny-prec=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
            x-exp
            (bigfloat-precision x)
            y-exp
            (bigfloat-precision y)
            op-prec
            prediction
            (abs (- x-exp y-exp)))
    (cond
      [(> op-prec prediction)
       (printf "x-exp=~a\ny-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
               x-exp
               y-exp
               op-prec
               prediction
               (abs (- x-exp y-exp)))]
      [(< 30 (abs (- op-prec prediction)))
       (printf "x-exp=~a\ny-exp=~a\ntarget-prec=~a\npred=~a\nabs-diff=~a\n\n"
               x-exp
               y-exp
               op-prec
               prediction
               (abs (- x-exp y-exp)))])
  ))

(define prec 512)
(for/list ([i (in-range 10)]) (spinner prec))