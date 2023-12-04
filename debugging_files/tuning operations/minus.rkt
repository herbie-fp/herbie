#lang racket/base

(require math/bigfloat)
(require racket/list math/flonum)
(require (only-in math/base random-bits))
(require math/base racket/match)

(define *max-prec* (make-parameter 8192))
(define *extra-prec* (make-parameter 20))

; Casts x to 'prec' precision
(define (bf->prec-bf prec x)
  (parameterize ([bf-precision prec] [bf-rounding-mode 'nearest])
    (bf+ x 0.bf)))

;; ------------------------------------- Random Generators -------------------------------------------
; Generates two FP points with 1 exponent off distance
(define (one-exp-off-points)
  (define exponent (random-integer -1000 1000))
  (define x (parameterize ([bf-precision (*max-prec*)])
                (bfstep
                 (bfexpt 2.bf (bf exponent))
                 (- (expt 2 (random-integer 1 (*max-prec*)))))))
  (define y (parameterize ([bf-precision (*max-prec*)])
                (bfstep
                 (bfexpt 2.bf (bf exponent))
                 (expt 2 (random-integer 1 (*max-prec*))))))
  (values x y))
 
; Generates a random FP point with a high exponent
(define (random-number-high-exponent)
  (parameterize ([bf-precision (*max-prec*)])
    (bfstep (bf*
             (bf- (bfrandom) (bf 0.5))
             (bf (random-bits (random-integer 1 1000))))
            (expt 2 (random-integer 1 (*max-prec*))))))

; Generates a random FP point with a low exponent
(define (random-number-low-exponent)
  (parameterize ([bf-precision (*max-prec*)])
    (bfstep (bf/
             (bf- (bfrandom) (bf 0.5))
             (bf (random-bits (random-integer 1 1000))))
            (expt 2 (random-integer 1 (*max-prec*))))))

; Generates two FP points for cancellation case where x is a low exponent FP and y = bfsin(x)
(define (sin-cancellation-points)
  (parameterize ([bf-precision (*max-prec*)])
    (define x (random-number-low-exponent))
    (define y (bfsin x))
    (values x y)))

; Generates two FP points with high exponent values
(define (two-random-points-high-exponents)
  (define x (random-number-high-exponent))
  (define y (random-number-high-exponent))
  (values x y))

; Generates two FP points with low exponent values
(define (two-random-points-low-exponents)
  (define x (random-number-low-exponent))
  (define y (random-number-low-exponent))
  (values x y))



(define (high-and-low-exponent-points)
  (define x (random-number-low-exponent))
  (define y (random-number-high-exponent))
  (values x y))

; Generates one FP with a high exponent and another one with some ulp distance from that point
(define (same-exponent-with-ulp-distance)
  (parameterize ([bf-precision (*max-prec*)])
    (define x (random-number-high-exponent))
    (define y (if (zero? (random-integer 0 1))
                  (bfstep x (expt 2 (random-integer 1 (*max-prec*))))       ; steps forward
                  (bfstep x (- (expt 2 (random-integer 1 (*max-prec*))))))) ; steps backward
    (values x y)))

; Creates two points with exponent distance from 2 up to 30 and a random mantissa shift
(define (few-exponents-distance)
  (parameterize ([bf-precision (*max-prec*)])
    (define exponent (random-integer -1000 1000))
    (define x (bfstep
               (bfexpt 2.bf (bf exponent))
               (- (expt 2 (random-integer 1 (*max-prec*))))))
  
    (define y (if (zero? (random-integer 0 1))
                  (bfstep
                   (bfexpt 2.bf (bf (+ exponent (random-integer 2 30))))  ; exponent shift forward
                           (expt 2 (random-integer 1 (*max-prec*))))      ; random mantissa shift
                  (bfstep
                   (bfexpt 2.bf (bf (- exponent (random-integer 2 30))))  ; exponent shift backward
                           (expt 2 (random-integer 1 (*max-prec*))))))    ; random mantissa shift
    (values x y)))
      
;; ------------------------------------- Precision Definitions ---------------------------------------
; Function defines precision for the operation so that the result will be fixed
;   Given operation and the output-precision in which the result should be fixed
(define (define-output-prec x y op output-prec)
  ; True result in output-prec
  (define true-res (bf->prec-bf output-prec
                                (parameterize ([bf-precision (*max-prec*)])
                                  (op x y))))
  (define target-prec (for/list ([i (in-range 24 (*max-prec*))]
                                 #:break
                                 (parameterize ([bf-precision output-prec])
                                   (bf=
                                    (bf->prec-bf output-prec (parameterize ([bf-precision i])
                                                               (parameterize ([bf-rounding-mode 'up])
                                                                 (op x y))))
                                    (bf->prec-bf output-prec (parameterize ([bf-precision i])
                                                               (parameterize ([bf-rounding-mode 'down])
                                                                 (op x y))))
                                    true-res)))
                        i))
  (cond
    [(empty? target-prec) 24]
    [else (+ (last target-prec) 1)]))

; Function defines input precision of the arguments given output-precision and arguments
(define (define-input-prec x y op output-prec)
  (define true-res (bf->prec-bf output-prec
                                (parameterize ([bf-precision (*max-prec*)])
                                  (op x y))))
  (define target-prec (for/list ([i (in-range 24 (*max-prec*) 1)]
                                 #:break
                                 (parameterize ([bf-precision (+ (*extra-prec*) output-prec)])
                                   (bf=
                                    (bf->prec-bf output-prec             ; cast to output-prec precision
                                                 (parameterize ([bf-rounding-mode 'up])
                                                   ; execute 2d function with (+ 10 output-prec) bits of precision
                                                   (op                   
                                                    (bf->prec-bf i x) (bf->prec-bf i y)))) ; cast x to i-precision
                                    (bf->prec-bf output-prec
                                                 (parameterize ([bf-rounding-mode 'down])
                                                   (op
                                                    (bf->prec-bf i x) (bf->prec-bf i y))))
                                    true-res)))
                        i))
  (cond
    [(empty? target-prec) 24]
    [else (+ (last target-prec) 1)]))

;; -------------------------------------------- Fuzzing ----------------------------------------------

(define (spinner op verbose)
  (define output-prec (random-integer 24 4096))  ; random output-precision

  ; Generates two random points which can cause cancellation, but some cases do not cause on purpose
  (define-values (x* y*)
    (let ([choice (random-integer 0 6)])
      (match choice
        [0 (one-exp-off-points)]                 ; possible cancellation
        [1 (sin-cancellation-points)]            ; possible cancellation
        [2 (two-random-points-high-exponents)]   ; no cancellation
        [3 (two-random-points-low-exponents)]    ; no cancellation
        [4 (high-and-low-exponent-points)]       ; no cancellation
        [5 (same-exponent-with-ulp-distance)]    ; possible cancellation
        [6 (few-exponents-distance)])))          ; weird case, some points crash the algorithm


  
  (define-values (x y)
    (parameterize ([bf-precision (*max-prec*)])
      (match (random-integer 0 3)
        [0 (values x* y*)]
        [1 (values (bf* x* -1.bf) y*)]
        [2 (values x* (bf* y* -1.bf))]
        [3 (values (bf* x* -1.bf) (bf* y* -1.bf))])))
    
  (define x-exp (+ (bigfloat-exponent x) (bigfloat-precision x)))
  (define y-exp (+ (bigfloat-exponent y) (bigfloat-precision y)))
    
  ; Define precision for 'op' that would produce an interval that will be fixed
  (define op-prec (define-output-prec x y op output-prec))

  ; Define input precision so than the interval will be fixed in output-prec bits of precision
  (define input-prec (define-input-prec x y op output-prec))
    
  (define op-prediction (+ (*extra-prec*) output-prec)) ; prediction of what the precision should be for op
    
  (define out-exp (parameterize ([bf-precision (*max-prec*)])
                    (+ (bigfloat-exponent (bf- x y)) (*max-prec*))))
    
  #;(define input-prediction (min (*max-prec*)  ; prediction of what the input precision should be
                                (+ 20         ; for that output-prec
                                   output-prec
                                   (if (and (>= 1 (abs (- x-exp y-exp)))
                                            (> x-exp -9220000000000000000))  ; 0.bf case to be added when modifying compiler.rkt
                                       (abs (+ x-exp out-exp))  ; y-exp to be considered as well
                                       0))))
  (define input-prediction (+ (*extra-prec*) (if (and 
                                                  (> (- x-exp out-exp) 0)
                                                  (or (< -9220000000000000000 x-exp)
                                                      (equal? x-exp -9223372036854775807)))
                                     (- x-exp out-exp)
                                     0)
                              output-prec))
  
  (when verbose
    (printf "x-exp=~a\nx=~a\ny-exp=~a\ny=~a\ntarget-prec=~a\npred=~a\nout-prec=~a\nout-exp=~a\n\n"
             x-exp
             (bf->prec-bf 53 x)
             y-exp
             (bf->prec-bf 53 y)
             input-prec
             input-prediction
             output-prec
             out-exp))

  ; Check prediction for operation precision
  (cond
    [(> op-prec op-prediction)
     (printf "Operation precision misprediction for:
\tx-exponent=~a\n\tx=~a\n\ty-exponent=~a\n\ty=~a\n\ttrue-input-prec=~a\n\tpredicted-input-precision=~a\n\toutput-exponent=~a\n\n"
            x-exp
            (bf->prec-bf 53 x)
            y-exp
            (bf->prec-bf 53 y)
            op-prec
            op-prediction
            out-exp)]
    #;[(< 30 (abs (- op-prec op-prediction)))
     (printf "Operation precision prediction for these points is correct but with a large margin (~a):
\tx-exponent=~a\n\tx=~a\n\ty-exponent=~a\n\ty=~a\n\ttrue-input-prec=~a\n\tpredicted-input-precision=~a\n\toutput-exponent=~a\n\n"
             (abs (- op-prec op-prediction))
             x-exp
            (bf->prec-bf 53 x)
            y-exp
            (bf->prec-bf 53 y)
            op-prec
            op-prediction
            out-exp)])

  ; Check prediction for input precision
  (cond
    [(> input-prec input-prediction)
     (printf "Input precision misprediction for:
\tx-exponent=~a\n\tx=~a\n\ty-exponent=~a\n\ty=~a\n\ttrue-input-prec=~a\n\tpredicted-input-precision=~a\n\tout-prec=~a\n\toutput-exponent=~a\n\n"
            x-exp
            (bf->prec-bf 53 x)
            y-exp
            (bf->prec-bf 53 y)
            input-prec
            input-prediction
            output-prec
            out-exp)]
    [(and (< 1 (abs (- input-prec input-prediction))) (> (- input-prediction output-prec) (*extra-prec*)))
     (printf "Input precision is too off for:
\tx-exponent=~a\n\tx=~a\n\ty-exponent=~a\n\ty=~a\n\ttrue-input-prec=~a
\tpredicted-input-precision=~a\n\tout-prec=~a\n\toutput-exponent=~a\n\tabs-diff=~a\n\n"
            x-exp
            (bf->prec-bf 53 x)
            y-exp
            (bf->prec-bf 53 y)
            input-prec
            input-prediction
            output-prec
            out-exp
            (- input-prediction input-prec))]))

(define op bf-)
(define verbose #f)
(for/list ([i (in-range 1000)]) (spinner op verbose))

#| Input precision misprediction for:
	x-exponent=-562
	x=(bf "5.856119881895219e-170")
	y-exponent=-587
	y=(bf "1.8414247483239091e-177")
	true-input-prec=8192
	predicted-input-precision=1554
	output-exponent=562
|#
