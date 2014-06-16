#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/programs)

(provide *points* *exacts* *num-points* make-exacts prepare-points errors errors-score)

(define *num-points* (make-parameter 512))
(define *exp-size* (make-parameter 256))

(define *points* (make-parameter '()))
(define *exacts* (make-parameter '()))

(define (select-points num)
  (let ([bucket-width (/ (- (*exp-size*) 2) num)]
        [bucket-bias (- (/ (*exp-size*) 2) 1)])
    (for/list ([i (range num)])
      (expt 2 (- (* bucket-width (+ i (random))) bucket-bias)))))

(define (make-points num dim)
  "Produce approximately `num` points in the `dim`-dimensional space,
   distributed overly-uniformly in the exponent"
  (if (= dim 0)
      '(())
      (let* ([num-ticks (round (expt num (/ 1 dim)))]
             [rest-num (/ num num-ticks)]
             [pos-ticks (ceiling (/ num-ticks 2))]
             [neg-ticks (- num-ticks pos-ticks)]
             [first (append (reverse (map - (select-points neg-ticks)))
                            (select-points pos-ticks))])
        (sort
         (apply append
                (for/list ([rest (make-points rest-num (- dim 1))])
                  (map (λ (x) (cons x rest)) first)))
         < #:key car))))

(define (make-exacts prog pts)
  (let ([f (eval-prog prog mode:bf)])
    (let loop ([prec (- (bf-precision) 16)] [prev #f])
      (bf-precision prec)
      (let ([curr (map f pts)])
        (if (and prev (andmap =-or-nan? prev curr))
            curr
            (loop (+ prec 16) curr))))))

(define (filter-points pts exacts)
  "Take only the points for which the exact value is normal"
  (reap (sow)
    (for ([pt pts] [exact exacts])
      (when (ordinary-float? exact)
        (sow pt)))))

(define (filter-exacts pts exacts)
  "Take only the exacts for which the exact value is normal"
  (filter ordinary-float? exacts))

; These definitions in place, we finally generate the points.

(define (prepare-points prog)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"

  (bf-precision 80)

  ; First, we generate points;
  (let* ([pts (make-points (*num-points*) (length (program-variables prog)))]
         [exacts (make-exacts prog pts)]
         ; Then, we remove the points for which the answers
         ; are not representable
         [pts* (filter-points pts exacts)]
         [exacts* (filter-exacts pts exacts)])
    (values pts* exacts*)))

(define (errors prog points exacts)
  (let ([fn (eval-prog prog mode:fl)])
    (for/list ([point points] [exact exacts])
      (let ([out (fn point)])
        (if (real? out)
            (+ 1 (abs (flonums-between out exact)))
            (+ 1 (flonum->ordinal +nan.0)))))))

(define (errors-score e)
  (let-values ([(reals unreals) (partition ordinary-float? e)])
    (/ (+ (apply + (map (λ (e) (/ (log e) (log 2))) reals))
	  (* 64 (length unreals)))
       (length e))))
