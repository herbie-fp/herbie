#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/programs)

(provide *points* *exacts* *num-points*
	 make-exacts
	 prepare-points prepare-points-period prepare-points-uniform
	 errors errors-score)

(define *num-points* (make-parameter 512))
(define *exp-size* (make-parameter 256))

(define *points* (make-parameter '()))
(define *exacts* (make-parameter '()))

(define (select-points num)
  (let ([bucket-width (/ (- (*exp-size*) 2) num)]
        [bucket-bias (- (/ (*exp-size*) 2) 1)])
    (for/list ([i (range num)])
      (expt 2 (- (* bucket-width (+ i (random))) bucket-bias)))))

(define (select-points-uniform num)
  (let ([bucket-width (/ (expt 2 (*exp-size*)) num)])
    (for/list ([i (range num)])
      (* bucket-width (+ i (random))))))

(define-values (make-points make-points-uniform)
  (letrec ([make-points-inner
	    (λ (num dim selector)
	      (if (= dim 0)
		  '(())
		  (let* ([num-ticks (round (expt num (/ 1 dim)))]
			 [rest-num (/ num num-ticks)]
			 [pos-ticks (ceiling (/ num-ticks 2))]
			 [neg-ticks (- num-ticks pos-ticks)]
			 [first (append (reverse (map - (selector neg-ticks)))
					(selector pos-ticks))])
		    (sort (apply append
				 (for/list ([rest (make-points-inner rest-num (- dim 1) selector)])
				   (map (λ (x) (cons x rest)) first)))
			  < #:key car))))])
    (values
     ;; Produce approximately `num` points in the `dim`-dimensional space,
     ;; distributed overly-uniformly in the exponent
     (curryr make-points-inner select-points)
     ;; Produce approximately `num` points in the `dim`-dimensional space,
     ;; distributed overly-uniformly.
     (curryr make-points-inner select-points-uniform))))

(define (make-period-points num periods)
  (let ([points-per-dim (floor (exp (/ (log num) (length periods))))])
    (apply list-product
	   (map (λ (prd)
		  (let ([bucket-width (/ prd points-per-dim)])
		    (for/list ([i (range points-per-dim)])
		      (+ (* i bucket-width) (* bucket-width (random))))))
		periods))))

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

(define (prepare-distributed-points point-maker prog)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"
  
  (bf-precision 80)
  ; First, we generate points;
  (let* ([pts (point-maker (*num-points*) (length (program-variables prog)))]
         [exacts (make-exacts prog pts)]
         ; Then, we remove the points for which the answers
         ; are not representable
         [pts* (filter-points pts exacts)]
         [exacts* (filter-exacts pts exacts)])
    (values pts* exacts*)))

(define (prepare-points-period prog periods)
  (bf-precision 80)
  (let* ([pts (make-period-points (*num-points*) periods)]
	 [exacts (make-exacts prog pts)]
	 [pts* (filter-points pts exacts)]
	 [exacts* (filter-exacts pts exacts)])
    (values pts* exacts*)))

(define prepare-points (curry prepare-distributed-points make-points))
(define prepare-points-uniform (curry prepare-distributed-points make-points-uniform))

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
