#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/programs)

(provide *points* *exacts* *num-points* *exp-size*
	 sample-expbucket sample-float sample-uniform sample-integer
         make-exacts prepare-points prepare-points-period errors errors-score)

(define *num-points* (make-parameter 1024))
(define *exp-size* (make-parameter 256))
(define *precision-step* (make-parameter 8))

(define *points* (make-parameter '()))
(define *exacts* (make-parameter '()))

(define (sample-expbucket num)
  (let ([bucket-width (/ (- (*exp-size*) 2) num)]
        [bucket-bias (- (/ (*exp-size*) 2) 1)])
    (for/list ([i (range num)])
      (expt 2 (- (* bucket-width (+ i (random))) bucket-bias)))))

(define (random-single-flonum)
  (real->single-flonum
   (let ([sign (if (= (random 2) 0) + -)]
         [exponent (random 256)]
         [mantissa (/ (random (expt 2 23)) (expt 2 23))])
     (cond
      [(and (= exponent 0) (= mantissa 0))
       (sign 0.0)]
      [(= exponent 0)
       (sign (* (expt 2 -126) mantissa))]
      [(and (= exponent 255) (= mantissa 0))
       (sign +inf.0)]
      [(= exponent 255)
       +nan.0]
      [else
       (sign (* (expt 2 (- exponent 127)) (+ 1 mantissa)))]))))

(define (sample-float num)
  (for/list ([i (range num)])
    (real->double-flonum (random-single-flonum))))

(define ((sample-uniform a b) num)
  (build-list num (λ (_) (+ (* (random) (- b a)) a))))

(define (sample-integer num)
  (build-list num (λ (_) (- (random-exp 32) (expt 2 31)))))

(define (make-period-points num periods)
  (let ([points-per-dim (floor (exp (/ (log num) (length periods))))])
    (apply list-product
	   (map (λ (prd)
		  (let ([bucket-width (/ prd points-per-dim)])
		    (for/list ([i (range points-per-dim)])
		      (+ (* i bucket-width) (* bucket-width (random))))))
		periods))))

(define (select-every skip l)
  (let loop ([l l] [count skip])
    (cond
     [(null? l) '()]
     [(= count 0)
      (cons (car l) (loop (cdr l) skip))]
     [else
      (loop (cdr l) (- count 1))])))

(define (make-exacts* prog pts)
  (let ([f (eval-prog prog mode:bf)] [n (length pts)])
    (let loop ([prec (- (bf-precision) (*precision-step*))]
               [prev #f])
      (bf-precision prec)
      (let ([curr (map f pts)])
        (if (and prev (andmap =-or-nan? prev curr))
            curr
            (loop (+ prec (*precision-step*)) curr))))))

(define (make-exacts prog pts)
  (define n (length pts))
  (let loop ([n* 16]) ; 16 is arbitrary; *num-points* should be n* times a power of 2
    (cond
     [(>= n* n)
      (make-exacts* prog pts)]
     [else
      (make-exacts* prog (select-every (round (/ n n*)) pts))
      (loop (* n* 2))])))

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

(define (prepare-points prog samplers)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"

  ; First, we generate points;
  (let loop ([pts '()] [exs '()])
    (if (>= (length pts) (*num-points*))
        (values (take pts (*num-points*)) (take exs (*num-points*)))
        (let* ([num (- (*num-points*) (length pts))]
               [pts1 (flip-lists (for/list ([rec samplers]) ((cdr rec) num)))]
               [exs1 (make-exacts prog pts1)]
               ; Then, we remove the points for which the answers
               ; are not representable
               [pts* (filter-points pts1 exs1)]
               [exs* (filter-exacts pts1 exs1)])
          (loop (append pts* pts) (append exs* exs))))))

(define (prepare-points-period prog periods)
  (let* ([pts (make-period-points (*num-points*) periods)]
	 [exacts (make-exacts prog pts)]
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
