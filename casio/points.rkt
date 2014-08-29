#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/programs)

(provide *points* *exacts* *num-points* *exp-size*
	 make-points-expbucket make-points-random make-points-uniform
         make-exacts prepare-points prepare-points-period errors errors-score)

(define *num-points* (make-parameter 1024))
(define *exp-size* (make-parameter 256)) ; TODO : Combine with below into a single way to specify
(define *precision-step* (make-parameter 8))

(define *points* (make-parameter '()))
(define *exacts* (make-parameter '()))

(define (select-points-expbucket num)
  (let ([bucket-width (/ (- (*exp-size*) 2) num)]
        [bucket-bias (- (/ (*exp-size*) 2) 1)])
    (for/list ([i (range num)])
      (expt 2 (- (* bucket-width (+ i (random))) bucket-bias)))))

(define (select-points-uniform num)
  (let ([bucket-width (/ (expt 2 (*exp-size*)) num)])
    (for/list ([i (range num)])
      (* bucket-width (+ i (random))))))

(define (random-single-flonum)
  (let loop ()
    (let ([x (bit-field->flonum (random-exp 64))])
      (if (or (= x 0) (and (ordinary-float? (real->single-flonum x))
                           (not (= (real->single-flonum x) 0))))
          (real->double-flonum (real->single-flonum x))
          (loop)))))

(define (select-points-random num)
  (for/list ([i (range num)])
    (random-single-flonum)))

(define ((point-factory selector) num dim)
  (if (= dim 0)
      '(())
      (let* ([num-ticks (round (expt num (/ 1 dim)))]
             [rest-num (/ num num-ticks)]
             [pos-ticks (ceiling (/ num-ticks 2))]
             [neg-ticks (- num-ticks pos-ticks)]
             [first (append (reverse (map - (selector neg-ticks)))
                            (selector pos-ticks))])
        (sort (apply append
                     (for/list ([rest ((point-factory selector) rest-num (- dim 1))])
                       (map (λ (x) (cons x rest)) first)))
              < #:key car))))

(define make-points-expbucket (point-factory select-points-expbucket))
(define make-points-random (point-factory select-points-random))
(define make-points-uniform (point-factory select-points-uniform))

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

(define (prepare-points prog point-maker)
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
