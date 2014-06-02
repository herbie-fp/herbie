#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/programs)

(provide *points* *exacts* *eval-pts* make-points make-exacts
         prepare-points
         errors errors-compare errors-difference errors-diff-score
	 errors-score reasonable-error? fn-points ascending-order
	 avg-bits-error)

(define *eval-pts* (make-parameter 512))
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
             [first (append (select-points pos-ticks)
                            (map - (select-points neg-ticks)))])
        (sort
         (apply append
                (for/list ([rest (make-points rest-num (- dim 1))])
                  (map (λ (x) (cons x rest)) first)))
         < #:key car))))

(define (make-exacts* f pts start-prec inc-prec prev)
  (bf-precision start-prec)
  (let loop ([pts pts] [new (map f pts)] [prev prev] [good '()] [bad '()])
    (cond
     [(null? pts)
      (let* ([bad-pts (map car bad)] [bad-ans (map cdr bad)]
             [new-ans
              (if (null? bad-pts)
                  '()
                  (make-exacts* f bad-pts (+ start-prec inc-prec) inc-prec bad-ans))])
        (map cdr (sort (append good (map cons bad-pts new-ans)) < #:key caar)))]
     [(and (car prev) (or (and (nan? (car prev)) (nan? (car new))) (= (car prev) (car new))))
      (loop (cdr pts) (cdr new) (cdr prev)
            (cons (cons (car pts) (car new)) good) bad)]
     [else
      (loop (cdr pts) (cdr new) (cdr prev)
            good (cons (cons (car pts) (car new)) bad))])))

(bf-precision 256)

(define (make-exacts prog pts)
  (let ([f (eval-prog prog mode:bf)])
    (let loop ([prec 64] [prev #f])
      (bf-precision prec)
      (let ([curr (map f pts)])
        (if (list= prev curr)
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

  ; First, we generate points;
  (let* ([pts (make-points (*eval-pts*) (length (program-variables prog)))]
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
            (+ 1 (flulp-error out (->flonum exact)))
            +inf.0)))))

(define (fn-points prog points)
  (let ([fn (eval-prog prog mode:fl)])
    (map fn points)))

(define (reasonable-error? x)
  ; TODO : Why do we need the 100% error case?
  (not (or (infinite? x) (nan? x))))

(define (errors-compare errors1 errors2)
  (map (λ (x) (cond [(< x 0) '<] [(> x 0) '>] [#t '=]))
       (errors-difference errors1 errors2)))

(define (errors-difference errors1 errors2)
  (for/list ([error1 errors1] [error2 errors2])
    (cond
     [(and (reasonable-error? error1) (reasonable-error? error2))
      (if (or (<= error1 0) (<= error2 0))
          (error "Error values must be positive" error1 error2)
          (/ (log (/ error1 error2)) (log 2)))]
     [(or (and (reasonable-error? error1) (not (reasonable-error? error2))))
      -inf.0]
     [(or (and (not (reasonable-error? error1)) (reasonable-error? error2)))
      +inf.0]
     [#t
      0.0]
     [#t (error "Failed to classify error1 and error2" error1 error2)])))

(define (errors-diff-score e1 e2)
  (let ([es1 (avg-bits-error e1)]
	[es2 (avg-bits-error e2)])
    (- es1 es2)))

(define (errors-score e)
  (let*-values ([(reals infs) (partition (lambda (n) (rational? n)) e)]
		[(positive-infs negative-infs) (partition (lambda (n) (> 0 n)) infs)])
    (/
     (+ (apply + reals)
        (* 64 (- (length negative-infs) (length positive-infs))))
     (length e))))

(define (avg-bits-error e)
  (let-values ([(reals unreals) (partition (λ (n) (rational? n)) e)])
    (/ (+ (apply + (map (λ (e) (/ (log e) (log 2))) reals))
	  (* 64 (length unreals)))
       (length e))))

;; Given a list in point order (small-positive to large-positive, then small-negative to large-negative),
;; Reorder it into ascending order (large-negative to small-negative, small-positive to large-positive).
(define (ascending-order var-index l)
  ;; The number of positives looks like it can vary, so first check how many points there are with a postive
  ;; number at position 'var-index'
  (let* ([num-positives (length (filter (compose positive? (curry (flip-args list-ref) var-index)) (*points*)))]
	 ;; Get the items that correspond to the positive and the negative points in two seperate lists.
	 [positives (take l num-positives)]
	 [negatives (drop l num-positives)])
    ;; Reverse the negatives, since they are initially in descending order, and then
    ;; append them to the postives to get the points in ascending order.
    (append (reverse negatives) positives)))
