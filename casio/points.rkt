#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/programs)

(provide *points* *exacts* *num-points*
         make-exacts prepare-points
         errors errors-score errors-compare)

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
            (+ 1 (flulp-error out (->flonum exact)))
            +inf.0)))))

(define (errors-score e)
  (let-values ([(reals unreals) (partition ordinary-float? e)])
    (/ (+ (apply + (map (λ (e) (/ (log e) (log 2))) reals))
	  (* 64 (length unreals)))
       (length e))))

(define (errors-compare egood ebad)
  (/
   (for/sum ([e+ egood] [e- ebad])
     (cond
      [(and (ordinary-float? e+) (ordinary-float? e-))
       (if (or (<= e+ 0) (<= e- 0))
           (error "Error values must be positive" e+ e-)
           (/ (- (log e+) (log e-)) (log 2)))]
      [(or (and (ordinary-float? e+) (not (ordinary-float? e-))))
       -64]
      [(or (and (not (ordinary-float? e+)) (ordinary-float? e-)))
       +64]
      [#t
       0.0]))
   (length egood)))
