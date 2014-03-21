#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/programs)

(provide *points* *exacts* prepare-points make-exacts
         errors errors-compare errors-difference errors-diff-score)

(define *points* (make-parameter '()))
(define *exacts* (make-parameter '()))

(define (exp->pt bucket-number bucket-width)
  "Given an exponential bucket"
  (expt 2 (- (* bucket-width (+ bucket-number (random))) 126)))

(define (list-cartesian-power lst repetitions)
  "Returns a list, each element of which is a list
   of `repetitions` elements of `lst`"

  (if (= repetitions 1)
      (map list lst)
      (let ([tails (list-cartesian-power lst (- repetitions 1))])
        (for*/list ([head lst] [tail tails])
          (cons head tail)))))

; The bucket width for a given number of dimensions
(define bucket-width-per-dim '(: 1 6 15 25 35 45))

(define (make-points dim)
  "Make a list of flonums.  The list spans a large range of values"

  (let* ([bucket-width (list-ref bucket-width-per-dim dim)]
         [num-buckets (floor (/ 253 bucket-width))]
         [bucket-indices (range 0 num-buckets)]
         [pts+ (map (curryr exp->pt bucket-width) bucket-indices)]
         [pts (append pts+ (map - pts+))])
    (list-cartesian-power pts dim)))

(bf-precision 256)

(define (make-exacts prog pts)
  "Given a list of arguments,
   produce a list of exact evaluations of a program-at those arguments
   using true arbitrary precision.  That is, we increase the bits
   available until the exact values converge.
   Not guaranteed to terminate."
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
  (let* ([pts (make-points (length (program-variables prog)))]
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

(define errors-compare-cache (make-hasheq))

(define (reasonable-error? x)
  ; TODO : Why do we need the 100% error case?
  (not (or (infinite? x) (nan? x))))

(define (errors-compare errors1 errors2)
  (map (λ (x) (cond [(< x 0) '<] [(> x 0) '>] [#t '=]))
       (errors-difference errors1 errors2)))

(define (errors-difference errors1 errors2)
  (hash-ref!
   (hash-ref! errors-compare-cache errors1 make-hasheq)
   errors2
   (λ ()
      (for/list ([error1 errors1] [error2 errors2])
        (cond
         [(and (reasonable-error? error1) (reasonable-error? error2))
          (cond
           [(and (= error1 0) (= error2 0)) 0.0]
	   ; You might think these should be -inf.0 and +inf.0,
	   ; but that lead to stupid behavior;
	   ; a least-significant-bit error versus no error
	   ; would be very heavily weighed
           [(= error1 0) (log error2)]
           [(= error2 0) (log error1)]
           [#t (/ (log (/ error1 error2)) (log 2))])]
         [(or (and (reasonable-error? error1) (not (reasonable-error? error2))))
          -inf.0]
         [(or (and (not (reasonable-error? error1)) (reasonable-error? error2)))
          +inf.0]
         [#t
          0.0]
         [#t (error "Failed to classify error1 and error2" error1 error2)])))))

(define (errors-diff-score e1 e2)
  (let ([d (errors-difference e1 e2)])
    (let*-values ([(reals infs) (partition (lambda (n) (rational? n)) d)]
		  [(positive-infs negative-infs) (partition (lambda (n) (> 0 n)) infs)])
      (+ (apply + reals)
	 (* 64 (- (length negative-infs) (length positive-infs)))))))
