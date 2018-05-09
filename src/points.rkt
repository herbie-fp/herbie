#lang racket

(require math/flonum)
(require math/bigfloat)
(require "float.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt" "range-analysis.rkt")

(provide *pcontext* in-pcontext mk-pcontext pcontext?
         prepare-points prepare-points-period make-exacts
         errors errors-score sorted-context-list sort-context-on-expr
         random-subsample)

(module+ test
  (require rackunit))

(define (sample-bounded lo hi #:left-closed? [left-closed? #t] #:right-closed? [right-closed? #t])
  (define lo* (exact->inexact lo))
  (define hi* (exact->inexact hi))
  (cond
   [(> lo* hi*) #f]
   [(= lo* hi*)
    (if (and left-closed? right-closed?) lo* #f)]
   [(< lo* hi*)
    (define ordinal (- (flonum->ordinal hi*) (flonum->ordinal lo*)))
    (define num-bits (ceiling (/ (log ordinal) (log 2))))
    (define random-num (random-exp (inexact->exact num-bits)))
    (if (or (and (not left-closed?) (equal? 0 random-num))
            (and (not right-closed?) (equal? ordinal random-num))
            (> random-num ordinal))
      ;; Happens with p < .5 so will not loop forever
      (sample-bounded lo hi #:left-closed? left-closed? #:right-closed? right-closed?)
      (ordinal->flonum (+ (flonum->ordinal lo*) random-num)))]))

(module+ test
  (check-true (<= 1.0 (sample-bounded 1 2) 2.0))
  (let ([a (sample-bounded 1 2 #:left-closed? #f)])
    (check-true (< 1 a))
    (check-true (<= a 2)))
  (check-false (sample-bounded 1 1.0 #:left-closed? #f) "Empty interval due to left openness")
  (check-false (sample-bounded 1 1.0 #:right-closed? #f) "Empty interval due to right openness")
  (check-false (sample-bounded 1 1.0 #:left-closed? #f #:right-closed? #f)
               "Empty interval due to both-openness")
  (check-false (sample-bounded 2.0 1.0) "Interval bounds flipped"))

(define/contract (sample-multi-bounded ranges)
  (-> (listof interval?) (or/c flonum? #f))
  (define ordinal-ranges
    (for/list ([range ranges])
      (match-define (interval (app exact->inexact lo) (app exact->inexact hi) lo? hi?) range)
      (list (flonum->ordinal lo) (flonum->ordinal hi) lo? hi?)))

  (define (points-in-range lo hi lo? hi?)
    ;; The `max` handles the case lo > hi and similar
    (max 0 (- hi lo (if lo? 0 1) (if hi? -1 0))))

  (define total-weight
    (apply +
           (for/list ([range ordinal-ranges])
             (match-define (list lo hi lo? hi?) range)
             (points-in-range lo hi lo? hi?))))

  (match total-weight
   [0 #f]
   [_
    (define num-bits (ceiling (/ (log total-weight) (log 2))))
    (define sample
      (let loop ()
        (define sample (random-exp (inexact->exact num-bits)))
        (if (< sample total-weight) sample (loop))))
    (let loop ([sample sample] [ordinal-ranges ordinal-ranges])
      ;; The `(car)` is guaranteed to succeed by the construction of `sample`
      (match-define (list lo hi lo? hi?) (car ordinal-ranges))
      (if (< sample (points-in-range lo hi lo? hi?))
          (ordinal->flonum (+ lo (if lo? 0 1) sample))
          (loop (- sample (points-in-range lo hi lo? hi?)) (cdr ordinal-ranges))))]))

(module+ test
  (check-true (set-member? '(0.0 1.0) (sample-multi-bounded (list (interval 0 0 #t #t) (interval 1 1 #t #t)))))
  (check-false (sample-multi-bounded (list (interval 0 0 #t #f) (interval 1 1 #f #t)))))

(define *pcontext* (make-parameter #f))

(struct pcontext (points exacts))

(define (in-pcontext context)
  (in-parallel (in-vector (pcontext-points context)) (in-vector (pcontext-exacts context))))

(define (mk-pcontext points exacts)
  (pcontext (if (list? points)
		(begin (assert (not (null? points)))
		       (list->vector points))
		(begin (assert (not (= 0 (vector-length points))))
		       points))
	    (if (list? exacts)
		(begin (assert (not (null? exacts)))
		       (list->vector exacts))
		(begin (assert (not (= 0 (vector-length exacts))))
		       exacts))))

(define (random-subsample pcontext n)
  (let*-values ([(old-points) (pcontext-points pcontext)]
                [(old-exacts) (pcontext-exacts pcontext)]
                [(points exacts)
                 (for/lists (points exacts)
		     ([i (in-range n)])
		   (let ([idx (random (vector-length old-points))])
		     (values (vector-ref old-points idx)
			     (vector-ref old-exacts idx))))])
    (mk-pcontext points exacts)))

(define (sorted-context-list context vidx)
  (let ([p&e (sort (for/list ([(pt ex) (in-pcontext context)]) (cons pt ex))
		   </total #:key (compose (curryr list-ref vidx) car))])
    (list (map car p&e) (map cdr p&e))))

(define (sort-context-on-expr context expr variables)
  (let ([p&e (sort (for/list ([(pt ex) (in-pcontext context)]) (cons pt ex))
		   </total #:key (compose (eval-prog `(λ ,variables ,expr) 'fl) car))])
    (list (map car p&e) (map cdr p&e))))

(define (make-period-points num periods)
  (let ([points-per-dim (floor (exp (/ (log num) (length periods))))])
    (apply cartesian-product
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

(define exacts-precs
  (vector-immutable 64 128 256 512 768 1024 1536 2048 3072 4096 6144 8192 12288 16384))

; start at 1, first iter of first call to make-exacts will drop briefly to 0
(define exacts-prec-idx 1)
(define (reset-exacts-prec-idx)
  (set! exacts-prec-idx 1))

(define (bump-exacts-prec)
  (assert (< -1 exacts-prec-idx))
  (set! exacts-prec-idx (+ exacts-prec-idx 1))
  (when (>= exacts-prec-idx (vector-length exacts-precs))
      (raise-herbie-error "Exceeded MPFR precision limit."
                          #:url "faq.html#mpfr-prec-limit"))
  (let ([p (vector-ref exacts-precs exacts-prec-idx)])
    (debug #:from 'points #:depth 4 "Bumping precision to" p)
    (bf-precision p)))

(define (dump-exacts-prec)
  (assert (< 0 exacts-prec-idx (vector-length exacts-precs)))
  (set! exacts-prec-idx (- exacts-prec-idx 1))
  (let ([p (vector-ref exacts-precs exacts-prec-idx)])
    (debug #:from 'points #:depth 4 "Dumping precision to" p)
    (bf-precision p)))

(define (make-exacts* prog pts precondition)
  (let ([f (eval-prog prog 'bf)] [n (length pts)]
        [pre (eval-prog `(λ ,(program-variables prog) ,precondition) 'bf)])
    ; drop a level of prec; iters bump till 64-bit prefixes converge
    (dump-exacts-prec)
    (let loop ([prev #f])
      (let ([curr (map f pts)]
            [good? (map pre pts)])
        (if (and prev (andmap (λ (good? old new) (or (not good?) (=-or-nan? old new))) good? prev curr))
            (map (λ (good? x) (if good? x +nan.0)) good? curr)
            (begin (bump-exacts-prec)
                   (loop curr)))))))

; warning: this will start at whatever precision exacts happens to be at
(define (make-exacts prog pts precondition)
  (define n (length pts))
  (let loop ([n* 16]) ; 16 is arbitrary; *num-points* should be n* times a power of 2
    (cond
     [(>= n* n)
      (make-exacts* prog pts precondition)]
     [else
      (make-exacts* prog (select-every (round (/ n n*)) pts) precondition)
      (loop (* n* 2))])))

(define (filter-points pts exacts)
  "Take only the points for which the exact value is normal, and the point is normal"
  (reap (sow)
    (for ([pt pts] [exact exacts])
      (when (and (ordinary-value? exact) (andmap ordinary-value? pt))
        (sow pt)))))

(define (filter-exacts pts exacts)
  "Take only the exacts for which the exact value is normal, and the point is normal"
  (reap (sow)
    (for ([pt pts] [exact exacts])
      (when (and (ordinary-value? exact) (andmap ordinary-value? pt))
	(sow exact)))))

; These definitions in place, we finally generate the points.

(define (prepare-points prog precondition)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"

  (define range-table (condition->range-table precondition))
  (reset-exacts-prec-idx)

  (define samplers
    (for/list ([var (program-variables prog)])
      (match (range-table-ref range-table var)
        [#f
         (raise-herbie-error "No valid values of variable ~a" var #:url "faq.html#no-valid-values")]
        [(interval lo hi lo? hi?)
         (λ () (sample-bounded lo hi #:left-closed? lo? #:right-closed? hi?))]
        [(list (? interval? ivals) ...)
         (λ () (sample-multi-bounded ivals))])))

  (let loop ([pts '()] [exs '()] [num-loops 0])
    (cond [(> num-loops 200)
           (raise-herbie-error "Cannot sample enough valid points."
                               #:url "faq.html#sample-valid-points")]
          [(>= (length pts) (*num-points*))
           (mk-pcontext (take pts (*num-points*))
                        (take exs (*num-points*)))]
          [#t
           (let* ([n-pts (length pts)]
                  ; add 10 here to avoid repeatedly trying to get last point
                  [num (+ 10 (- (*num-points*) n-pts))]
                  [_ (debug #:from 'points #:depth 4
                            "Sampling" num "additional inputs,"
                            "have" n-pts "/" (*num-points*))]
                  [pts1 (for/list ([n (in-range num)])
                          (for/list ([var (program-variables prog)]
                                     [sampler samplers])
                            (sampler)))]
                  [_ (debug #:from 'points #:depth 4
                            "Computing" num "additional correct outputs")]
                  [exs1 (make-exacts prog pts1 precondition)]
                  [_ (debug #:from 'points #:depth 4
                            "Filtering points with unrepresentable outputs")]
                  [pts* (filter-points pts1 exs1)]
                  [exs* (filter-exacts pts1 exs1)])
            ; keep iterating till we get at least *num-points*
            (loop (append pts* pts) (append exs* exs) (+ 1 num-loops)))])))

(define (prepare-points-period prog periods)
  (let* ([pts (make-period-points (*num-points*) periods)]
	 [exacts (make-exacts prog pts)]
	 [pts* (filter-points pts exacts)]
	 [exacts* (filter-exacts pts exacts)])
    (mk-pcontext pts* exacts*)))

(define (errors prog pcontext)
  (let ([fn (eval-prog prog 'fl)]
	[max-ulps (expt 2 (*bit-width*))])
    (for/list ([(point exact) (in-pcontext pcontext)])
      (let ([out (fn point)])
	(add1
	 (if (real? out)
	     (abs (ulp-difference out exact))
	     max-ulps))))))

(define (errors-score e)
  (let-values ([(reals unreals) (partition ordinary-value? e)])
    (if (flag-set? 'reduce 'avg-error)
        (/ (+ (apply + (map ulps->bits reals))
              (* (*bit-width*) (length unreals)))
           (length e))
        (apply max (map ulps->bits reals)))))
