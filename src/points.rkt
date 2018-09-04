#lang racket

(require math/flonum)
(require math/bigfloat)
(require "float.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt"
         "range-analysis.rkt" "syntax/softposit.rkt")

(provide *pcontext* in-pcontext mk-pcontext pcontext?
         prepare-points
         errors errors-score sort-context-on-expr)

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

(define/contract (mk-pcontext points exacts)
  ;; TODO: The second argumnet type should be any of the possible input types,
  ;; not just any type in general (maybe the first argument too?)
  (-> (non-empty-listof (listof any/c)) (non-empty-listof any/c) pcontext?)
  (pcontext (list->vector points) (list->vector exacts)))

(define (sort-context-on-expr context expr variables)
  (let ([p&e (sort (for/list ([(pt ex) (in-pcontext context)]) (cons pt ex))
		   </total #:key (compose (eval-prog `(λ ,variables ,expr) 'fl) car))])
    (mk-pcontext (map car p&e) (map cdr p&e))))

(define (select-every skip l)
  (let loop ([l l] [count skip])
    (cond
     [(null? l) '()]
     [(= count 0)
      (cons (car l) (loop (cdr l) skip))]
     [else
      (loop (cdr l) (- count 1))])))

(define (make-exacts* prog pts precondition)
  (let ([f (eval-prog prog 'bf)] [n (length pts)]
        [pre (eval-prog `(λ ,(program-variables prog) ,precondition) 'bf)])
    (let loop ([prec (max 64 (- (bf-precision) (*precision-step*)))]
               [prev #f])
      (when (> prec (*max-mpfr-prec*))
        (raise-herbie-error "Exceeded MPFR precision limit."
                            #:url "faq.html#mpfr-prec-limit"))
      (debug #:from 'points #:depth 4
             "Setting MPFR precision to" prec)
      (bf-precision prec)
      (let ([curr (map f pts)]
            [good? (map pre pts)])
        (if (and prev (andmap (λ (good? old new) (or (not good?) (=-or-nan? old new))) good? prev curr))
            (map (λ (good? x) (if good? x +nan.0)) good? curr)
            (loop (+ prec (*precision-step*)) curr))))))

; warning: this will start at whatever precision exacts happens to be at
(define (make-exacts prog pts precondition)
  (define n (length pts))
  (let loop ([nth (floor (/ n 16))])
    (if (< nth 2)
        (begin
          (debug #:from 'points #:depth 4
                 "Computing exacts for" n "points")
          (make-exacts* prog pts precondition))
        (begin
          (debug #:from 'points #:depth 4
                 "Computing exacts on every" nth "of" n
                 "points to ramp up precision")
          (make-exacts* prog (select-every nth pts) precondition)
          (loop (floor (/ nth 2)))))))

(define (filter-p&e pts exacts)
  "Take only the points and exacts for which the exact value and the point coords are ordinary"
  (for/lists (ps es)
      ([pt pts] [ex exacts] #:when (ordinary-value? ex) #:when (andmap ordinary-value? pt))
    (values pt ex)))

(define (extract-sampled-points allvars precondition)
  (match precondition
    [`(or (and (== ,(? variable? varss) ,(? constant? valss)) ...) ...)
     (define pts
       (for/list ([vars varss] [vals valss])
         (if (set=? vars allvars)
             (map (curry dict-ref (map cons vars vals)) allvars)
             #f)))
     (and (andmap identity pts) pts)]
    [_ #f]))

; These definitions in place, we finally generate the points.

(define (prepare-points-ranges prog precondition range-table)
  (define (sample)
    (for/list ([var (program-variables prog)])
      (match (range-table-ref range-table var)
        ;; TODO does the single-interval case ever happen?
        [(interval lo hi lo? hi?)
         (sample-bounded lo hi #:left-closed? lo? #:right-closed? hi?)]
        [(list (? interval? ivals) ...)
         (sample-multi-bounded ivals)])))

  (let loop ([pts '()] [exs '()] [num-loops 0])
    (define npts (length pts))
    (cond
     [(> num-loops 200)
      (raise-herbie-error "Cannot sample enough valid points."
                          #:url "faq.html#sample-valid-points")]
     [(>= npts (*num-points*))
      (debug #:from 'points #:tag 'exit #:depth 4
             "Sampled" npts "points with exact outputs")
      (mk-pcontext (take-up-to pts (*num-points*)) (take-up-to exs (*num-points*)))]
     [else
      (define num (max 4 (- (*num-points*) npts))) ; pad to avoid repeatedly trying to get last point
      (debug #:from 'points #:depth 4
             "Sampling" num "additional inputs,"
             "on iter" num-loops "have" npts "/" (*num-points*))
      (define pts1 (for/list ([i (in-range num)]) (sample)))
      (define exs1 (make-exacts prog pts1 precondition))
      (debug #:from 'points #:depth 4
             "Filtering points with unrepresentable outputs")
      (define-values (pts* exs*) (filter-p&e pts1 exs1))
      ;; keep iterating till we get at least *num-points*
      (loop (append pts* pts) (append exs* exs) (+ 1 num-loops))])))

(define (prepare-points prog precondition)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"

  (define sampled-pts (extract-sampled-points (program-variables prog) precondition))
  (define range-table (condition->range-table precondition))

  (cond
   [sampled-pts
    (mk-pcontext sampled-pts (make-exacts prog sampled-pts 'TRUE))]
   [else
    (for ([var (program-variables prog)]
          #:unless (range-table-ref range-table var))
      (raise-herbie-error "No valid values of variable ~a" var
                          #:url "faq.html#no-valid-values"))
    (prepare-points-ranges prog precondition range-table)]))

(define (errors prog pcontext)
  (let ([fn (eval-prog prog 'fl)]
	[max-ulps (expt 2 (*bit-width*))])
    (for/list ([(point exact) (in-pcontext pcontext)])
      (let ([out (fn point)])
	(add1
	 (if (or (real? out)
                 (posit8? out) (posit16? out) (posit32? out)
                 (quire8? out) (quire16? out) (quire32? out))
           (abs (ulp-difference out exact))
           max-ulps))))))

(define (errors-score e)
  (let-values ([(reals unreals) (partition ordinary-value? e)])
    (if (flag-set? 'reduce 'avg-error)
        (/ (+ (apply + (map ulps->bits reals))
              (* (*bit-width*) (length unreals)))
           (length e))
        (apply max (map ulps->bits reals)))))
