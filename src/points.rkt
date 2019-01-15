#lang racket

(require math/flonum math/bigfloat)
(require "float.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt" "range-analysis.rkt" "biginterval.rkt")

(provide *pcontext* in-pcontext mk-pcontext pcontext?
         prepare-points sampling-method
         errors errors-score sort-context-on-expr
         oracle-error baseline-error oracle-error-idx)

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
  (-> (non-empty-listof (listof any/c)) (non-empty-listof real?) pcontext?)
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

(define (make-exacts-walkup prog pts precondition)
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
      (let ([curr (map (compose ->flonum f) pts)]
            [good? (map pre pts)])
        (if (and prev (andmap (λ (good? old new) (or (not good?) (=-or-nan? old new))) good? prev curr))
            (map (λ (good? x) (if good? x +nan.0)) good? curr)
            (loop (+ prec (*precision-step*)) curr))))))

; warning: this will start at whatever precision exacts happens to be at
(define (make-exacts-halfpoints prog pts precondition)
  (define n (length pts))
  (let loop ([nth (floor (/ n 16))])
    (if (< nth 2)
        (begin
          (debug #:from 'points #:depth 4
                 "Computing exacts for" n "points")
          (make-exacts-walkup prog pts precondition))
        (begin
          (debug #:from 'points #:depth 4
                 "Computing exacts on every" nth "of" n
                 "points to ramp up precision")
          (make-exacts-walkup prog (select-every nth pts) precondition)
          (loop (floor (/ nth 2)))))))

(define (supported-ival-expr? expr)
  (match expr
    [(list op args ...)
     (and (operator-info op 'ival) (andmap supported-ival-expr? args))]
    [(or (? variable?) (? constant?)) true]))

(module+ test
  (require "formats/test.rkt")
  (require racket/runtime-path)
  (define-runtime-path benchmarks "../bench/")
  (define exprs
    (let ([tests (load-tests benchmarks)])
      (append (map test-input tests) (map test-precondition tests))))
  (define unsup-count (count (compose not supported-ival-expr?) exprs))
  (eprintf "-> ~a benchmarks still not supported by the biginterval sampler.\n" unsup-count)
  (check <= unsup-count 8))

(define ival-warnings (make-parameter '()))

(define (pre-logger dict prog)
  (match-lambda*
   [`(exit ,prec ,pt)
    (hash-update! dict (list 'pre 'exit prec) (curry + 1) 0)
    (when (= (hash-ref dict (list 'pre 'exit prec)) 1)
      (eprintf "Warning: could not determine a ground truth for precondition\n")
      (for ([var (program-variables prog)] [val pt])
        (eprintf "  ~a = ~a\n" var val))
      (eprintf "See <https://herbie.uwplse.org/doc/~a/faq.html#mpfr-prec-limit> for more info.\n" *herbie-version*))]
   [`(sampled ,prec ,in #f)
    (hash-update! dict (list 'pre 'false prec) (curry + 1) 0)]
   [`(sampled ,prec ,in #t)
    (hash-update! dict (list 'pre 'true prec) (curry + 1) 0)]
   [`(nan ,prec ,in)
    (hash-update! dict (list 'pre 'nan prec) (curry + 1) 0)]))

(define (body-logger dict prog)
  (match-lambda*
   [`(exit ,prec ,pt)
    (hash-update! dict (list 'body 'exit prec) (curry + 1) 0)
    (when (= (hash-ref dict (list 'body 'exit prec)) 1)
      (eprintf "Warning: could not determine a ground truth for program body\n")
      (for ([var (program-variables prog)] [val pt])
        (eprintf "  ~a = ~a\n" var val))
      (eprintf "See <https://herbie.uwplse.org/doc/~a/faq.html#mpfr-prec-limit> for more info.\n" *herbie-version*))]
   [`(sampled ,prec ,in ,out)
    (hash-update! dict (list 'body 'real prec) (curry + 1) 0)]
   [`(nan ,prec ,in)
    (hash-update! dict (list 'body 'nan prec) (curry + 1) 0)]))

(define (ival-eval fn pt #:precision [precision 80] #:log [log! void])
  (let loop ([precision precision])
    (parameterize ([bf-precision precision])
      (if (> precision (*max-mpfr-prec*))
          (begin (log! 'exit precision pt) +nan.0)
          (match-let* ([(ival lo hi err? err) (fn pt)] [lo* (->flonum lo)] [hi* (->flonum hi)])
            (cond
             [err
              (log! 'nan precision pt)
              +nan.0]
             [(and (not err?) (or (equal? lo* hi*) (and (equal? lo* -0.0) (equal? hi* +0.0))))
              (log! 'sampled precision pt hi*)
              hi*]
             [else
              (loop (inexact->exact (round (* precision 2))))]))))))

(define (make-exacts-intervals prog pts precondition #:log [log #f])
  (define pre-fn (eval-prog `(λ ,(program-variables prog) ,precondition) 'ival))
  (define body-fn (eval-prog prog 'ival))
  (for/list ([pt pts])
    (if (ival-eval pre-fn pt #:log (if log (pre-logger log `(λ ,(program-variables prog) ,precondition)) void))
        (ival-eval body-fn pt #:log (if log (body-logger log prog) void))
        +nan.0)))

(define (make-exacts prog pts precondition #:log [log #f])
  (if (and (supported-ival-expr? precondition) (supported-ival-expr? (program-body prog)))
      (make-exacts-intervals prog pts precondition #:log log)
      (make-exacts-halfpoints prog pts precondition)))

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

(define (prepare-points-ranges prog precondition range-table #:log [log #f])
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
      (define exs1 (make-exacts prog pts1 precondition #:log log))
      (debug #:from 'points #:depth 4
             "Filtering points with unrepresentable outputs")
      (define-values (pts* exs*) (filter-p&e pts1 exs1))
      ;; keep iterating till we get at least *num-points*
      (loop (append pts* pts) (append exs* exs) (+ 1 num-loops))])))

(define (prepare-points prog precondition #:log [log #f])
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"

  (match (extract-sampled-points (program-variables prog) precondition)
   [(? list? sampled-pts)
    (when log (hash-set! log 'presampled (length sampled-pts)))
    (mk-pcontext sampled-pts (make-exacts prog sampled-pts 'TRUE #:log log))]
   [#f
    (define range-table (condition->range-table precondition))
    (for ([var (program-variables prog)]
          #:unless (range-table-ref range-table var))
      (raise-herbie-error "No valid values of variable ~a" var
                          #:url "faq.html#no-valid-values"))
    (prepare-points-ranges prog precondition range-table #:log log)]))

(define (sampling-method prog precondition)
  (cond
   [(extract-sampled-points (program-variables prog) precondition)
    'sampled]
   [(and (supported-ival-expr? precondition) (supported-ival-expr? (program-body prog)))
    'intervals]
   [else
    'halfpoints]))

(define (eval-errors eval-fn pcontext)
  (define max-ulps (expt 2 (*bit-width*)))
  (for/list ([(point exact) (in-pcontext pcontext)])
    (define out (eval-fn point))
    (add1
      (if (real? out)
        (abs (ulp-difference out exact))
        max-ulps))))

(define (point-error inexact exact)
  (add1
    (if (real? inexact)
      (abs (ulp-difference inexact exact))
      (expt 2 (*bit-width*)))))

(define (oracle-error-idx alt-bodies points exacts)
  (for/list ([point points] [exact exacts])
    (list point (argmin (λ (i) (point-error ((list-ref alt-bodies i) point) exact)) (range (length alt-bodies))))))

(define (oracle-error alt-bodies pcontext)
  (for/list ([(point exact) (in-pcontext pcontext)])
    (argmin identity (map (λ (alt) (point-error (alt point) exact)) alt-bodies))))

(define (baseline-error alt-bodies pcontext newpcontext)
  (define baseline (argmin (λ (alt) (errors-score (eval-errors alt pcontext))) alt-bodies))
  (eval-errors baseline newpcontext))

(define (errors prog pcontext)
  (eval-errors (eval-prog prog 'fl) pcontext))

(define (errors-score e)
  (let-values ([(reals unreals) (partition ordinary-value? e)])
    (if (flag-set? 'reduce 'avg-error)
        (/ (+ (apply + (map ulps->bits reals))
              (* (*bit-width*) (length unreals)))
           (length e))
        (apply max (map ulps->bits reals)))))
