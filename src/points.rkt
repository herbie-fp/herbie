#lang racket

(require math/flonum math/bigfloat)
(require "float.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt" "range-analysis.rkt" "biginterval.rkt")

(provide *pcontext* in-pcontext mk-pcontext pcontext?
         prepare-points sampling-method
         errors errors-score sort-context-on-expr
         oracle-error baseline-error oracle-error-idx)

(module+ test
  (require rackunit))

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

(define (point-logger name dict prog)
  (define start (current-inexact-milliseconds))
  (define (log! . args)
    (define key
      (match args
        [`(exit ,prec ,pt)
         (define key (list name 'exit prec))
         (unless (hash-has-key? dict key)
           (eprintf "Warning: could not determine a ground truth for program ~a\n" name)
           (for ([var (program-variables prog)] [val pt])
             (eprintf "  ~a = ~a\n" var val))
           (eprintf "See <https://herbie.uwplse.org/doc/~a/faq.html#mpfr-prec-limit> for more info.\n" *herbie-version*))
         key]
        [`(sampled ,prec ,pt #f) (list name 'false prec)]
        [`(sampled ,prec ,pt #t) (list name 'true prec)]
        [`(sampled ,prec ,pt ,_) (list name 'valid prec)]
        [`(nan ,prec ,pt) (list name 'nan prec)]))
    (define dt (- (current-inexact-milliseconds) start))
    (hash-update! dict key (λ (x) (cons (+ (car x) 1) (+ (cdr x) dt))) (cons 0 0)))
  (if dict log! void))

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

; These definitions in place, we finally generate the points.

(define (prepare-points-intervals prog precondition #:log [log #f])
  (define range-table (condition->range-table precondition))
  (for ([var (program-variables prog)]
        #:unless (range-table-ref range-table var))
    (raise-herbie-error "No valid values of variable ~a" var
                        #:url "faq.html#no-valid-values"))

  (define pre-prog `(λ ,(program-variables prog) ,precondition))
  (define pre-fn (eval-prog pre-prog 'ival))
  (define body-fn (eval-prog prog 'ival))

  (define-values (points exacts)
    (let loop ([sampled 0] [skipped 0] [points '()] [exacts '()])
      (define pt
        (map (compose sample-multi-bounded (curry range-table-ref range-table))
             (program-variables prog)))

      (define pre
        (or (equal? precondition 'TRUE)
            (ival-eval pre-fn pt #:log (point-logger 'pre log pre-prog))))

      (define ex
        (and pre (ival-eval body-fn pt #:log (point-logger 'body log prog))))

      (cond
       [(and (andmap ordinary-value? pt) pre (ordinary-value? ex))
        (if (>= sampled (- (*num-points*) 1))
            (values points exacts)
            (loop (+ 1 sampled) 0 (cons pt points) (cons ex exacts)))]
       [else
        (unless (< skipped (- (*max-skipped-points*) 1))
          (raise-herbie-error "Cannot sample enough valid points."
                              #:url "faq.html#sample-valid-points"))
        (loop sampled (+ 1 skipped) points exacts)])))

  (mk-pcontext points exacts))

(define (prepare-points prog precondition #:log [log #f])
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"
  (if (and (supported-ival-expr? precondition) (supported-ival-expr? (program-body prog)))
    (prepare-points-intervals prog precondition #:log log)
    (prepare-points-halfpoints prog precondition #:log log)))

(define (sampling-method prog precondition)
  (cond
   [(and (supported-ival-expr? precondition) (supported-ival-expr? (program-body prog)))
    'intervals]
   [else
    'halfpoints]))

(define (point-error inexact exact)
  (add1
    (if (real? inexact)
      (abs (ulp-difference inexact exact))
      (expt 2 (*bit-width*)))))

(define (eval-errors eval-fn pcontext)
  (define max-ulps (expt 2 (*bit-width*)))
  (for/list ([(point exact) (in-pcontext pcontext)])
    (point-error (eval-fn point) exact)))

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

;; Old, halfpoints method of sampling points

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
(define (make-exacts-halfpoints prog pts precondition #:log [log #f])
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

(define (filter-p&e pts exacts)
  "Take only the points and exacts for which the exact value and the point coords are ordinary"
  (for/lists (ps es)
      ([pt pts] [ex exacts] #:when (ordinary-value? ex) #:when (andmap ordinary-value? pt))
    (values pt ex)))

;; This is the obsolete version for the "halfpoint" method
(define (prepare-points-halfpoints prog precondition #:log [log #f])
  (define range-table (condition->range-table precondition))
  (for ([var (program-variables prog)]
        #:unless (range-table-ref range-table var))
    (raise-herbie-error "No valid values of variable ~a" var
                        #:url "faq.html#no-valid-values"))

  (define (sample)
    (map (compose sample-multi-bounded (curry range-table-ref range-table)) (program-variables prog)))

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
      (define exs1 (make-exacts-halfpoints prog pts1 precondition #:log log))
      (debug #:from 'points #:depth 4
             "Filtering points with unrepresentable outputs")
      (define-values (pts* exs*) (filter-p&e pts1 exs1))
      ;; keep iterating till we get at least *num-points*
      (loop (append pts* pts) (append exs* exs) (+ 1 num-loops))])))
