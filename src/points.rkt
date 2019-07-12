#lang racket

(require math/flonum)
(require math/bigfloat)
(require "float.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt" "timeline.rkt"
         "range-analysis.rkt" "biginterval.rkt" "interface.rkt")

(provide *pcontext* in-pcontext mk-pcontext pcontext?
         prepare-points errors errors-score
         oracle-error baseline-error oracle-error-idx)

(module+ test (require rackunit))
(module+ internals (provide make-sampler ival-eval))

(define/contract (sample-multi-bounded ranges)
  (-> (listof interval?) (or/c flonum? single-flonum? #f))
  (define repr (get-representation (if (flag-set? 'precision 'double) 'binary64 'binary32)))
  (define ->ordinal (representation-repr->ordinal repr))
  (define <-ordinal (representation-ordinal->repr repr))
  (define <-exact (representation-exact->repr repr))

  (define ordinal-ranges
    (for/list ([range ranges])
      (match-define (interval (app <-exact lo) (app <-exact hi) lo? hi?) range)
      (cons (+ (->ordinal lo) (if lo? 0 1)) (+ (->ordinal hi) (if hi? 1 0)))))

  (<-ordinal (apply random-ranges ordinal-ranges)))

(module+ test
  (check-true (set-member? '(0.0 1.0) (sample-multi-bounded (list (interval 0 0 #t #t) (interval 1 1 #t #t)))))
  (check-exn
   exn:fail?
   (λ () (sample-multi-bounded (list (interval 0 0 #t #f) (interval 1 1 #f #t))))))

(define *pcontext* (make-parameter #f))

(struct pcontext (points exacts))

(define (in-pcontext context)
  (in-parallel (in-vector (pcontext-points context)) (in-vector (pcontext-exacts context))))

(define/contract (mk-pcontext points exacts)
  ;; TODO: The second argument type should be any of the possible input types,
  ;; not just any type in general (maybe the first argument too?)
  (-> (non-empty-listof (listof any/c)) (non-empty-listof any/c) pcontext?)
  (pcontext (list->vector points) (list->vector exacts)))

(module+ test
  (require "formats/test.rkt")
  (require racket/runtime-path)
  (define-runtime-path benchmarks "../bench/")
  (define exprs
    (let ([tests (expect-warning 'duplicate-names (λ () (load-tests benchmarks)))])
      (append (map test-input tests) (map test-precondition tests))))
  (define unsup-count (count (compose not (curryr expr-supports? 'ival)) exprs))
  (eprintf "-> ~a benchmarks still not supported by the biginterval sampler.\n" unsup-count)
  (check <= unsup-count 50))

(define (point-logger name dict prog)
  (define start (current-inexact-milliseconds))
  (define (log! . args)
    (define key
      (match args
        [`(exit ,prec ,pt)
         (define key (list name 'exit prec))
         (warn 'ground-truth #:url "faq.html#ground-truth"
               "could not determine a ground truth for program ~a" name
               #:extra (for/list ([var (program-variables prog)] [val pt])
                         (format "~a = ~a" var val)))
         key]
        [`(sampled ,prec ,pt #f) (list name 'false prec)]
        [`(sampled ,prec ,pt #t) (list name 'true prec)]
        [`(sampled ,prec ,pt ,_) (list name 'valid prec)]
        [`(nan ,prec ,pt) (list name 'nan prec)]))
    (define dt (- (current-inexact-milliseconds) start))
    (hash-update! dict key (λ (x) (cons (+ (car x) 1) (+ (cdr x) dt))) (cons 0 0)))
  (if dict log! void))

(define (ival-eval fn pt prec #:precision [precision 80] #:log [log! void])
  (define <-bf (representation-bf->repr (get-representation prec)))
  (let loop ([precision precision])
    (parameterize ([bf-precision precision])
      (if (> precision (*max-mpfr-prec*))
          (begin (log! 'exit precision pt) +nan.0)
          (match-let* ([(ival lo hi err? err) (fn pt)] [lo* (<-bf lo)] [hi* (<-bf hi)])
            (cond
             [err
              (log! 'nan precision pt)
              +nan.0]
             [(and (not err?) (or (equal? lo* hi*)
                                  (and (equal? lo* -0.0) (equal? hi* +0.0))
                                  (and (equal? lo* -0.0f0) (equal? hi* +0.0f0))))
              (log! 'sampled precision pt hi*)
              hi*]
             [else
              (loop (inexact->exact (round (* precision 2))))]))))))

; These definitions in place, we finally generate the points.

(define (make-sampler precondition)
  (define range-table (condition->range-table (program-body precondition)))
  (for ([var (program-variables precondition)]
        #:when (null? (range-table-ref range-table var)))
    (raise-herbie-error "No valid values of variable ~a" var
                        #:url "faq.html#no-valid-values"))
  (λ ()
    (map (compose sample-multi-bounded (curry range-table-ref range-table))
         (program-variables precondition))))

(define (prepare-points-intervals prog precondition precision)
  (timeline-log! 'method 'intervals)
  (define log (make-hash))
  (timeline-log! 'outcomes log)

  (define pre-prog `(λ ,(program-variables prog) ,precondition))
  (define sampler (make-sampler pre-prog))

  (define pre-fn (eval-prog pre-prog 'ival))
  (define body-fn (eval-prog prog 'ival))

  (define-values (points exacts)
    (let loop ([sampled 0] [skipped 0] [points '()] [exacts '()])
      (define pt (sampler))

      (define pre
        (or (equal? precondition 'TRUE)
            (ival-eval pre-fn pt 'bool #:log (point-logger 'pre log pre-prog))))

      (define ex
        (and pre (ival-eval body-fn pt precision #:log (point-logger 'body log prog))))

      (define repr (infer-representation (car pt)))
      (cond
       [(and (andmap (curryr ordinary-value? repr) pt) pre (ordinary-value? ex repr))
        (if (>= sampled (- (*num-points*) 1))
            (values points exacts)
            (loop (+ 1 sampled) 0 (cons pt points) (cons ex exacts)))]
       [else
        (unless (< skipped (- (*max-skipped-points*) 1))
          (raise-herbie-error "Cannot sample enough valid points."
                              #:url "faq.html#sample-valid-points"))
        (loop sampled (+ 1 skipped) points exacts)])))

  (mk-pcontext points exacts))

(define (prepare-points prog precondition precision)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"
  (if (and (expr-supports? precondition 'ival) (expr-supports? (program-body prog) 'ival))
    (prepare-points-intervals prog precondition precision)
    (prepare-points-halfpoints prog precondition precision)))

#;(define (prepare-points prog precondition precision)
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
    (prepare-points-halfpoints prog precondition precision range-table)]))


(define (point-error out exact)
  (define repr (infer-double-representation out exact))
  (if (ordinary-value? out repr)
      (+ 1 (abs (ulp-difference out exact repr)))
      (+ 1 (expt 2 (*bit-width*)))))

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

(define (errors-score e)
  (define repr (get-representation 'binary64))
  (let-values ([(reals unreals) (partition (curryr ordinary-value? repr) e)])
    (if (flag-set? 'reduce 'avg-error)
        (/ (+ (apply + (map ulps->bits reals))
              (* (*bit-width*) (length unreals)))
           (length e))
        (apply max (map ulps->bits reals)))))

(define (errors prog pcontext)
  (define fn (eval-prog prog 'fl))
  (for/list ([(point exact) (in-pcontext pcontext)])
    (with-handlers ([exn:fail? (λ (e) (eprintf "Error when evaluating ~a on ~a\n" prog point) (raise e))])
      (point-error (fn point) exact))))

;; Old, halfpoints method of sampling points

(define (select-every skip l)
  (let loop ([l l] [count skip])
    (cond
     [(null? l) '()]
     [(= count 0)
      (cons (car l) (loop (cdr l) skip))]
     [else
      (loop (cdr l) (- count 1))])))

(define (make-exacts-walkup prog pts precondition prec)
  (define <-bf (representation-bf->repr (get-representation prec)))
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
      (let ([curr (map (compose <-bf f) pts)]
            [good? (map pre pts)])
        (if (and prev (andmap (λ (good? old new) (or (not good?) (=-or-nan? old new))) good? prev curr))
            (map (λ (good? x) (if good? x +nan.0)) good? curr)
            (loop (+ prec (*precision-step*)) curr))))))

; warning: this will start at whatever precision exacts happens to be at
(define (make-exacts-halfpoints prog pts precondition prec)
  (define n (length pts))
  (let loop ([nth (floor (/ n 16))])
    (if (< nth 2)
        (begin
          (debug #:from 'points #:depth 4
                 "Computing exacts for" n "points")
          (make-exacts-walkup prog pts precondition prec))
        (begin
          (debug #:from 'points #:depth 4
                 "Computing exacts on every" nth "of" n
                 "points to ramp up precision")
          (make-exacts-walkup prog (select-every nth pts) precondition prec)
          (loop (floor (/ nth 2)))))))

(define (filter-p&e pts exacts)
  "Take only the points and exacts for which the exact value and the point coords are ordinary"
  (define repr (get-representation (*output-prec*)))
  (for/lists (ps es)
      ([pt pts] [ex exacts] #:when (ordinary-value? ex repr)
                            #:when (andmap (curryr ordinary-value? repr) pt))
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

(define (filter-valid-points prog precondition points)
  (define f (eval-prog (list 'λ (program-variables prog) precondition) 'fl))
  (filter f points))

;; This is the obsolete version for the "halfpoint" method
(define (prepare-points-halfpoints prog precondition precision)
  (timeline-log! 'method 'halfpoints)
  (define sample (make-sampler `(λ ,(program-variables prog) ,precondition)))

  (let loop ([pts '()] [exs '()] [num-loops 0])
    (define npts (length pts))
    (cond
     [(> num-loops 200)
      (raise-herbie-error "Cannot sample enough valid points."
                          #:url "faq.html#sample-valid-points")]
     [(>= npts (*num-points*))
      (debug #:from 'points #:depth 4 "Sampled" npts "points with exact outputs")
      (mk-pcontext (take-up-to pts (*num-points*)) (take-up-to exs (*num-points*)))]
     [else
      (define num-vars (length (program-variables prog)))
      (define num (max 4 (- (*num-points*) npts))) ; pad to avoid repeatedly trying to get last point
      (debug #:from 'points #:depth 4
             "Sampling" num "additional inputs,"
             "on iter" num-loops "have" npts "/" (*num-points*))
      (define sampler
        (if (set-member? '(binary32 binary64) precision)
            sample
            (λ () (for/list ([var (program-variables prog)])
                    (random-generate (get-representation precision))))))
      (define pts1 (for/list ([n (in-range num)]) (sampler)))
      (define exs1 (make-exacts-halfpoints prog pts1 precondition precision))
      (debug #:from 'points #:depth 4
             "Filtering points with unrepresentable outputs")
      (define-values (pts* exs*) (filter-p&e pts1 exs1))
      (loop (append pts* pts) (append exs* exs) (+ 1 num-loops))])))
