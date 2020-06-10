#lang racket

(require math/bigfloat rival math/base)
(require "float.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt" "timeline.rkt"
         "interface.rkt" "sampling.rkt")

(provide *pcontext* in-pcontext mk-pcontext pcontext? prepare-points
         errors batch-errors errors-score
         oracle-error baseline-error oracle-error-idx)

(module+ test (require rackunit))
(module+ internals (provide ival-eval))

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
  (require "syntax/read.rkt")
  (require racket/runtime-path)
  (define-runtime-path benchmarks "../bench/")
  (define exprs
    (let ([tests (expect-warning 'duplicate-names (λ () (load-tests benchmarks)))])
      (append (map test-input tests) (map test-precondition tests))))
  (check = (count (compose not (curryr expr-supports? 'ival)) exprs) 0))

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
        [`(overflowed ,prec ,pt)
         (list name 'overflowed prec)]
        [`(sampled ,prec ,pt #f) (list name 'false prec)]
        [`(sampled ,prec ,pt #t) (list name 'true prec)]
        [`(sampled ,prec ,pt ,_) (list name 'valid prec)]
        [`(infinite ,prec ,pt ,_) (list name 'invalid prec)]
        [`(nan ,prec ,pt) (list name 'nan prec)]))
    (define dt (- (current-inexact-milliseconds) start))
    (hash-update! dict key (λ (x) (cons (+ (car x) 1) (+ (cdr x) dt))) (cons 0 0)))
  (if dict log! void))

(define (ival-eval fn pt repr #:precision [precision 80] #:log [log! void])
  (define <-bf (representation-bf->repr repr))
  (let loop ([precision precision])
    (match-define (ival out (app <-bf lo) (app <-bf hi))
                  (parameterize ([bf-precision precision]) (apply fn pt)))
    (define lo! (ival-lo-fixed? out))
    (define hi! (ival-hi-fixed? out))
    (cond
     [(ival-err out)
      (log! 'nan precision pt)
      +nan.0]
     [(and (not (ival-err? out))
           (or (equal? lo hi) (and (number? lo) (= lo hi)))) ; 0.0 and -0.0
      (if (ordinary-value? hi repr)
          (log! 'sampled precision pt hi)
          (log! 'infinite precision pt hi))
      hi]
     [(and lo! hi!)
      (log! 'overflowed precision pt)
      +nan.0]
     [(or (and lo! (bigfloat? (ival-lo out)) (bfinfinite? (ival-lo out)))
          (and hi! (bigfloat? (ival-hi out)) (bfinfinite? (ival-hi out))))
      ;; We never sample infinite points anyway
      (log! 'overflowed precision pt)
      +nan.0]
     [else
      (define precision* (exact-floor (* precision 2)))
      (if (> precision* (*max-mpfr-prec*))
          (begin (log! 'exit precision pt) +nan.0)
          (loop precision*))])))

(define (prepare-points-intervals prog precondition repr)
  (timeline-log! 'method 'intervals)
  (define log (make-hash))
  (timeline-log! 'outcomes log)

  (define pre-prog `(λ ,(program-variables prog) ,precondition))
  (define sampler (make-sampler repr pre-prog prog))

  (define pre-fn (eval-prog pre-prog 'ival repr))
  (define body-fn (eval-prog prog 'ival repr))

  (define-values (points exacts)
    (let loop ([sampled 0] [skipped 0] [points '()] [exacts '()])
      (define pt (sampler))

      (define pre
        (or (equal? precondition 'TRUE)
            (ival-eval pre-fn pt (get-representation 'bool) #:log (point-logger 'pre log pre-prog))))

      (define ex
        (and pre (ival-eval body-fn pt repr #:log (point-logger 'body log prog))))

      (define success
        ;; +nan.0 is the "error" return code for ival-eval
        (and (not (equal? pre +nan.0)) (not (equal? ex +nan.0))))

      (cond
       [(and success (andmap (curryr ordinary-value? repr) pt) pre (ordinary-value? ex repr))
        (if (>= sampled (- (*num-points*) 1))
            (values points exacts)
            (loop (+ 1 sampled) 0 (cons pt points) (cons ex exacts)))]
       [else
        (unless (< skipped (- (*max-skipped-points*) 1))
          (raise-herbie-error "Cannot sample enough valid points."
                              #:url "faq.html#sample-valid-points"))
        (loop sampled (+ 1 skipped) points exacts)])))

  (mk-pcontext points exacts))

(define (prepare-points prog precondition repr)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"
  (if (and (expr-supports? precondition 'ival) (expr-supports? (program-body prog) 'ival))
    (prepare-points-intervals prog precondition repr)
    (prepare-points-halfpoints prog precondition repr)))

(define (point-error out exact repr)
  (if (ordinary-value? out repr)
      (ulp-difference out exact repr)
      (+ 1 (expt 2 (representation-total-bits repr)))))

(define (eval-errors eval-fn pcontext repr)
  (for/list ([(point exact) (in-pcontext pcontext)])
    (point-error (apply eval-fn point) exact repr)))

(define (oracle-error-idx alt-bodies points exacts repr)
  (for/list ([point points] [exact exacts])
    (list point (argmin (λ (i) (point-error ((list-ref alt-bodies i) point) exact repr)) (range (length alt-bodies))))))

(define (oracle-error alt-bodies pcontext repr)
  (for/list ([(point exact) (in-pcontext pcontext)])
    (argmin identity (map (λ (alt) (point-error (apply alt point) exact repr)) alt-bodies))))

(define (baseline-error alt-bodies pcontext newpcontext repr)
  (define baseline (argmin (λ (alt) (errors-score (eval-errors alt pcontext repr))) alt-bodies))
  (eval-errors baseline newpcontext repr))

(define (avg . s)
  (/ (apply + s) (length s)))

(define (errors-score e)
  (apply (if (flag-set? 'reduce 'avg-error) avg max)
         (map ulps->bits e)))

(define (errors prog pcontext repr)
  (define fn (eval-prog prog 'fl repr))
  (for/list ([(point exact) (in-pcontext pcontext)])
    (with-handlers ([exn:fail? (λ (e) (eprintf "Error when evaluating ~a on ~a\n" prog point) (raise e))])
      (point-error (apply fn point) exact repr))))

(define (batch-errors progs pcontext repr)
  (define fn (batch-eval-progs progs 'fl repr))
  (for/list ([(point exact) (in-pcontext pcontext)])
    (with-handlers ([exn:fail? (λ (e) (eprintf "Error when evaluating ~a on ~a\n" progs point) (raise e))])
      (for/vector ([out (in-vector (apply fn point))])
        (point-error out exact repr)))))

;; Old, halfpoints method of sampling points

(define (select-every skip l)
  (let loop ([l l] [count skip])
    (cond
     [(null? l) '()]
     [(= count 0)
      (cons (car l) (loop (cdr l) skip))]
     [else
      (loop (cdr l) (- count 1))])))

(define ((call f) pt) (apply f pt))

(define (make-exacts-walkup prog pts precondition repr)
  (define <-bf (representation-bf->repr repr))
  (let ([f (eval-prog prog 'bf repr)] [n (length pts)]
        [pre (eval-prog `(λ ,(program-variables prog) ,precondition) 'bf repr)])
    (let loop ([prec (max 64 (- (bf-precision) (*precision-step*)))]
               [prev #f])
      (when (> prec (*max-mpfr-prec*))
        (raise-herbie-error "Exceeded MPFR precision limit."
                            #:url "faq.html#mpfr-prec-limit"))
      (debug #:from 'points #:depth 4
             "Setting MPFR precision to" prec)
      (bf-precision prec)
      (let ([curr (map (compose <-bf (call f)) pts)]
            [good? (map (call pre) pts)])
        (if (and prev (andmap (λ (good? old new) (or (not good?) (=-or-nan? old new repr))) good? prev curr))
            (map (λ (good? x) (if good? x +nan.0)) good? curr)
            (loop (+ prec (*precision-step*)) curr))))))

; warning: this will start at whatever precision exacts happens to be at
(define (make-exacts-halfpoints prog pts precondition repr)
  (define n (length pts))
  (let loop ([nth (floor (/ n 16))])
    (if (< nth 2)
        (begin
          (debug #:from 'points #:depth 4
                 "Computing exacts for" n "points")
          (make-exacts-walkup prog pts precondition repr))
        (begin
          (debug #:from 'points #:depth 4
                 "Computing exacts on every" nth "of" n
                 "points to ramp up precision")
          (make-exacts-walkup prog (select-every nth pts) precondition repr)
          (loop (floor (/ nth 2)))))))

(define (filter-p&e pts exacts)
  "Take only the points and exacts for which the exact value and the point coords are ordinary"
  (for/lists (ps es)
      ([pt pts] [ex exacts]
       #:unless (and (real? ex) (nan? ex))
       #:when (ordinary-value? ex (*output-repr*))
       #:when (andmap (curryr ordinary-value? (*output-repr*)) pt))
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

;; This is the obsolete version for the "halfpoint" method
(define (prepare-points-halfpoints prog precondition repr)
  (timeline-log! 'method 'halfpoints)
  (define sample (make-sampler repr `(λ ,(program-variables prog) ,precondition)))

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
      (define pts1 (for/list ([n (in-range num)]) (sample)))
      (define exs1 (make-exacts-halfpoints prog pts1 precondition repr))
      (debug #:from 'points #:depth 4
             "Filtering points with unrepresentable outputs")
      (define-values (pts* exs*) (filter-p&e pts1 exs1))
      (loop (append pts* pts) (append exs* exs) (+ 1 num-loops))])))
