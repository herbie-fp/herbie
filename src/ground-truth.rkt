#lang racket

(require math/bigfloat rival math/base)
(require "float.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt" "timeline.rkt"
         "interface.rkt" "points.rkt")

(provide prepare-points)

(module+ test (require rackunit "load-plugin.rkt"))
(module+ internals (provide ival-eval))

(module+ test
  (require "syntax/read.rkt")
  (require racket/runtime-path)
  (define-runtime-path benchmarks "../bench/")
  (define exprs
    (let ([tests (load-tests benchmarks)])
      (append (map test-program tests) (map test-precondition tests))))
  (check-true (andmap (compose (curryr expr-supports? 'ival) program-body) exprs)))

(define (point-logger name prog)
  (define start (current-inexact-milliseconds))
  (define (log! . args)
    (define now (current-inexact-milliseconds))
    (match-define (list category prec)
      (match args
        [`(exit ,prec ,pt)
         (define key (list 'exit prec))
         (warn 'ground-truth #:url "faq.html#ground-truth"
               "could not determine a ground truth for program ~a" name
               #:extra (for/list ([var (program-variables prog)] [val pt])
                         (format "~a = ~a" var val)))
         key]
        [`(overflowed ,prec ,pt)
         (list 'overflowed prec)]
        [`(sampled ,prec ,pt #f) (list 'false prec)]
        [`(sampled ,prec ,pt #t) (list 'true prec)]
        [`(sampled ,prec ,pt ,_) (list 'valid prec)]
        [`(infinite ,prec ,pt ,_) (list 'invalid prec)]
        [`(nan ,prec ,pt) (list 'nan prec)]))
    (define dt (- now start))
    (timeline-push! 'outcomes (~a name) prec (~a category) dt 1)
    (set! start now))
  log!)

(define (ival-eval fn pt repr #:precision [precision 80] #:log [log! void])
  (define <-bf (representation-bf->repr repr))
  (let loop ([precision precision])
    (match-define (ival out (app <-bf lo) (app <-bf hi))
                  (parameterize ([bf-precision precision]) (apply fn pt)))
    (define lo! (ival-lo-fixed? out))
    (define hi! (ival-hi-fixed? out))
    (define lo=hi (or (equal? lo hi) (and (number? lo) (= lo hi)))) ; 0.0 and -0.0
    (cond
     [(ival-err out)
      (log! 'nan precision pt)
      +nan.0]
     [(and (not (ival-err? out)) lo=hi)
      (if (ordinary-value? hi repr)
          (log! 'sampled precision pt hi)
          (log! 'infinite precision pt hi))
      hi]
     [(and lo! hi! (not lo=hi))
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

(define (prepare-points-intervals prog precondition repr sampler)
  (timeline-push! 'method "intervals")

  (define pre-fn (eval-prog precondition 'ival repr))
  (define body-fn (eval-prog prog 'ival repr))

  (define-values (points exacts)
    (let loop ([sampled 0] [skipped 0] [points '()] [exacts '()])
      (define pt (sampler))

      (define pre
        (or (equal? (program-body precondition) '(TRUE))
            (ival-eval pre-fn pt (get-representation 'bool) #:precision (bf-precision)
                       #:log (point-logger 'pre precondition))))

      (define ex
        (and pre (ival-eval body-fn pt repr #:precision (bf-precision)
                            #:log (point-logger 'body prog))))

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
  (timeline-compact! 'outcomes)
  (mk-pcontext points exacts))

(define (prepare-points prog precondition repr sampler)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"
  (if (and (expr-supports? (program-body precondition) 'ival)
           (expr-supports? (program-body prog) 'ival))
    (prepare-points-intervals prog precondition repr sampler)
    (prepare-points-halfpoints prog precondition repr sampler)))

;; Old, halfpoints method of sampling points

(define (select-every skip l)
  (let loop ([l l] [count skip])
    (cond
     [(null? l) '()]
     [(= count 0)
      (cons (car l) (loop (cdr l) skip))]
     [else
      (loop (cdr l) (- count 1))])))

(define (make-exacts-walkup prog pts precondition repr)
  (define <-bf (representation-bf->repr repr))
  (let ([f (eval-prog prog 'bf repr)] [n (length pts)]
        [pre (eval-prog precondition 'bf repr)])
    (let loop ([prec (max 64 (- (bf-precision) (*precision-step*)))]
               [prev #f])
      (when (> prec (*max-mpfr-prec*))
        (raise-herbie-error "Exceeded MPFR precision limit."
                            #:url "faq.html#mpfr-prec-limit"))
      (debug #:from 'points #:depth 4
             "Setting MPFR precision to" prec)
      (bf-precision prec)
      (let ([curr (map (compose <-bf (curry apply f)) pts)]
            [good? (map (curry apply pre) pts)])
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
  (for/lists (ps pr es)
      ([pt pts] [ex exacts]
       #:unless (and (real? ex) (nan? ex))
       #:when (ordinary-value? ex (*output-repr*))
       #:when (andmap (curryr ordinary-value? (*output-repr*)) pt))
    (values pt ex)))


;; This is the obsolete version for the "halfpoint" method
(define (prepare-points-halfpoints prog precondition repr sampler)
  (timeline-push! 'method "halfpoints")
  (let loop ([pts '()] [exs '()] [num-loops 0])
    (define npts (length pts))
    (cond
     [(> num-loops 200)
      (raise-herbie-error "Cannot sample enough valid points."
                          #:url "faq.html#sample-valid-points")]
     [(>= npts (*num-points*))
      (debug #:from 'points #:depth 4 "Sampled" npts "points with exact outputs")
      (define-values (pts* exs*)
        (for/lists (pts exs) ([_ (in-range (*num-points*))] [pt (in-list pts)] [ex (in-list exs)])
          (values pt ex)))
      (mk-pcontext pts* exs*)]
     [else
      (define num-vars (length (program-variables prog)))
      (define num (max 4 (- (*num-points*) npts))) ; pad to avoid repeatedly trying to get last point
      (debug #:from 'points #:depth 4
             "Sampling" num "additional inputs,"
             "on iter" num-loops "have" npts "/" (*num-points*))
      (define pts1 (for/list ([n (in-range num)]) (sampler)))
      (define exs1 (make-exacts-halfpoints prog pts1 precondition repr))
      (debug #:from 'points #:depth 4
             "Filtering points with unrepresentable outputs")
      (define-values (pts* exs*) (filter-p&e pts1 exs1))
      (loop (append pts* pts) (append exs* exs) (+ 1 num-loops))])))
