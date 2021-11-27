#lang racket

(require math/bigfloat rival)
(require [only-in "errors.rkt" warn raise-herbie-error]
         [only-in "programs.rkt" program-variables program-body eval-prog expr-supports?]
         [only-in "timeline.rkt" timeline-push! timeline-compact!]
         [only-in "float.rkt" ordinary-value?]
         [only-in "interface.rkt" representation-bf->repr get-representation]
         [only-in "config.rkt" *max-mpfr-prec* *num-points* *max-skipped-points*]
         [only-in "points.rkt" mk-pcontext])

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

(define (eval-prog-ival prog name repr)
  (cond
   [(expr-supports? (program-body prog) 'ival)
    (eval-prog prog 'ival repr)]
   [else
    (warn 'no-ival-operator #:url "faq.html#no-ival-operator"
          "using unsound ground truth evaluation for program ~a" name)
    (compose mk-ival (eval-prog prog 'bf repr))]))

(define (prepare-points prog precondition repr sampler)
  (timeline-push! 'method "intervals")

  (define starting-precision
    (if (and (expr-supports? (program-body precondition) 'ival)
             (expr-supports? (program-body prog) 'ival))
        (bf-precision)
        (*max-mpfr-prec*)))

  (define pre-fn (eval-prog-ival precondition 'pre repr))
  (define body-fn (eval-prog-ival prog 'body repr))

  (define-values (points exacts)
    (let loop ([sampled 0] [skipped 0] [points '()] [exacts '()])
      (define pt (sampler))

      (define pre
        (or (equal? (program-body precondition) '(TRUE))
            (ival-eval pre-fn pt (get-representation 'bool)
                       #:precision starting-precision
                       #:log (point-logger 'pre precondition))))

      (define ex
        (and pre (ival-eval body-fn pt repr
                            #:precision starting-precision
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

