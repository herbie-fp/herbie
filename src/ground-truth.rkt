#lang racket

(require math/bigfloat rival)
(require [only-in "errors.rkt" warn raise-herbie-error]
         [only-in "programs.rkt" program-variables program-body eval-prog expr-supports?]
         [only-in "timeline.rkt" timeline-push! timeline-compact!]
         [only-in "float.rkt" ordinary-value?]
         [only-in "interface.rkt" representation-bf->repr get-representation]
         [only-in "config.rkt" *max-mpfr-prec* *num-points* *max-skipped-points*]
         [only-in "points.rkt" mk-pcontext]
         [only-in "sampling.rkt" make-search-func])

(provide batch-prepare-points prepare-points sample-points)

(module+ test (require rackunit "load-plugin.rkt"))

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
        [`(unsamplable ,prec ,pt) (list 'overflowed prec)]
        [`(sampled ,prec ,pt) (list 'valid prec)]
        [`(invalid ,prec ,pt) (list 'invalid prec)]))
    (define dt (- now start))
    (timeline-push! 'outcomes (~a name) prec (~a category) dt 1)
    (set! start now))
  log!)

(define (ival-eval fn pt repr #:precision [precision 80] #:log [log! void])
  (define <-bf (representation-bf->repr repr))
  (let loop ([precision precision])
    (match-define (list valid exs ...) (parameterize ([bf-precision precision]) (apply fn pt)))
    (define precision* (exact-floor (* precision 2)))
    (cond
     [(not (ival-hi valid))
      (log! 'invalid precision pt)
      +nan.0]
     [(and (not (ival-lo valid)) (ival-lo-fixed? valid))
      (log! 'unsamplable precision pt)
      +nan.0]
     [(ival-lo valid)
      (log! 'sampled precision pt)
      (map <-bf exs)]
     [(> precision* (*max-mpfr-prec*))
      (log! 'exit precision pt)
      +nan.0]
     [else
      (loop precision*)])))

(define (batch-prepare-points progs precondition repr sampler)
  (timeline-push! 'method "intervals")

  ;; If we're using the bf fallback, start at the max precision
  (define starting-precision
    (if (and (expr-supports? (program-body precondition) 'ival)
             (expr-supports? (program-body prog) 'ival))
        (bf-precision)
        (*max-mpfr-prec*)))

  (define fn (make-search-func precondition progs repr '()))

  (define-values (points exactss)
    (let loop ([sampled 0] [skipped 0] [points '()] [exactss '()])
      (define pt (sampler))

      (define exs
        (ival-eval fn pt repr
                   #:precision starting-precision
                   #:log (point-logger 'body prog)))

      (cond
       [(and (not (nan? ex)) (andmap (curryr ordinary-value? repr) pt))
        (if (>= (+ 1 sampled) (*num-points*))
            (values (cons pt points) (cons ex exactss))
            (loop (+ 1 sampled) 0 (cons pt points) (cons ex exactss)))]
       [else
        (when (>= skipped (*max-skipped-points*))
          (raise-herbie-error "Cannot sample enough valid points."
                              #:url "faq.html#sample-valid-points"))
        (loop sampled (+ 1 skipped) points exactss)])))
  (timeline-compact! 'outcomes)
  (cons points (flip-lists exactss)))

(define (prepare-points prog precondition repr sampler)
  (apply mk-pcontext (batch-prepare-points (list prog) precondition repr sampler)))

(define (sample-points precondition progs repr)
  (define sampler (make-sampler repr precondition progs empty))
  (batch-prepare-points progs precondition repr sampler))
