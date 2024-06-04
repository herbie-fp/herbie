#lang racket

(require rival)

(require "correct-round.rkt")

(provide my-rival-compile my-rival-apply my-rival-analyze
         (struct-out my-discretization)
         (struct-out my-exn:rival)
         (struct-out my-exn:rival:invalid)
         (struct-out my-exn:rival:unsamplable)
         *my-rival-max-precision*
         *my-rival-max-iterations*
         *my-rival-profile-executions*
         my-rival-profile (struct-out my-execution))

(define ground-truth-require-convergence (make-parameter #t))

(define (is-samplable-interval disc interval)
  (define convert (my-discretization-convert disc))
  (define distance (my-discretization-distance disc))
  (define (close-enough? lo hi)
    (= (distance (convert lo) (convert hi)) 0))
  ((close-enough->ival close-enough?) interval))

(define (rival-machine-full machine inputs)
  (set-rival-machine-iteration! machine (*sampling-iteration*))
  (rival-machine-adjust machine)
  (rival-machine-load machine inputs)
  (rival-machine-run machine)
  (define outvec (rival-machine-return machine))
  (define outlen (vector-length outvec))
  (define discs (rival-machine-discs machine))
  (for/vector #:length outlen ([y (in-vector outvec)] [disc (in-vector discs)])
    (ival-then
     ; The two `invalid` ones have to go first, because later checks
     ; can error if the input is erroneous
     (ival-assert (ival-not (ival-error? y)) 'invalid)
     ; 'infinte case handle in `ival-eval`
     (ival-assert
      (if (ground-truth-require-convergence)
          (is-samplable-interval disc y)
          (ival (ival-hi (is-samplable-interval disc y))))
      'unsamplable)
     y)))

(define (my-rival-compile exprs vars discs)
  (compile-specs exprs vars discs))

(struct my-exn:rival exn:fail ())
(struct my-exn:rival:invalid my-exn:rival (pt))
(struct my-exn:rival:unsamplable my-exn:rival (pt))

(define (ival-any-error? ivals)
  (for/fold ([out (ival-error? (vector-ref ivals 0))])
            ([iv (in-vector ivals 1)])
    (ival-or out (ival-error? iv))))

(struct my-execution (name number precision time) #:prefab)

(define (my-rival-profile machine param)
  (match param
    ['instructions (vector-length (rival-machine-instructions machine))]
    ['iterations (rival-machine-iteration machine)]
    ['bumps (rival-machine-bumps machine)]
    ['executions
     (define profile-ptr (rival-machine-profile-ptr machine))
     (define profile-instruction (rival-machine-profile-instruction machine))
     (define profile-number (rival-machine-profile-number machine))
     (define profile-time (rival-machine-profile-time machine))
     (define profile-precision (rival-machine-profile-precision machine))
     (begin0
         (for/vector #:length profile-ptr
                     ([instruction (in-vector profile-instruction 0 profile-ptr)]
                      [number (in-vector profile-number 0 profile-ptr)]
                      [precision (in-vector profile-precision 0 profile-ptr)]
                      [time (in-vector profile-time 0 profile-ptr)])
           (my-execution instruction number precision time))
       (set-rival-machine-profile-ptr! machine 0))]))

(define (ival-real x)
  (ival x))

(define (my-rival-apply machine pt)
  (define discs (rival-machine-discs machine))
  (set-rival-machine-bumps! machine 0)
  (let loop ([iter 0])
    (define exs
      (parameterize ([*sampling-iteration* iter]
                     [ground-truth-require-convergence #t])
        (rival-machine-full machine (vector-map ival-real pt))))
    (match-define (ival err err?) (ival-any-error? exs))
    (cond
      [err
       (raise (my-exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [(not err?)
       (for/vector #:length (vector-length discs) ([ex (in-vector exs)] [disc (in-vector discs)])
         ; We are promised at this point that (distance (convert lo) (convert hi)) = 0 so use lo
         ([my-discretization-convert disc] (ival-lo ex)))]
      [(>= iter (*rival-max-iterations*))
       (raise (my-exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else
       (loop (+ 1 iter))])))

(define (my-rival-analyze machine rect)
  (define res
    (parameterize ([*sampling-iteration* 0]
                   [ground-truth-require-convergence #f])
      (rival-machine-full machine rect)))
  (ival-any-error? res))
