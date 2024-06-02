#lang racket

(require rival)

(require "correct-round.rkt")

(provide rival-compile rival-apply rival-analyze
         (struct-out exn:rival)
         (struct-out exn:rival:invalid)
         (struct-out exn:rival:unsamplable)
         (struct-out discretization)
         *rival-max-precision* *rival-max-iterations*
         rival-profile (struct-out execution) *rival-profile-executions*)

(define ground-truth-require-convergence (make-parameter #t))

(define (is-samplable-interval disc interval)
  (define convert (discretization-convert disc))
  (define distance (discretization-distance disc))
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

(define (rival-compile exprs vars discs)
  (compile-specs exprs vars discs))

(struct exn:rival exn:fail ())
(struct exn:rival:invalid exn:rival (pt))
(struct exn:rival:unsamplable exn:rival (pt))

(define (ival-any-error? ivals)
  (for/fold ([out (ival-error? (vector-ref ivals 0))])
            ([iv (in-vector ivals 1)])
    (ival-or out (ival-error? iv))))

(struct execution (name number precision time) #:prefab)

(define (rival-profile machine param)
  (match param
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
           (execution instruction number precision time))
       (set-rival-machine-profile-ptr! machine 0))]))

(define (ival-real x)
  (ival x))

(define (rival-apply machine pt)
  (define discs (rival-machine-discs machine))
  (let loop ([iter 0])
    (define exs
      (parameterize ([*sampling-iteration* iter]
                     [ground-truth-require-convergence #t])
        (rival-machine-full machine (vector-map ival-real pt))))
    (match-define (ival err err?) (ival-any-error? exs))
    (cond
      [err
       (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [(not err?)
       (for/list ([ex (in-vector exs)] [disc (in-vector discs)])
         ; We are promised at this point that (distance (convert lo) (convert hi)) = 0 so use lo
         ([discretization-convert disc] (ival-lo ex)))]
      [(>= iter (*rival-max-iterations*))
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else
       (loop (+ 1 iter))])))

(define (rival-analyze machine rect)
  (define res
    (parameterize ([*sampling-iteration* 0]
                   [ground-truth-require-convergence #f])
      (rival-machine-full machine rect)))
  (ival-any-error? res))
