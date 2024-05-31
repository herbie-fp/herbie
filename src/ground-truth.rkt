#lang racket

(require rival)

(require "correct-round.rkt")

(provide rival-compile rival-apply rival-analyze
         (struct-out exn:rival)
         (struct-out exn:rival:invalid)
         (struct-out exn:rival:unsamplable)
         rival-profile-iterations-taken
         (struct-out discretization))

(define ground-truth-require-convergence (make-parameter #t))

(define (is-samplable-interval disc interval)
  (define convert (discretization-convert disc))
  (define distance (discretization-distance disc))
  (define (close-enough? lo hi)
    (= (distance (convert lo) (convert hi)) 0))
  ((close-enough->ival close-enough?) interval))

(struct rival-machine (fn discs))

(define (rival-compile exprs vars discs)
  (define fns (compile-specs exprs vars discs))
  (define outlen (length exprs))
  (define (rival-compiled inputs)
    (define outvec (fns inputs))
    (for/vector #:length outlen ([y (in-vector outvec)] [disc (in-list discs)])
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
  (rival-machine rival-compiled discs))

(struct exn:rival exn:fail ())
(struct exn:rival:invalid exn:rival (pt))
(struct exn:rival:unsamplable exn:rival (pt))

(define (ival-any-error? ivals)
  (for/fold ([out (ival-error? (vector-ref ivals 0))])
            ([iv (in-vector ivals 1)])
    (ival-or out (ival-error? iv))))

(define rival-profile-iterations-taken 0)

(define (ival-real x)
  (ival x))

(define (rival-apply machine pt)
  (match-define (rival-machine fn discs) machine)
  (let loop ([iter 0])
    (set! rival-profile-iterations-taken iter)
    (define exs
      (parameterize ([*sampling-iteration* iter]
                     [ground-truth-require-convergence #t])
        (fn (vector-map ival-real pt))))
    (match-define (ival err err?) (ival-any-error? exs))
    (cond
      [err
       (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [(not err?)
       (for/list ([ex (in-vector exs)] [disc (in-list discs)])
         ; We are promised at this point that (distance (convert lo) (convert hi)) = 0 so use lo
         ([discretization-convert disc] (ival-lo ex)))]
      [(>= iter (*max-sampling-iterations*))
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else
       (loop (+ 1 iter))])))

(define (rival-analyze machine rect)
  (match-define (rival-machine fn discs) machine)
  (define res
    (parameterize ([*sampling-iteration* 0]
                   [ground-truth-require-convergence #f])
      (fn rect)))
  (ival-any-error? res))
