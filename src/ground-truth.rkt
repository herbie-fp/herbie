#lang racket

(require math/bigfloat
         rival)

(require "syntax/types.rkt"
         "common.rkt"
         "compiler.rkt" "timeline.rkt")

(provide eval-progs-real
         ground-truth-require-convergence 
         ival-eval
         make-search-func)

(define ground-truth-require-convergence (make-parameter #t))

(define (is-infinite-interval repr interval)
  (define <-bf (representation-bf->repr repr))
  (define ->bf (representation-repr->bf repr))
  ;; HACK: the comparisons to 0.bf is just about posits, where right now -inf.bf
  ;; rounds to the NaR value, which then represents +inf.bf, which is positive.
  (define (positive-inf? x)
    (parameterize ([bf-rounding-mode 'nearest])
      (and (bigfloat? x) (bf> x 0.bf) (bf= (->bf (<-bf x)) +inf.bf))))
  (define (negative-inf? x)
    (parameterize ([bf-rounding-mode 'nearest])
      (and (bigfloat? x) (bf< x 0.bf) (bf= (->bf (<-bf x)) -inf.bf))))
  (define ival-positive-infinite (monotonic->ival positive-inf?))
  (define ival-negative-infinite (comonotonic->ival negative-inf?))
  (ival-or (ival-positive-infinite interval)
           (ival-negative-infinite interval)))

(define (is-samplable-interval repr interval)
  (define <-bf (representation-bf->repr repr))
  (define (close-enough? lo hi)
    (let ([lo* (<-bf lo)] [hi* (<-bf hi)])
      (or (equal? lo* hi*) (and (number? lo*) (= lo* hi*)))))
  ((close-enough->ival close-enough?) interval))


;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func pre specs ctxs)
  (define fns (compile-specs (cons pre specs) (context-vars (car ctxs))))
  ; inputs can either be intervals or representation values
  (define (compiled-spec . inputs)
    (define inputs*
      (for/list ([input (in-list inputs)]
                 [repr (context-var-reprs (car ctxs))])
        (if (ival? input) input (ival ((representation-repr->bf repr) input)))))
    (define outvec (apply fns inputs*))
    (define ival-pre (vector-ref outvec 0))
    (for/list ([y (in-vector outvec 1)] [ctx (in-list ctxs)])
      (define repr (context-repr ctx))
      (ival-then
       ; The two `invalid` ones have to go first, because later checks
       ; can error if the input is erroneous
       (ival-assert (ival-not (ival-error? y)) 'invalid)
       (ival-assert (ival-not (ival-error? ival-pre)) 'invalid)
       (ival-assert ival-pre 'precondition)
       ; 'infinte case handle in `ival-eval`
       (ival-assert
        (if (ground-truth-require-convergence)
            (is-samplable-interval repr y)
            (ival (ival-hi (is-samplable-interval repr y))))
        'unsamplable)
       y)))
  compiled-spec)

(define (ival-eval repr fn pt [iter 0] [precision (*starting-prec*)])
  (define start (current-inexact-milliseconds))
  (define-values (status final-prec value)
    (let loop ([iter iter] [precision precision])
      (define exs
        (if (*use-mixed-precision*)
            (parameterize ([*sampling-iteration* iter]) (apply fn pt))
            (parameterize ([bf-precision precision]) (apply fn pt))))
      (match-define (ival err err?) (apply ival-or (map ival-error? exs)))
      (define iter* (+ 1 iter))
      (define precision* (exact-floor (* precision 2)))
      (cond
        [err
         (values err (if (*use-mixed-precision*) iter precision) +nan.0)]
        [(not err?)
         (define infinite?
           (ival-lo (is-infinite-interval repr (apply ival-or exs))))
         (values (if infinite? 'infinite 'valid) (if (*use-mixed-precision*) iter precision) exs)]
        [(if (*use-mixed-precision*) (> iter* (*max-sampling-iterations*)) (> precision* (*max-mpfr-prec*)))
         (values 'exit (if (*use-mixed-precision*) iter precision) +nan.0)]
        [else
         (loop iter* precision*)])))
  (timeline-push!/unsafe 'outcomes (- (current-inexact-milliseconds) start)
                         final-prec (~a status) 1)
  (values status precision value))

; ENSURE: all contexts have the same list of variables
(define (eval-progs-real progs ctxs)
  (define repr (context-repr (car ctxs)))
  (define fn (make-search-func '(TRUE) progs ctxs))
  (define (f . pt)
    (define-values (result prec exs) (ival-eval repr fn pt))
    (match exs
      [(? list?)
      (for/list ([ex exs] [ctx* ctxs])
        ((representation-bf->repr (context-repr ctx*)) (ival-lo ex)))]
      [(? nan?)
      (for/list ([ctx* ctxs])
        ((representation-bf->repr (context-repr ctx*)) +nan.bf))]))
  (procedure-rename f '<eval-prog-real>))

