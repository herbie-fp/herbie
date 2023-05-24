#lang racket

(require math/bigfloat rival)
(require "programs.rkt" "syntax/types.rkt" "sampling.rkt" "timeline.rkt")

(provide sample-points batch-prepare-points make-search-func eval-prog-list-real)

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

(define ground-truth-require-convergence (make-parameter #t))

;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func precondition programs ctxlist)
  (define fns (batch-eval-progs (cons precondition programs) 'ival (car ctxlist)))
  (λ inputs
    (match-define (list ival-pre ival-bodies ...) (apply fns inputs))
    (for/list ([y ival-bodies])
      (define repr (context-repr (car ctxlist)))
      (ival-then
       ; The two `invalid` ones have to go first, because later checks
       ; can error if the input is erroneous
       (ival-assert (ival-not (ival-error? y)) 'invalid)
       (ival-assert (ival-not (ival-error? ival-pre)) 'invalid)
       (ival-assert ival-pre 'precondition)
       (ival-assert (ival-not (is-infinite-interval repr y)) 'infinite)
       (ival-assert
        (if (ground-truth-require-convergence)
            (is-samplable-interval repr y)
            (ival (ival-hi (is-samplable-interval repr y))))
        'unsamplable)
       y))))

; ENSURE: all contexts have the same list of variables
(define (eval-prog-list-real prog-list ctx-list)
  (define pre `(λ ,(context-vars (car ctx-list)) (TRUE)))
  (define fn (make-search-func pre prog-list ctx-list))
  (define (f . pt)
    (define-values (result prec exs) (ival-eval fn pt))
    (match exs
      [(? list?)
       (for/list ([ex exs] [ctx* ctx-list])
         ((representation-bf->repr (context-repr ctx*)) (ival-lo ex)))]
      [(? nan?)
       (for/list ([_ prog-list] [ctx* ctx-list])
         ((representation-bf->repr (context-repr ctx*)) +nan.bf))]))
  (procedure-rename f '<eval-prog-real>))

(define (combine-tables t1 t2)
  (define t2-total (apply + (hash-values t2)))
  (define t1-base (+ (hash-ref t1 'unknown 0) (hash-ref t1 'valid 0)))
  (define t2* (hash-map t2 (λ (k v) (* (/ v t2-total) t1-base))))
  (for/fold ([t1 (hash-remove (hash-remove t1 'unknown) 'valid)])
      ([(k v) (in-hash t2)])
    (hash-set t1 k (+ (hash-ref t1 k 0) (* (/ v t2-total) t1-base)))))

(define (sample-points precondition progs ctxlist)
  (timeline-event! 'analyze)
  (define fn (make-search-func precondition progs ctxlist))
  (match-define (cons sampler table)
    (parameterize ([ground-truth-require-convergence #f])
      ;; TODO: Should make-sampler allow multiple contexts?
      (make-sampler (first ctxlist) precondition progs fn)))
  (timeline-event! 'sample)
  (match-define (cons table2 results) 
  ;; TODO: should batch-prepare-points allow multiple contexts?
  (batch-prepare-points fn (first ctxlist) sampler))
  (cons (combine-tables table table2) results))
