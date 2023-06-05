#lang racket

(require math/bigfloat rival)
(require "programs.rkt" "syntax/types.rkt" "sampling.rkt" "timeline.rkt" "errors.rkt")

(provide sample-points batch-prepare-points make-search-func eval-prog-real)

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
(define (make-search-func pre exprs ctx)
  (define fns (batch-eval-progs (cons pre exprs) 'ival ctx))
  (λ inputs
    (define out (apply fns inputs))
    (match-define (list ival-pre ival-bodies ...) out)
    (define repr (context-repr ctx))
    (for/list ([y ival-bodies])
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
       y))))

(define (eval-prog-real prog ctx)
  (define repr (context-repr ctx))
  (define fn (make-search-func '(TRUE) (list prog) ctx))
  (define (f . pt)
    (define-values (result prec exs) (ival-eval repr fn pt))
    (match exs
      [(list (ival lo hi))
       ((representation-bf->repr repr) lo)]
      [(? nan?)
       +nan.0]))
  (procedure-rename f '<eval-prog-real>))

(define (combine-tables t1 t2)
  (define t2-total (apply + (hash-values t2)))
  (define t1-base (+ (hash-ref t1 'unknown 0) (hash-ref t1 'valid 0)))
  (for/fold ([t1 (hash-remove (hash-remove t1 'unknown) 'valid)])
      ([(k v) (in-hash t2)])
    (hash-set t1 k (+ (hash-ref t1 k 0) (* (/ v t2-total) t1-base)))))

(define (sample-points pre exprs ctx)
  (timeline-event! 'analyze)
  (define fn (make-search-func pre exprs ctx))
  (match-define (cons sampler table)
    (parameterize ([ground-truth-require-convergence #f])
      (make-sampler ctx pre fn)))
  (timeline-event! 'sample)
  (match-define (cons table2 results) (batch-prepare-points fn ctx sampler))
  (representation-repr->bf (context-repr ctx))
  (define combined-table (combine-tables table table2))
  (when (> (hash-ref combined-table 'infinite 0.0) 0.2)
    (warn 'inf-points #:url "faq.html#inf-points"
      "Getting infinite outputs for ~a of points. You may want to add a precondition." (hash-ref combined-table 'infinite)))
  (cons combined-table results))
