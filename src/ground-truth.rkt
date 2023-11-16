#lang racket

(require math/bigfloat rival)
(require "syntax/sugar.rkt" "syntax/types.rkt"
         "common.rkt" "compiler.rkt" "sampling.rkt" "timeline.rkt"
         "errors.rkt")

(provide sample-points batch-prepare-points make-search-func eval-progs-real)

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
(define (make-search-func pre exprs ctxs)
  (define specs (map prog->legacy (cons pre exprs)))
  (define fns (compile-specs specs (context-vars (car ctxs))))
  ; inputs can either be intervals or representation values
  (λ inputs
    (define inputs*
      (for/list ([input (in-list inputs)]
                 [repr (context-var-reprs (car ctxs))])
        (if (ival? input) input (ival ((representation-repr->bf repr) input)))))
    (match-define (list ival-pre ival-bodies ...) (apply fns inputs*))
    (for/list ([y ival-bodies] [ctx ctxs])
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
       y))))

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

(define (combine-tables t1 t2)
  (define t2-total (apply + (hash-values t2)))
  (define t1-base (+ (hash-ref t1 'unknown 0) (hash-ref t1 'valid 0)))
  (for/fold ([t1 (hash-remove (hash-remove t1 'unknown) 'valid)])
      ([(k v) (in-hash t2)])
    (hash-set t1 k (+ (hash-ref t1 k 0) (* (/ v t2-total) t1-base)))))

(define (sample-points pre exprs ctxs)
  (timeline-event! 'analyze)
  (define fn (make-search-func pre exprs ctxs))
  (match-define (cons sampler table)
    (parameterize ([ground-truth-require-convergence #f])
      ;; TODO: Should make-sampler allow multiple contexts?
      (make-sampler (first ctxs) pre fn)))
  (timeline-event! 'sample)
  ;; TODO: should batch-prepare-points allow multiple contexts?
  (match-define (cons table2 results) (batch-prepare-points fn (first ctxs) sampler))
  (define total (apply + (hash-values table2)))
  (when (> (hash-ref table2 'infinite 0.0) (* 0.2 total))
   (warn 'inf-points #:url "faq.html#inf-points"
    "~a of points produce a very large (infinite) output. You may want to add a precondition." 
    (format-accuracy (- total (hash-ref table2 'infinite)) total #:unit "%")))
  (cons (combine-tables table table2) results))
