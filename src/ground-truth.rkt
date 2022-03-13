#lang racket

(require math/bigfloat rival)
(require "errors.rkt" "programs.rkt" "interface.rkt" "sampling.rkt")

(provide make-search-func prepare-points sample-points ground-truth-require-convergence)

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

(define (valid-result? repr out)
  (ival-and (ival-not (is-infinite-interval repr out))
            (if (ground-truth-require-convergence)
                (is-samplable-interval repr out)
                (ival (ival-hi (is-samplable-interval repr out))))
            (ival-not (ival-error? out))))

(define (eval-prog-wrapper progs repr)
  (displayln progs)
  (match (filter (compose not (curryr expr-supports? 'ival) program-body) progs)
    ['()
     (values 'ival (batch-eval-progs progs 'ival repr))]
    [(list prog others ...)
     (warn 'no-ival-operator #:url "faq.html#no-ival-operator"
           "using unsound ground truth evaluation for program ~a" prog)
     (define f (batch-eval-progs progs 'bf repr))
     (define (unival x) (if (bigfloat? x) x (ival-lo x)))
     (values 'bf (λ (x) (vector-map (λ (y) (ival y)) (apply f (map unival args)))))]))

;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func precondition programs repr)
  (define-values (how fns) (eval-prog-wrapper (cons precondition programs) repr))
  (values
   how 
   (λ inputs
     (match-define (list ival-pre ival-bodies ...) (vector->list (apply fns inputs)))
     (cons (apply ival-and ival-pre (map (curry valid-result? repr) ival-bodies))
           ival-bodies))))

(define (prepare-points prog precondition repr sampler)
  (define-values (how fn) (make-search-func precondition (list prog) repr))
  (batch-prepare-points how fn repr sampler))

(define (sample-points precondition progs repr)
  (define-values (how fn) (make-search-func precondition progs repr))
  (define sampler 
    (parameterize ([ground-truth-require-convergence #f])
      (make-sampler repr precondition progs how fn)))
  (batch-prepare-points how fn repr sampler))
