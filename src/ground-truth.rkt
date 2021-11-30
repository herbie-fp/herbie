#lang racket

(require math/bigfloat rival)
(require "errors.rkt" "programs.rkt" "interface.rkt" "sampling.rkt")

(provide make-search-func prepare-points sample-points)

(define (ival-positive-infinite repr interval)
  (define <-bf (representation-bf->repr repr))
  (define ->bf (representation-repr->bf repr))
  (define (positive-inf? x) (bf= (->bf (<-bf x)) +inf.bf))
  (match-define (ival lo hi) interval)
  (cond
   [(or (bfnan? lo) (bfnan? hi))
    (ival-bool #t)]
   [else
    (define i (ival (positive-inf? lo) (positive-inf? hi)))
    (define i2 (if (ival-lo-fixed? interval) (ival-fix-lo i) i))
    (define i3 (if (ival-hi-fixed? interval) (ival-fix-hi i2) i2))
    i3]))

(define (is-finite-interval repr interval)
  (define positive-inf? (ival-positive-infinite repr))
  (match-define (ival lo hi) interval)
  (cond
   [(bigfloat? lo)
    (ival-not (ival-or (ival-positive-infinite repr interval)
                       (ival-positive-infinite repr (ival-neg interval))))]
   [else
    (ival-bool #t)]))

(define (is-samplable-interval repr interval)
  (define <-bf (representation-bf->repr repr))
  (match-define (ival (app <-bf lo) (app <-bf hi)) interval)
  (define lo! (ival-lo-fixed? interval))
  (define hi! (ival-hi-fixed? interval))
  (define lo=hi (or (equal? lo hi) (and (number? lo) (= lo hi)))) ; 0.0 vs -0.0
  (define can-converge (or (not lo!) (not hi!) lo=hi))
  (ival lo=hi can-converge))

(define (valid-result? repr ival)
  (ival-and (is-finite-interval repr ival)
            (is-samplable-interval repr ival)
            (ival-not (ival-error? ival))))

(define (eval-prog-wrapper progs repr)
  (match (map (compose not (curryr expr-supports? 'ival) program-body) progs)
    ['()
     (values 'ival (batch-eval-progs progs 'ival repr))]
    [(list prog others ...)
     (warn 'no-ival-operator #:url "faq.html#no-ival-operator"
           "using unsound ground truth evaluation for program ~a" prog)
     (define f (batch-eval-progs progs 'bf repr))
     (values 'bf (λ (x) (vector-map (λ (y) (ival y)) (ival (f x)))))]))

;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func precondition programs repr preprocess-structs)
  (define preprocessor (ival-preprocesses precondition preprocess-structs repr))
  (define-values (how fns) (eval-prog-wrapper (cons precondition programs) repr))
  (values
   how 
   (λ inputs
     (define inputs* (preprocessor inputs))
     (match-define (list ival-pre ival-bodies ...) (vector->list (apply fns inputs*)))
     (cons (apply ival-and ival-pre (map (curry valid-result? repr) ival-bodies))
           ival-bodies))))

(define (prepare-points prog precondition repr sampler)
  (define-values (how fn) (make-search-func precondition (list prog) repr '()))
  (batch-prepare-points how fn repr sampler))

(define (sample-points precondition progs repr)
  (define-values (how fn) (make-search-func precondition (list prog) repr '()))
  (define sampler (make-sampler repr precondition progs how fn empty))
  (batch-prepare-points how fn repr sampler))
