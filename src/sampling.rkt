#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "searchreals.rkt" "programs.rkt" "config.rkt" "errors.rkt" "common.rkt"
         "float.rkt" "alternative.rkt" "interface.rkt"
         "timeline.rkt" "syntax/types.rkt" "syntax/sugar.rkt"
         "preprocess.rkt")
(module+ test (require rackunit "load-plugin.rkt"))
(provide make-sampler valid-result?)

(define (precondition->hyperrects precondition reprs repr)
  ;; FPBench needs unparameterized operators
  (define range-table 
    (condition->range-table  
      (resugar-program (program-body precondition) repr #:full #f)))

  (apply cartesian-product
         (for/list ([var-name (program-variables precondition)] [repr reprs])
           (map (lambda (interval) (fpbench-ival->ival repr interval))
                (range-table-ref range-table var-name)))))

(define (fpbench-ival->ival repr fpbench-interval)
  (match-define (interval lo hi lo? hi?) fpbench-interval)
  (match (type-name (representation-type repr))
    ['real
     (define bound (bound-ordinary-values repr))
     (define bflo (match lo [-inf.0 (bf- bound)] [+inf.0 bound] [x (bf x)]))
     (define bfhi (match hi [-inf.0 (bf- bound)] [+inf.0 bound] [x (bf x)]))
     (ival (bfstep bflo (if lo? 0 1)) (bfstep bfhi (if hi? 0 -1)))]
    ['bool
     (ival #f #t)]))

(module+ test
  (check-equal? (precondition->hyperrects
                 '(λ (a b) (and (and (<=.f64 0 a) (<=.f64 a 1))
                                (and (<=.f64 0 b) (<=.f64 b 1)))) (list repr repr) repr)
                (list (list (ival (bf 0.0) (bf 1.0)) (ival (bf 0.0) (bf 1.0))))))

;; we want a index i such that vector[i] > num and vector[i-1] <= num
;; assumes vector strictly increasing
(define (binary-search vector num)
  (let loop ([left 0] [right (- (vector-length vector) 1)])
    (cond
     [(>= left right)
      (min left (- (vector-length vector) 1))]
     [else
      (define mid (floor (/ (+ left right) 2)))
      (define pivot (vector-ref vector mid))
      (if (<= pivot num) (loop (+ 1 mid) right) (loop left mid))])))

(module+ test
  (define rand-list
    (let loop ([current 0])
      (if (> current 200)
          empty
          (let ([r (+ current (random-integer 1 10))])
            (cons r (loop r))))))
  (define arr
    (list->vector rand-list))
  (for ([i (range 0 20)])
    (define max-num (vector-ref arr (- (vector-length arr) 1)))
    (define search-for (random-integer 0 max-num))
    (define search-result (binary-search arr search-for))
    (check-true (> (vector-ref arr search-result) search-for))
    (when (> search-result 0)
      (check-true (<= (vector-ref arr (- search-result 1)) search-for)))))

(define (sample-ival interval repr)
  (define ->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))
  (define <-ordinal (representation-ordinal->repr repr))
  (<-ordinal (random-integer (->ordinal (ival-lo interval))
                             (+ 1 (->ordinal (ival-hi interval))))))

(define (make-hyperrect-sampler hyperrects* reprs)
  (when (null? hyperrects*)
    (raise-herbie-sampling-error "No valid values." #:url "faq.html#no-valid-values"))
  (define hyperrects (list->vector hyperrects*))
  (define weights (partial-sums (vector-map (curryr hyperrect-weight reprs) hyperrects)))
  (define weight-max (vector-ref weights (- (vector-length weights) 1)))
  (λ ()
    (define rand-ordinal (random-integer 0 weight-max))
    (define hyperrect (vector-ref hyperrects (binary-search weights rand-ordinal)))
    (map sample-ival hyperrect reprs)))

(module+ test
  (define repr (get-representation 'binary64))
  (define two-point-hyperrects (list (list (ival (bf 0) (bf 0)) (ival (bf 1) (bf 1)))))
  (check-true
   (andmap (curry set-member? '(0.0 1.0))
           ((make-hyperrect-sampler two-point-hyperrects (list repr repr))))))

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
     (batch-eval-progs progs 'ival repr)]
    [(list prog others ...)
     (warn 'no-ival-operator #:url "faq.html#no-ival-operator"
           "using unsound ground truth evaluation for program ~a" prog)
     (define f (batch-eval-progs progs 'bf repr))
     (λ (x) (vector-map (λ (y) (ival y)) (ival (f x))))]))

;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func precondition programs repr preprocess-structs)
  (define preprocessor (ival-preprocesses precondition preprocess-structs repr))
  (define fns (eval-prog-wrapper (cons precondition programs) repr))
  (λ inputs
    (define inputs* (preprocessor inputs))
    (match-define (list ival-pre ival-bodies ...) (vector->list (apply fns inputs*)))
    (cons (apply ival-and ival-pre (map (curry valid-result? repr) ival-bodies))
          ival-bodies)))

(define (make-sampler-analysis repr reprs precondition programs preprocess-structs)
  (define hyperrects-analysis (precondition->hyperrects precondition reprs repr))
  (define search-func (make-search-func precondition programs repr preprocess-structs))
  (define hyperrects
    (find-intervals (compose car search-func) hyperrects-analysis
                    #:reprs reprs #:fuel (*max-find-range-depth*)))
  (make-hyperrect-sampler hyperrects reprs))

; These definitions in place, we finally generate the points.
; A sampler returns two points- one without preprocessing and one with preprocessing
(define (make-sampler repr precondition programs preprocess-structs)
  (define reprs (map (curry dict-ref (*var-reprs*)) (program-variables precondition)))

  (cond
   [(and
     (flag-set? 'setup 'search)
     (andmap (compose (curry equal? 'real) type-name representation-type) (cons repr reprs))
     (andmap (compose (curryr expr-supports? 'ival) program-body) (cons precondition programs))
     (not (empty? reprs)))
    (timeline-push! 'method "search")
    (make-sampler-analysis repr reprs precondition programs preprocess-structs)]
   [else
    (timeline-push! 'method "random")
    (λ () (map random-generate reprs))]))
