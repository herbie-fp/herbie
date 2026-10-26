#lang racket
(require math/bigfloat
         math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../syntax/block.rkt"
         "searchreals.rkt"
         "../syntax/rival.rkt")

(provide sample-points)

;; Part 1: use FPBench's condition->range-table to create initial hyperrects

(define (precondition->hyperrects pre vars var-reprs)
  (define range-table (condition->range-table pre))
  (apply cartesian-product
         (for/list ([var-name (in-vector vars)]
                    [var-repr (in-vector var-reprs)])
           (map (lambda (interval) (fpbench-ival->ival var-repr interval))
                (range-table-ref range-table var-name)))))

(define (fpbench-ival->ival repr fpbench-interval)
  (match-define (interval lo hi lo? hi?) fpbench-interval)
  (match (representation-type repr)
    ['real (ival (bfstep (bf lo) (if lo? 0 1)) (bfstep (bf hi) (if hi? 0 -1)))]
    ['bool (ival #f #t)]))

(module+ test
  (require rackunit)
  (require "../syntax/platform.rkt"
           "../syntax/load-platform.rkt")
  (activate-platform! (*platform-name*))
  (define binary64 (get-representation 'binary64))
  (define pre '(and (and (<= 0 a) (<= a 1)) (and (<= 0 b) (<= b 1))))

  (check-equal? (precondition->hyperrects pre '#(a b) (vector binary64 binary64))
                (list (list (ival 0.bf 1.bf) (ival 0.bf 1.bf)))))

(module+ test
  (define bool (get-representation 'bool))
  (define ctx (context '(x) bool (list binary64)))
  (define-values (block vs) (progs->block (list '(< x 0)) #:ctx ctx))
  (define results
    (parameterize ([*num-points* 4]
                   [*max-find-range-depth* 2])
      (sample-points '(and (<= -1 x) (<= x 1)) block vs (list bool))))
  (check-equal? (length (first results)) 4)
  (check-equal? (length (second results)) 4)
  (check-true (andmap boolean? (second results))))

;; Part 2: using subdivision search to find valid intervals

;; we want a index i such that vector[i] > num and vector[i-1] <= num
;; assumes vector strictly increasing
(define (binary-search vector num)
  (let loop ([left 0]
             [right (- (vector-length vector) 1)])
    (cond
      [(= left right) left]
      [else
       (define mid (arithmetic-shift (+ left right) -1))
       (define pivot (vector-ref vector mid))
       (if (<= pivot num)
           (loop (+ 1 mid) right)
           (loop left mid))])))

;; Draw from exact integer weights without generating a random integer as wide as
;; their sum. Refine a uniform binary prefix until its interval belongs to one
;; weight; only prefixes that straddle a boundary need more random bits.
(define (sample-weighted-index weights total)
  (let ([initial-bits (min 60 (+ (integer-length (vector-length weights)) 4))])
    (let loop ([prefix (random-bits initial-bits)]
               [bits initial-bits])
      (define lower-product (* prefix total))
      (define upper-product (+ lower-product total))
      (define index (binary-search weights (arithmetic-shift lower-product (- bits))))
      ;; The prefix interval must stay below the selected cumulative weight.
      (if (<= upper-product (arithmetic-shift (vector-ref weights index) bits))
          index
          (let ([more-bits 30])
            (loop (+ (arithmetic-shift prefix more-bits) (random-bits more-bits))
                  (+ bits more-bits)))))))

;; Rejection avoids bias when bound is not a power of two.
(define (random-below bound)
  (define bits (integer-length (sub1 bound)))
  (let loop ()
    (define candidate (random-bits bits))
    (if (< candidate bound)
        candidate
        (loop))))

(module+ test
  (define arr (partial-sums (build-vector 50 (lambda (_) (random-integer 1 100)))))
  (define max-num (vector-ref arr (- (vector-length arr) 1)))
  (for ([search-for (in-range max-num)])
    (define search-result (binary-search arr search-for))
    (check-true (> (vector-ref arr search-result) search-for))
    (check-true (or (zero? search-result) (<= (vector-ref arr (- search-result 1)) search-for)))))

(define (make-hyperrect-sampler hyperrects* hints* reprs)
  (when (null? hyperrects*)
    (raise-herbie-sampling-error "No valid values." #:url "faq.html#no-valid-values"))
  (define hints (list->vector hints*))
  (define hyperrects (list->vector hyperrects*))
  (define lo-ends
    (for/vector #:length (vector-length hyperrects)
                ([hyperrect (in-vector hyperrects)])
      (for/list ([interval (in-list hyperrect)]
                 [repr (in-vector reprs)])
        ((representation-repr->ordinal repr) ((representation-bf->repr repr) (ival-lo interval))))))
  (define hi-ends
    (for/vector #:length (vector-length hyperrects)
                ([hyperrect (in-vector hyperrects)])
      (for/list ([interval (in-list hyperrect)]
                 [repr (in-vector reprs)])
        (+ 1
           ((representation-repr->ordinal repr)
            ((representation-bf->repr repr) (ival-hi interval)))))))
  (define widths
    (for/vector #:length (vector-length hyperrects)
                ([los (in-vector lo-ends)]
                 [his (in-vector hi-ends)])
      (map - his los)))
  (define rect-weights (vector-map (curryr hyperrect-weight reprs) hyperrects))
  (define weight-gcd
    (for/fold ([common-divisor 0]) ([weight (in-vector rect-weights)])
      (gcd common-divisor weight)))
  ;; Removing a common factor preserves probabilities and shrinks the draw bound.
  (define weights (partial-sums (vector-map (curryr quotient weight-gcd) rect-weights)))
  (define weight-max (vector-ref weights (- (vector-length weights) 1)))

  ;; returns pt and hint
  (define num-vars (vector-length reprs))
  (define (hyperrect-sampler)
    (define idx (sample-weighted-index weights weight-max))
    (values (for/vector #:length num-vars
                        ([lo (in-list (vector-ref lo-ends idx))]
                         [width (in-list (vector-ref widths idx))]
                         [repr (in-vector reprs)])
              ((representation-ordinal->repr repr) (+ lo (random-below width))))
            (vector-ref hints idx)))
  hyperrect-sampler)

(module+ test
  (define ordinal->binary64 (representation-ordinal->repr binary64))
  (define binary64->ordinal (representation-repr->ordinal binary64))
  (define binary64->bf (representation-repr->bf binary64))

  (define (sample-ordinal-range lo-ordinal hi-ordinal)
    (define expected
      (for/list ([ordinal (in-range lo-ordinal (add1 hi-ordinal))])
        (ordinal->binary64 ordinal)))
    (define interval (ival (binary64->bf (first expected)) (binary64->bf (last expected))))
    (define sampler (make-hyperrect-sampler (list (list interval)) '(#f) (vector binary64)))
    (define samples
      (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
        (random-seed 1)
        (for/list ([i (in-range 400)])
          (define-values (point hint) (sampler))
          (vector-ref point 0))))
    (check-equal? (sort (remove-duplicates samples) <) (sort expected <))
    (for ([value (in-list expected)])
      (define num-occurrences (count (curry = value) samples))
      (check-true (<= 50 num-occurrences 150))))

  (sample-ordinal-range -1 1)
  (define broad-interval (ival (binary64->bf -1.0) (binary64->bf 1.0)))
  (define broad-width (- (add1 (binary64->ordinal 1.0)) (binary64->ordinal -1.0)))
  (check-true (> broad-width (expt 2 60)))
  (define broad-sampler
    (make-hyperrect-sampler (list (list broad-interval)) '(broad) (vector binary64)))
  (check-true (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
                (random-seed 4)
                (for/and ([i (in-range 100)])
                  (define-values (point hint) (broad-sampler))
                  (and (eq? hint 'broad) (<= -1.0 (vector-ref point 0) 1.0)))))

  (define multi-reprs (vector binary64 binary64))
  (define base-ordinal (binary64->ordinal 1.0))
  (define (ordinal-interval lo count)
    (ival (binary64->bf (ordinal->binary64 lo))
          (binary64->bf (ordinal->binary64 (+ lo (sub1 count))))))
  (define full-interval (ordinal-interval base-ordinal #x100000000))
  (define half-interval (ordinal-interval base-ordinal #x80000000))
  (define full-rect (list full-interval full-interval))
  (define half-rect (list half-interval full-interval))
  (define full-weight (hyperrect-weight full-rect multi-reprs))
  (define half-weight (hyperrect-weight half-rect multi-reprs))
  (check-true (> full-weight (expt 2 63)))
  (check-equal? full-weight (* 2 half-weight))
  (define multi-sampler (make-hyperrect-sampler (list full-rect half-rect) '(full half) multi-reprs))
  (define full-hi-ordinal (+ base-ordinal #xffffffff))
  (define half-hi-ordinal (+ base-ordinal #x7fffffff))
  (define full-count
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 2)
      (for/fold ([full-count 0]) ([i (in-range 1000)])
        (define-values (point hint) (multi-sampler))
        (check-equal? (vector-length point) 2)
        (for ([value (in-vector point)]
              [var-idx (in-naturals)])
          (define ordinal (binary64->ordinal value))
          (define hi-ordinal
            (if (and (eq? hint 'half) (zero? var-idx)) half-hi-ordinal full-hi-ordinal))
          (check-true (<= base-ordinal ordinal hi-ordinal)))
        (check-true (or (eq? hint 'full) (eq? hint 'half)))
        (if (eq? hint 'full)
            (add1 full-count)
            full-count))))
  (check-true (<= 620 full-count 715)))

(module+ test
  (define many-reprs (make-vector 8 binary64))
  (define wide-interval (ordinal-interval base-ordinal #x100000000))
  (define almost-wide-interval (ordinal-interval base-ordinal #xffffffff))
  (define wide-rect (make-list 8 wide-interval))
  (define almost-wide-rect (make-list 8 almost-wide-interval))
  (define wide-weight (hyperrect-weight wide-rect many-reprs))
  (define almost-wide-weight (hyperrect-weight almost-wide-rect many-reprs))
  (check-true (> wide-weight (expt 2 255)))
  (check-equal? (gcd wide-weight almost-wide-weight) 1)
  (define wide-sampler
    (make-hyperrect-sampler (list wide-rect almost-wide-rect) '(wide almost-wide) many-reprs))
  (define wide-count
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 3)
      (for/fold ([wide-count 0]) ([i (in-range 1000)])
        (define-values (point hint) (wide-sampler))
        (check-equal? (vector-length point) 8)
        (for ([value (in-vector point)])
          (define ordinal (binary64->ordinal value))
          (define hi-ordinal
            (if (eq? hint 'wide)
                (+ base-ordinal #xffffffff)
                (+ base-ordinal #xfffffffe)))
          (check-true (<= base-ordinal ordinal hi-ordinal)))
        (check-true (or (eq? hint 'wide) (eq? hint 'almost-wide)))
        (if (eq? hint 'wide)
            (add1 wide-count)
            wide-count))))
  (check-true (<= 450 wide-count 550)))

(define (make-sampler compiler)
  (match-define (real-compiler pre vars var-reprs _ reprs _ _ _ _ _) compiler)
  (cond
    [(and (flag-set? 'setup 'search)
          (not (vector-empty? var-reprs))
          (for/and ([repr (in-vector var-reprs)])
            (equal? (representation-type repr) 'real)))
     (timeline-push! 'method "search")
     (define hyperrects-analysis (precondition->hyperrects pre vars var-reprs))
     ; hints-hyperrects is a (listof '(hint hyperrect))
     (match-define (list hyperrects hints sampling-table)
       (find-intervals compiler hyperrects-analysis #:fuel (*max-find-range-depth*)))
     (values (make-hyperrect-sampler hyperrects hints var-reprs) sampling-table)]
    [else
     (timeline-push! 'method "random")
     ; sampler return false hint since rival-analyze has not been called in random method
     (values (λ () (values (vector-map random-generate var-reprs) #f)) (hash 'unknown 1.0))]))

(define (combine-tables t1 t2)
  (define t2-total (apply + (hash-values t2)))
  (define t1-base (+ (hash-ref t1 'unknown 0) (hash-ref t1 'valid 0)))
  (for/fold ([t1 (hash-remove (hash-remove t1 'unknown) 'valid)]) ([(k v) (in-hash t2)])
    (hash-set t1 k (+ (hash-ref t1 k 0) (* (/ v t2-total) t1-base)))))

(define (sample-points pre block vs reprs)
  (timeline-event! 'analyze)
  (define compiler (make-real-compiler block vs reprs #:pre pre))
  (define-values (sampler table) (make-sampler compiler))
  (timeline-event! 'sample)

  ;; Part 3: sample points and compute exact outputs using Rival.
  (define outcomes (make-hash))
  (define vars (real-compiler-vars compiler))
  (define var-reprs (real-compiler-var-reprs compiler))
  (define output-reprs (real-compiler-reprs compiler))
  (define assemble-point (real-compiler-assemble-point compiler))
  (define assemble-output (real-compiler-assemble-output compiler))
  (define exact-count (length (assemble-output (make-list (vector-length output-reprs) #f))))

  (define-values (points exactss)
    (let loop ([sampled 0]
               [skipped 0]
               [points '()]
               [exactss (make-list exact-count '())])
      (define-values (pt hint) (sampler))
      (define-values (status exs) (real-apply compiler pt hint))
      (case status
        [(exit)
         (warn 'ground-truth
               "could not determine a ground truth"
               #:url "faq.html#ground-truth"
               #:extra (vector->list (vector-map (curry format "~a = ~a") vars pt)))]
        [(valid)
         (for ([ex (in-list exs)]
               [repr (in-vector output-reprs)])
           ; The `bool` representation does not produce bigfloats
           (define maybe-bf ((representation-repr->bf repr) ex))
           (when (and (bigfloat? maybe-bf) (bfinfinite? maybe-bf))
             (set! status 'infinite)))])

      (hash-update! outcomes status add1 0)

      (define is-bad?
        (for/or ([input (in-vector pt)]
                 [repr (in-vector var-reprs)])
          ((representation-special-value? repr) input)))

      (cond
        [(and (list? exs) (not is-bad?))
         (define assembled-exs (assemble-output exs))
         (define assembled-pt (assemble-point pt))
         (define next-exactss (map cons assembled-exs exactss))
         (if (>= (+ 1 sampled) (*num-points*))
             (values (cons assembled-pt points) next-exactss)
             (loop (+ 1 sampled) 0 (cons assembled-pt points) next-exactss))]
        [else
         (when (>= skipped (*max-skipped-points*))
           (raise-herbie-sampling-error "Cannot sample enough valid points."
                                        #:url "faq.html#sample-valid-points"))
         (loop sampled (+ 1 skipped) points exactss)])))

  (define total (apply + (hash-values outcomes)))
  (when (> (hash-ref outcomes 'infinite 0.0) (* 0.2 total))
    (warn 'inf-points
          #:url "faq.html#inf-points"
          "~a% of points produce a very large (infinite) output. You may want to add a precondition."
          (~r (/ (- total (hash-ref outcomes 'infinite)) total 0.01) #:precision '(= 1))))
  (timeline-push! 'bogosity (combine-tables table outcomes))
  (cons points exactss))
