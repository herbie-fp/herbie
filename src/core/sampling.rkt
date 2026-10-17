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

(provide block-prepare-points
         sample-points)

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

(module+ test
  (define arr (partial-sums (build-vector 50 (lambda (_) (random-integer 1 100)))))
  (define max-num (vector-ref arr (- (vector-length arr) 1)))
  (for ([search-for (in-range max-num)])
    (define search-result (binary-search arr search-for))
    (check-true (> (vector-ref arr search-result) search-for))
    (check-true (or (zero? search-result) (<= (vector-ref arr (- search-result 1)) search-for)))))

;; Binary64 ordinals are wider than a fixnum. Bias them by 2^63 and keep the key in
;; two 32-bit fixnums so sampling a coordinate doesn't allocate a bignum.
(struct ordinal-key (hi lo))

(define ordinal-key-mask #xffffffff)

(define (binary64-ordinal->key ordinal)
  (ordinal-key (+ #x80000000 (arithmetic-shift ordinal -32)) (bitwise-and ordinal ordinal-key-mask)))

(define (ordinal-key<? a b)
  (or (< (ordinal-key-hi a) (ordinal-key-hi b))
      (and (= (ordinal-key-hi a) (ordinal-key-hi b)) (< (ordinal-key-lo a) (ordinal-key-lo b)))))

(define (ordinal-key-difference hi lo)
  (define lo-diff (- (ordinal-key-lo hi) (ordinal-key-lo lo)))
  (ordinal-key (- (ordinal-key-hi hi) (ordinal-key-hi lo) (if (negative? lo-diff) 1 0))
               (bitwise-and lo-diff ordinal-key-mask)))

(define (ordinal-key-width-bits width)
  (define hi (ordinal-key-hi width))
  (define lo (ordinal-key-lo width))
  (cond
    [(zero? hi) (integer-length (sub1 lo))]
    [(zero? lo) (+ 32 (integer-length (sub1 hi)))]
    [else (+ 32 (integer-length hi))]))

(define (random-ordinal-key-below width)
  (define bits (ordinal-key-width-bits width))
  (define (random-key)
    (cond
      [(zero? bits) (ordinal-key 0 0)]
      [else
       (define value (random-bits bits))
       (ordinal-key (arithmetic-shift value -32) (bitwise-and value ordinal-key-mask))]))
  (let loop ()
    (define candidate (random-key))
    (if (ordinal-key<? candidate width)
        candidate
        (loop))))

(define (ordinal-key+ a b)
  (define lo (+ (ordinal-key-lo a) (ordinal-key-lo b)))
  (ordinal-key (bitwise-and (+ (ordinal-key-hi a) (ordinal-key-hi b) (if (> lo ordinal-key-mask) 1 0))
                            ordinal-key-mask)
               (bitwise-and lo ordinal-key-mask)))

(define (ordinal-key->binary64 key bytes)
  (define hi (ordinal-key-hi key))
  (define lo (ordinal-key-lo key))
  (define-values (raw-hi raw-lo)
    (if (>= hi #x80000000)
        (values (- hi #x80000000) lo)
        (values (+ #x80000000 (- #x80000000 hi (if (zero? lo) 0 1)))
                (if (zero? lo)
                    0
                    (- (arithmetic-shift 1 32) lo)))))
  (integer->integer-bytes raw-lo 4 #f #f bytes 0)
  (integer->integer-bytes raw-hi 4 #f #f bytes 4)
  (floating-point-bytes->real bytes #f))

(define (sample-binary64 lo hi bytes)
  (define width (ordinal-key-difference hi lo))
  (ordinal-key->binary64 (ordinal-key+ lo (random-ordinal-key-below width)) bytes))

(define (make-hyperrect-sampler hyperrects* hints* reprs)
  (when (null? hyperrects*)
    (raise-herbie-sampling-error "No valid values." #:url "faq.html#no-valid-values"))
  (define hints (list->vector hints*))
  (define hyperrects (list->vector hyperrects*))
  (define (ordinal-endpoint interval repr upper?)
    (define ordinal
      ((representation-repr->ordinal repr) ((representation-bf->repr repr) (if upper?
                                                                               (ival-hi interval)
                                                                               (ival-lo interval)))))
    (define endpoint
      (if upper?
          (add1 ordinal)
          ordinal))
    (if (and (equal? repr <binary64>) (fixnum? ordinal-key-mask))
        (binary64-ordinal->key endpoint)
        endpoint))
  (define lo-ends
    (for/vector #:length (vector-length hyperrects)
                ([hyperrect (in-vector hyperrects)])
      (for/list ([interval (in-list hyperrect)]
                 [repr (in-vector reprs)])
        (ordinal-endpoint interval repr #f))))
  (define hi-ends
    (for/vector #:length (vector-length hyperrects)
                ([hyperrect (in-vector hyperrects)])
      (for/list ([interval (in-list hyperrect)]
                 [repr (in-vector reprs)])
        (ordinal-endpoint interval repr #t))))
  (define rect-weights (vector-map (curryr hyperrect-weight reprs) hyperrects))
  (define weight-gcd
    (for/fold ([common-divisor 0]) ([weight (in-vector rect-weights)])
      (gcd common-divisor weight)))
  ;; Removing a common factor preserves probabilities and shrinks the draw bound.
  (define weights (partial-sums (vector-map (curryr quotient weight-gcd) rect-weights)))
  (define weight-max (vector-ref weights (- (vector-length weights) 1)))

  ;; returns pt and hint
  (define num-vars (vector-length reprs))
  (define has-binary64?
    (for/or ([repr (in-vector reprs)])
      (equal? repr <binary64>)))
  (define (hyperrect-sampler)
    (define idx (sample-weighted-index weights weight-max))
    (define bytes (and has-binary64? (make-bytes 8)))
    (values (for/vector #:length num-vars
                        ([lo (in-list (vector-ref lo-ends idx))]
                         [hi (in-list (vector-ref hi-ends idx))]
                         [repr (in-vector reprs)])
              (if (ordinal-key? lo)
                  (sample-binary64 lo hi bytes)
                  ((representation-ordinal->repr repr) (random-integer lo hi))))
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
  (define one-ordinal (binary64->ordinal 1.0))
  (sample-ordinal-range one-ordinal (+ one-ordinal 3))

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
  (match-define (real-compiler pre vars var-reprs _ reprs _ _ _ _) compiler)
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

;; Returns an evaluator for a list of expressions.
;; Part 3: compute exact values using Rival's algorithm

(define (block-prepare-points compiler sampler)
  ;; If we're using the bf fallback, start at the max precision
  (define outcomes (make-hash))
  (define vars (real-compiler-vars compiler))
  (define var-reprs (real-compiler-var-reprs compiler))
  (define reprs (real-compiler-reprs compiler))
  (define assemble-point (real-compiler-assemble-point compiler))
  (define assemble-output (real-compiler-assemble-output compiler))
  (define exact-count (length (assemble-output (make-list (vector-length reprs) #f))))

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
               [repr (in-vector reprs)])
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
  (values (cons points exactss) outcomes))

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
  (define-values (results table2) (block-prepare-points compiler sampler))
  (define total (apply + (hash-values table2)))
  (when (> (hash-ref table2 'infinite 0.0) (* 0.2 total))
    (warn 'inf-points
          #:url "faq.html#inf-points"
          "~a% of points produce a very large (infinite) output. You may want to add a precondition."
          (~r (/ (- total (hash-ref table2 'infinite)) total 0.01) #:precision '(= 1))))
  (timeline-push! 'bogosity (combine-tables table table2))
  results)
