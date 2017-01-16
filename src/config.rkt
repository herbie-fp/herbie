#lang racket
(require racket/runtime-path)
(provide (all-defined-out))

(define-runtime-path report-output-path "../graphs/")
(define-runtime-path demo-output-path "../www/demo/")
(define-runtime-path viz-output-path "../www/viz/")
(define-runtime-path benchmark-path "../bench/")

;; Flag Stuff

(define all-flags
  #hash([precision . (double)]
        [setup . (simplify early-exit)]
        [generate . (rr taylor simplify)]
        [reduce . (regimes taylor simplify avg-error post-process)]
        [rules . (arithmetic polynomials fractions exponents trigonometry numerics)]))

(define default-flags
  #hash([precision . (double)]
        [setup . (simplify)]
        [generate . (rr taylor simplify)]
        [reduce . (regimes taylor simplify avg-error)]
        [rules . (arithmetic polynomials fractions exponents trigonometry)]))

(define (enable-flag! category flag)
  (define (update cat-flags) (set-add cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (disable-flag! category flag)
  (define (update cat-flags) (set-remove cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (has-flag? class flag)
  (set-member? (dict-ref (*flags*) class) flag))

(define *flags* (make-parameter (hash-copy default-flags)))

(define (changed-flags)
  (filter identity
          (for*/list ([(class flags) all-flags] [flag flags])
            (match* ((has-flag? class flag)
                     (parameterize ([*flags* default-flags]) (has-flag? class flag)))
              [(#t #t) #f]
              [(#f #f) #f]
              [(#t #f) (list 'enabled class flags)]
              [(#f #t) (list 'disabled class flag)]))))

(define ((flag type f) a b)
  (if (has-flag? type f) a b))

;; Number of points to sample for evaluating program accuracy
(define *num-points* (make-parameter 256))

;; Number of iterations of the core loop for improving program accuracy
(define *num-iterations* (make-parameter 3))

;; The step size with which arbitrary-precision precision is increased
;; DANGEROUS TO CHANGE
(define *precision-step* (make-parameter 256))

;; When doing a binary search in regime inference,
;; this is the fraction of the gap between two points that the search must reach
(define *epsilon-fraction* (/ 1 200))

;; In periodicity analysis,
;; this is how small the period of a function must be to count as periodic
(define *max-period-coeff* 20)

;; In localization, the maximum number of locations returned
(define *localize-expressions-limit* (make-parameter 4))

(define *binary-search-test-points* (make-parameter 16))

(define *herbie-version* "1.0")
