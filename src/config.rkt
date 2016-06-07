#lang racket
(require racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path report-output-path "../graphs/")
(define-runtime-path demo-output-path "../www/demo/")
(define-runtime-path viz-output-path "../www/viz/")
(define-runtime-path benchmark-path "../bench/")

;; Flag Stuff

(define *flags*
  (make-parameter
   #hash([precision . (double)]
         [setup    . (simplify)]
         [generate . (rr taylor simplify)]
         [reduce   . (regimes taylor simplify avg-error)]
         [rules . (arithmetic polynomials fractions exponents trigonometry)])))

(define (toggle-flag! category flag)
  (*flags*
   (hash-update (*flags*) category
		(λ (flag-list)
		  (if (member flag flag-list)
		      (remove flag flag-list)
		      (cons flag flag-list))))))

(define ((flag type f) a b)
  (if (member f (hash-ref (*flags*) type
                          (λ () (error "Invalid flag type" type))))
      a
      b))

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
