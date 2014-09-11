#lang racket

(provide *flags* flag toggle-flag!
         *num-points* *precision-step* *num-iterations*
         *default-test-value* *epsilon-fraction*
         *goal-cost-improvement*
         *max-period-coeff*)

;; Flag Stuff

(define *flags*
  (make-parameter
   #hash([generate . (simplify rm)]
         [reduce   . (regimes zach taylor)]
         [regimes  . (recurse)]
         [simplify . ()]
         [setup    . (simplify periodicity)])))

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
(define *num-points* (make-parameter 1024))

;; Number of iterations of the core loop for improving program accuracy
(define *num-iterations* (make-parameter 2))

;; The step size with which arbitrary-precision precision is increased
;; DANGEROUS TO CHANGE
(define *precision-step* (make-parameter 8))

;; When doing a binary search in regime inference,
;; axes not being split on are set to this value
(define *default-test-value* 0)

;; When doing a binary search in regime inference,
;; this is the fraction of the gap between two points that the search must reach
(define *epsilon-fraction* (/ 1 200))

;; In the old simplification algorithm,
;; this is how many bits more accurate the result must be than the input to count as simpler
(define *goal-cost-improvement* 4)

;; In periodicity analysis,
;; this is how small the period of a function must be to count as periodic
(define *max-period-coeff* 20)
