#lang racket

(require herbie/common)

(provide (all-defined-out))

(define (two-sided-log2 x)
  (cond [(positive? x)
	 (log2 (add1 x))]
	[(negative? x)
	 (- (log2 (add1 (- x))))]
	[#t 0]))
