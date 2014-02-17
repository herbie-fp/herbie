#lang racket

(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/redgreen)

(define (alternative<>? alt1 alt2)
  "Compare two alternatives; return if incomparable.
   Compares first by a lattice order on points, then by program cost."

  (let ([comparisons (errors-compare (alt-errors alt1) (alt-errors alt2))])
    (and (member '< comparisons) (member '> comparisons))))

(define (alternative<? alt1 alt2)
  "Compare two alternatives.
   Compares first by a lattice order on points, then by program cost."

  (let ([comparisons (errors-compare (alt-errors alt1) (alt-errors alt2))])
    (or (andmap (negate (curry eq? '>)) comparisons)
        (< (alt-cost alt1) (alt-cost alt2)))))

(define (alt-program=? alt1 alt2)
  (equal? (program-body (alt-program alt1))
          (program-body (alt-program alt2))))

;; We can now search to find the best expression.
;; This is an A* search internally.

(define (generate-alternatives alt)
  (remove-duplicates (alt-rewrite-tree alt)))

(define (step options done)
  (define (duplicate? alt)
    (memf (curry alt-program=? alt) (append done options)))

  (let* ([parent (car options)]
         [rest (cdr options)]
         [children (generate-alternatives parent)])
    (values
     (sort (append rest (filter (negate duplicate?) children)) alternative<?)
     (cons parent done))))

(define (search-options options done iters)
  (cond
   [(and (pair? done) (green? (car done)))
    (car done)]
   [(or (null? options) (= iters 0))
    #f] ; Didn't find anything [:(]
   [#t
    (let-values ([(options* done*) (step options done)])
      (search-options options* done* (- iters 1)))]))

(define (brute-force-search alt0 iters)
  "Brute-force search for a better version of `prog`,
   giving up after `iters` iterations without progress"

  (search-options (list alt0) '() iters))
