#lang racket

(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/redgreen)

(provide brute-force-search)

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

(define (step options done iters)
  (define (duplicate? alt)
    (memf (curry alt-program=? alt) (append done options)))

  (let* ([parent (car options)]
         [rest (cdr options)]
         [children (generate-alternatives parent)]
         [options* (sort (append rest
                                 (filter (negate duplicate?) children))
                         alternative<?)])
    (debug (alt-change (car options*)) #:from (list 'bfs iters))
    (values options* (cons parent done))))

(define (search-options options done iters)
  (cond
   [(and (pair? done) (green? (car done)))
    (car done)]
   [(or (null? options) (= iters 0)) ; Didn't find anything [:(]
    (car (sort (append options done) alternative<?))]
   [#t
    (let-values ([(options* done*) (step options done iters)])
      (search-options options* done* (- iters 1)))]))

(define (brute-force-search alt0 iters)
  "Brute-force search for a better version of `prog`,
   giving up after `iters` iterations without progress"

  (debug alt0 "for" iters #:from 'bfs #:tag 'enter)
  (search-options (list alt0) '() iters))
