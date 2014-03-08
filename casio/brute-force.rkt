#lang racket

(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/redgreen)
(require casio/simplify)

(provide brute-force-search)

;; We can now search to find the best expression.
;; This is an A* search internally.

(define (generate-alternatives alt)
  (map simplify (remove-duplicates (alt-rewrite-tree alt))))

(define (step options done iters)
  (define (duplicate? altn)
    (member altn (append done options)))

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
  (search-options (list (simplify alt0)) '() iters))
