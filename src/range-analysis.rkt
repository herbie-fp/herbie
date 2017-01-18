#lang racket

(module+ test
 (require rackunit))

;; NOTE: an interval can also be #f for an empty interval
(struct interval (l u l? u?) #:transparent)

(define (interval-intersect interval1 interval2)
  (cond
   [(and interval1 interval2)
    (match-define (interval l1 u1 l1? u1?) interval1)
    (match-define (interval l2 u2 l2? u2?) interval2)

    (define l? 
     (cond [(< l1 l2) l2?]
           [(= l1 l2) (and l1? l2?)]
           [(> l1 l2) l1?]))
    (define u?
     (cond [(< u1 u2) u1?]
           [(= u1 u2) (and u1? u2?)]
           [(> u1 u2) u2?]))
    (define l (max l1 l2))
    (define u (min u1 u2))

    (if (or (< u1 l2) (< u2 l1) (and (= l u) (not (and l? u?))))
        #f
        (interval l u l? u?))]
   [else #f]))

(module+ test
 ;; Overlapping on a range
 (check-equal? (interval-intersect (interval 0 2 #t #t) (interval 1 3 #t #t)) (interval 1 2 #t #t))
 (check-equal? (interval-intersect (interval 0 2 #t #t) (interval 1 3 #f #t)) (interval 1 2 #f #t))
 (check-equal? (interval-intersect (interval 0 2 #t #f) (interval 1 3 #t #t)) (interval 1 2 #t #f))
 (check-equal? (interval-intersect (interval 0 2 #t #f) (interval 1 3 #f #t)) (interval 1 2 #f #f))
 ;; Overlapping on a point
 (check-equal? (interval-intersect (interval 0 2 #t #t) (interval 2 3 #t #t)) (interval 2 2 #t #t))
 (check-equal? (interval-intersect (interval 0 2 #t #f) (interval 2 3 #t #t)) #f)
 ;; No overlap
 (check-equal? (interval-intersect (interval 0 1 #t #t) (interval 2 3 #t #t)) #f)
 ;; Single point intervals
 (check-equal? (interval-intersect (interval 1 1 #t #t) (interval 0 2 #f #f)) (interval 1 1 #t #t))
 (check-equal? (interval-intersect (interval 1 1 #t #t) (interval 1 2 #f #f)) #f))

(define (interval-union interval1 interval2)
  (cond
   [(and interval1 interval2)
    (match-define (interval l1 u1 l1? u1?) interval1)
    (match-define (interval l2 u2 l2? u2?) interval2)

    (define l? 
     (cond [(< l1 l2) l1?]
           [(= l1 l2) (or l1? l2?)]
           [(> l1 l2) l2?]))
    (define u?
     (cond [(< u1 u2) u2?]
           [(= u1 u2) (or u1? u2?)]
           [(> u1 u2) u1?]))
    (define l (min l1 l2))
    (define u (max u1 u2))

    (interval l u l? u?)]
   [interval1 interval1]
   [else interval2]))

(module+ test
 ;; Overlapping on a range
 (check-equal? (interval-union (interval 0 2 #t #t) (interval 1 3 #t #t)) (interval 0 3 #t #t))
 (check-equal? (interval-union (interval 0 2 #t #f) (interval 1 3 #f #f)) (interval 0 3 #t #f))
 (check-equal? (interval-union (interval 0 2 #f #t) (interval 1 3 #f #f)) (interval 0 3 #f #f))
 ;; Single point intervals
 (check-equal? (interval-union (interval 0 0 #t #t) (interval 1 3 #f #f)) (interval 0 3 #t #f))
 (check-equal? (interval-union (interval 2 2 #t #t) (interval 2 3 #f #f)) (interval 2 3 #t #f))
 (check-equal? (interval-union (interval 0 0 #t #t) (interval 3 3 #t #t)) (interval 0 3 #t #t))
 ;; Disjoint intervals
 (check-equal? (interval-union (interval 0 1 #t #f) (interval 2 3 #f #t)) (interval 0 3 #t #t)))

