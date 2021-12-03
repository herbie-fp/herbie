#lang racket

(require rackunit)
(require "alternative.rkt" "interface.rkt" "points.rkt")

(provide generate-pareto-curve)

(define *pareto-ensure-convex* (make-parameter #t))

(define (alt-score alt context repr)
  (errors-score (errors (alt-program alt) context repr)))

(define (paired-less? elem1 elem2)
  (let ([c1 (car elem1)] [c2 (car elem2)])
    (if (= c1 c2)
        (> (cdr elem1) (cdr elem2))
        (< c1 c2))))

;;; For testing ;;;

(define (monotonically-decreasing? pts)
  (let loop ([x 0] [y +inf.0] [pts pts])
    (cond
     [(null? pts) #t]
     [(or (< (caar pts) x) (> (cdar pts) y)) #f]
     [else (loop (caar pts) (cdar pts) (cdr pts))])))

(define (convex? pts)
  (let loop ([pts pts])
    (match pts
     [(list p0 p1 p2 pns ...)
      (define m01 (/ (- (cdr p1) (cdr p0)) (- (car p1) (car p0))))
      (define m12 (/ (- (cdr p2) (cdr p1)) (- (car p2) (car p1))))
      (if (< m12 m01)
          #f
          (loop (cdr pts)))]
     [_ #t])))

; Takes a set of monotonically decreasing points (x, y), x >= 0
; and returns the subset of them that form a convex function
(define (convex-points pts)
  (let loop ([pts* '()] [pts pts])
    (match pts
     [(list p0 p1 p2 pns ...)
      (define m01 (/ (- (cdr p1) (cdr p0)) (- (car p1) (car p0))))
      (define m12 (/ (- (cdr p2) (cdr p1)) (- (car p2) (car p1))))
      ; if { p0, p1, p2 } are not convex:
      ;   discard p1
      ;   try backtracking one point (if not continue)
      ; else move forward one point
      (if (< m12 m01)
          (if (null? pts*)
              (loop pts* (append (list p0 p2) pns))
              (loop (cdr pts*) (append (list (car pts*) p0 p2) pns)))
          (loop (cons (car pts) pts*) (cdr pts)))]
     [_ (append (reverse pts*) pts)])))

; Take the first set of points {(x, y)}
;   If x_i = x_j, take the point with higher y
;   As x increases, y should decrease;
;     remove points that break this property
; Add the remaining points to the next set of points (cartesian product)
;   to form a new set of (n * m) points
;   Repeat the process above ...
(define (sum-pareto-pnts pts)
  (let loop ([pts pts] [h (make-hash `((0 . 0)))]) ; keep a hash of costs and partial sums
    (cond
     [(null? pts) h]
     [(null? (car pts)) (loop (cdr pts) h)]
     [else
      (define h* (make-hash))
      (for* ([(x y) (in-hash h)] [pt (car pts)])  ; make a new hash: h + pts, dedup by taking max
        (hash-update! h* (+ x (car pt))
                      (λ (x) (min x (+ y (cdr pt))))
                      (+ y (cdr pt))))
      (for/fold ([best +inf.0]) ([x (sort (hash-keys h*) <)]) 
        (let ([y (hash-ref h* x)])  ; as x increases, y must decrease; remove increased points
          (cond
           [(< y best) y]
           [else
            (hash-remove! h* x)
            best])))
      (loop (cdr pts) h*)])))

; Create a pareto curve across tests
(define/contract (generate-pareto-curve pts)
  (-> (listof cons?) (listof cons?))
  (cond
   [(null? pts) '()]
   [else
    (define pts* (map (curryr sort paired-less?) pts))
    (define coords (hash->list (sum-pareto-pnts pts*)))
    (define sorted (sort coords < #:key car))
    (if (*pareto-ensure-convex*)
        (convex-points sorted)
        sorted)]))

(module+ test
  (define pts
    (for/list ([k (in-range 1 100)])
      (for/list ([n (in-range 1 1000 10)])
        (cons (+ k n) (- 1100 k n)))))
  (define pts* (generate-pareto-curve pts))
  (check-true (monotonically-decreasing? pts*) pts*)
  (when (*pareto-ensure-convex*)
    (check-true (convex? pts*) pts*)))
