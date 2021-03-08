#lang racket

(require rackunit)
(require "alternative.rkt" "interface.rkt" "points.rkt")

(provide pareto-measure-alts pareto-measure-pnts compute-pareto-curve)

(define (alt-score alt context repr)
  (errors-score (errors (alt-program alt) context repr)))

(define (paired-less? elem1 elem2)
  (let ([c1 (car elem1)] [c2 (car elem2)])
    (if (= c1 c2)
        (> (cdr elem1) (cdr elem2))
        (< c1 c2))))

; Accepts a (sorted) list of points, returns the area under a curve and above the line
; between (x-min, y-min) and (y-min, y-max).
(define (pareto-area pts)
  (match-define (list (cons xs ys) ...) pts)

  ; Find the smallest rectangle the contains all the points
  (define x-min (first xs))
  (define x-max (last xs))
  (define y-min (argmin identity ys))
  (define y-max (argmax identity ys))
  (define area/2 (* 1/2 (- x-max x-min) (- y-max y-min)))

  (cond
   [(zero? area/2) 0]
   [else
    ; Transform so (x-min, y-min) is the origin
    (define xs* (map (curryr - x-min) xs))
    (define ys* (map (curryr - y-min) ys))
    (define pts* (map cons xs* ys*))
    (define rsum ; triangular riemann sum
      (let loop ([p0 (car pts*)] [ps (cdr pts*)])
        (cond
        [(null? ps) 0]
        [else
          (define dx (- (caar ps) (car p0)))
          (define dy (- (cdar ps) (cdr p0)))
          (+ (* 1/2 dx dy) (* dx (cdr p0))
            (loop (car ps) (cdr ps)))])))
    (/ (- rsum area/2) area/2)]))

; Measure the pareto curve of a test
(define (pareto-measure-alts alts context repr)
  (cond
   [(< (length alts) 2) 0]
   [else
    (define bits (representation-total-bits repr))
    (define scores (map (λ (x) (- bits (alt-score x context repr))) alts))
    (define costs (map alt-cost alts))
    (define paired (map cons costs scores))
    (define paired* (sort paired paired-less?))
    (pareto-area paired*)]))

; Measure the area under the pareto curve (unsorted points)
(define (pareto-measure-pnts pnts) 
  (cond
   [(< (length pnts) 2) 0]
   [else
    (define pnts* (sort pnts paired-less?))
    (pareto-area pnts*)]))

; In a nutshell,
;
; Start with the point (0, 0)
; Add the first set of points {(x, y)}
;   If x_i = x_j, take the point with higher y
;   As x increases, y should also increase; remove points
;     that breaks this property
; Take the next set of points and add via cartesian product to produce
;   n x m points
;   Repeat the process above ...
;
(define (sum-pareto-pnts pts)
  (let loop ([pts pts] [h (make-hash `((0 . 0)))]) ; keep a hash of costs and partial sums
    (cond
     [(null? pts) h]
     [(null? (car pts)) (loop (cdr pts) h)]
     [else
      (define h* (make-hash))
      (for* ([(x y) (in-hash h)] [pt (car pts)])  ; make a new hash: h + pts, dedup by taking max
        (hash-update! h* (+ x (car pt))
                      (λ (x) (max x (+ y (cdr pt))))
                      (+ y (cdr pt))))
      (for/fold ([best 0]) ([x (sort (hash-keys h*) <)]) 
        (let ([y (hash-ref h* x)])  ; as cost increases, the sums must increase, remove decreased points
          (cond
           [(> y best) y]
           [else
            (hash-remove! h* x)
            best])))
      (loop (cdr pts) h*)])))

; Create a pareto curve across tests
(define (compute-pareto-curve pts)
  (cond
   [(null? pts) '()]
   [else
    (define pts* (map (curryr sort paired-less?) pts))
    (define coords (hash->list (sum-pareto-pnts pts*)))
    (sort coords < #:key car)]))

(module+ test
  (define pts
    (for/list ([k (in-range 1 100)])
      (for/list ([n (in-range 1 1000 10)])
        (cons (+ k n) (* k n)))))
  (define pareto (compute-pareto-curve pts))
  (pareto-measure-pnts pareto))