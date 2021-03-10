#lang racket

(require rackunit)
(require "alternative.rkt" "interface.rkt" "points.rkt")

(provide generate-pareto-curve)

(define (alt-score alt context repr)
  (errors-score (errors (alt-program alt) context repr)))

(define (paired-less? elem1 elem2)
  (let ([c1 (car elem1)] [c2 (car elem2)])
    (if (= c1 c2)
        (> (cdr elem1) (cdr elem2))
        (< c1 c2))))

; Start with the point (0, 0)
; Add the first set of points {(x, y)}
;   If x_i = x_j, take the point with higher y
;   As x increases, y should decrease; remove points
;     that break this property
; Take the next set of points and add via cartesian product to produce
;   (n * m) points
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
    (sort coords < #:key car)]))

(module+ test
  (define pts
    (for/list ([k (in-range 1 100)])
      (for/list ([n (in-range 1 1000 10)])
        (cons (+ k n) (- 1100 k n)))))
  (define pts* (generate-pareto-curve pts))
  (for/fold ([best +inf.0] #:result (void)) ([pt pts*])
    (check <= (cdr pt) best)
    (cdr pt)))