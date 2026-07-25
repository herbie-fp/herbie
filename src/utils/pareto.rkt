#lang typed/racket

(require typed/racket/unsafe)

(unsafe-provide (struct-out pareto-point) pareto-map pareto-union pareto-combine)

(struct pareto-point ([cost : Real] [error : Real] [data : Any]) #:prefab)

(: ppt->pt (-> pareto-point (Listof Real)))
(define (ppt->pt ppt)
  (list (pareto-point-cost ppt) (pareto-point-error ppt)))

(: pt->ppt (-> (Listof Any) pareto-point))
(define (pt->ppt pt)
  (pareto-point (assert (first pt) real?) (assert (second pt) real?) (list)))

(: pareto-shift (-> pareto-point (Listof pareto-point) (Listof pareto-point)))
(define (pareto-shift ppt0 frontier)
  (match-define (pareto-point cost0 err0 _) ppt0)
  (for/list ([ppt (in-list frontier)])
    (match-define (pareto-point cost err _) ppt)
    (pareto-point (+ cost0 cost) (+ err0 err) (list))))

(: pareto-compare (-> pareto-point pareto-point (U '= '< '> '<>)))
(define (pareto-compare pt1 pt2)
  (match-define (pareto-point cost1 err1 data1) pt1)
  (match-define (pareto-point cost2 err2 data2) pt2)
  (cond
    [(and (= cost1 cost2) (= err1 err2)) '=]
    [(and (<= cost1 cost2) (<= err1 err2)) '<]
    [(and (>= cost1 cost2) (>= err1 err2)) '>]
    [else '<>]))

(: pareto-map (-> (-> Any Any) (Listof pareto-point) (Listof pareto-point)))
(define (pareto-map f curve)
  (for/list ([ppt (in-list curve)])
    (struct-copy pareto-point ppt [data (f (pareto-point-data ppt))])))

;; Takes two lists of `pareto-point` structs that are Pareto-optimal
;; and returns the Pareto-optimal subset of their union.
;; The curves most be sorted using the same method.
(: pareto-union
   (->* ((Listof pareto-point) (Listof pareto-point))
        (#:combine (-> Any Any Any))
        (Listof pareto-point)))
(define (pareto-union curve1
                      curve2
                      #:combine [combine (lambda (a b) (append (assert a list?) (assert b list?)))])
  (let loop ([curve1 curve1]
             [curve2 curve2])
    ; The curve is sorted so that highest accuracy is first
    (match* (curve1 curve2)
      [('() _) curve2]
      [(_ '()) curve1]
      [((cons ppt1 rest1) (cons ppt2 rest2))
       (match (pareto-compare ppt1 ppt2)
         ['< (loop curve1 rest2)]
         ['> (loop rest1 curve2)]
         ['=
          (define joint-data (combine (pareto-point-data ppt1) (pareto-point-data ppt2)))
          (define joint (struct-copy pareto-point ppt1 [data joint-data]))
          (cons joint (loop rest1 rest2))]
         ['<>
          (if (< (pareto-point-error ppt1) (pareto-point-error ppt2))
              (cons ppt1 (loop rest1 curve2))
              (cons ppt2 (loop curve1 rest2)))])])))

;; Takes a Pareto frontier and returns the subset of
;; points that are convex.
(: pareto-convex (-> (Listof pareto-point) (Listof pareto-point)))
(define (pareto-convex ppts)
  (pareto-convex-loop '() ppts))

(: pareto-convex-loop (-> (Listof pareto-point) (Listof pareto-point) (Listof pareto-point)))
(define (pareto-convex-loop ppts* ppts)
  (match ppts
    [(list p0 p1 p2 pns ...)
     (match-define (pareto-point p0x p0y _) p0)
     (match-define (pareto-point p1x p1y _) p1)
     (match-define (pareto-point p2x p2y _) p2)
     ; if { p0, p1, p2 } are not convex:
     ;   discard p1
     ;   try backtracking one point (if not continue)
     ; else move forward one point
     (define m01 (/ (- p1y p0y) (- p1x p0x)))
     (define m12 (/ (- p2y p1y) (- p2x p1x)))
     (match* ((> m12 m01) (null? ppts*))
       [(#t #t) (pareto-convex-loop ppts* (append (list p0 p2) pns))]
       [(#t #f) (pareto-convex-loop (rest ppts*) (append (list (first ppts*) p0 p2) pns))]
       [(#f _) (pareto-convex-loop (cons p0 ppts*) (append (list p1 p2) pns))])]
    [_ (append (reverse ppts*) ppts)]))

;; Takes a list of `pareto-point` structs
;; and returns the Pareto-optimal subset.
(: pareto-minimize (-> (Listof pareto-point) (Listof pareto-point)))
(define (pareto-minimize ppts)
  (define ppts* (sort ppts pareto-point<?))
  (for/fold ([minimized '()]) ([ppt (in-list ppts*)])
    (pareto-union (list ppt) minimized)))

(: pareto-point<? (-> pareto-point pareto-point Boolean))
(define (pareto-point<? ppt1 ppt2)
  (< (pareto-point-cost ppt1) (pareto-point-cost ppt2)))

;; Creates a synthetic frontier from multiple frontiers
;; as described in the ARITH '21 paper.
(: pareto-combine (->* ((Listof (Listof (Listof Any)))) (#:convex? Boolean) (Listof (Listof Real))))
(define (pareto-combine frontiers #:convex? [convex? #f])
  (define frontiers*
    (map (lambda ([frontier : (Listof (Listof Any))]) (pareto-minimize (map pt->ppt frontier)))
         frontiers))
  (map ppt->pt
       (foldl (lambda ([frontier : (Listof pareto-point)] [combined : (Listof pareto-point)])
                (if (null? combined)
                    (pareto-convex-if convex? frontier)
                    (pareto-convex-if convex? (pareto-combine-frontier combined frontier))))
              '()
              frontiers*)))

(: pareto-convex-if (-> Boolean (Listof pareto-point) (Listof pareto-point)))
(define (pareto-convex-if convex? frontier)
  (if convex?
      (pareto-convex frontier)
      frontier))

(: pareto-combine-frontier (-> (Listof pareto-point) (Listof pareto-point) (Listof pareto-point)))
(define (pareto-combine-frontier combined frontier)
  (foldl (lambda ([ppt : pareto-point] [combined* : (Listof pareto-point)])
           (define ppts (pareto-minimize (pareto-shift ppt frontier)))
           (pareto-union ppts combined*))
         '()
         combined))

(module+ test
  (require typed/rackunit)

  (: make-pareto-point (-> (Listof Any) pareto-point))
  (define (make-pareto-point pt)
    (match pt
      [(list cost err altns ...) (pareto-point (assert cost real?) (assert err real?) altns)]))

  (: pareto-point-error<? (-> pareto-point pareto-point Boolean))
  (define (pareto-point-error<? ppt1 ppt2)
    (< (pareto-point-error ppt1) (pareto-point-error ppt2)))

  (define (make-pareto [pts : (Listof (Listof Any))])
    (sort (map make-pareto-point pts) pareto-point-error<?))

  (: pareto->list (-> pareto-point (Listof Any)))
  (define (pareto->list ppt)
    (match ppt
      [(pareto-point cost err altns) (list* cost err (assert altns list?))]))

  (: list-first<? (-> (Listof Any) (Listof Any) Boolean))
  (define (list-first<? pt1 pt2)
    (< (assert (first pt1) real?) (assert (first pt2) real?)))

  (define (from-pareto [pts : (Listof pareto-point)])
    (sort (map pareto->list pts) list-first<?))

  (define (pareto-add [curve : (Listof pareto-point)] [d : Any] [c : Real] [e : Real])
    (pareto-union (list (pareto-point c e (list d))) curve))

  (check-equal? (from-pareto (make-pareto '((1 5 a) (2 3 b) (5 1 a b)))) '((1 5 a) (2 3 b) (5 1 a b)))
  (check-equal? (from-pareto (pareto-add (make-pareto '()) 'a 1 5)) '((1 5 a)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (5 1 b))) 'c 3 3))
                '((1 5 a) (3 3 c) (5 1 b)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b))) 'c 5 1))
                '((1 5 a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((3 3 b) (5 1 c))) 'a 1 5))
                '((1 5 a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 1 5))
                '((1 5 d a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 3 3))
                '((1 5 a) (3 3 d b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 2 2))
                '((1 5 a) (2 2 d) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 1 a))) 'b 1 3)) '((1 1 a))))
