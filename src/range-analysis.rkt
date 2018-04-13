#lang racket
(require "syntax/syntax.rkt")
(provide (struct-out interval) interval-union interval-intersect
         make-empty-range-table make-range-table range-table-ref range-table-union range-table-intersect
         condition->range-table)

(module+ test
 (require rackunit))

;; NOTE: an interval can also be #f for an empty interval
(struct interval (l u l? u?) #:transparent)

(define (make-interval l u [l? #f] [u? #f])
  (and
   (not (and (= l -inf.0) l?))
   (not (and (= u +inf.0) u?))
   (cond
     [(< l u) (interval l u l? u?)]
     [(and (= l u) l? u?) (interval l u l? u?)]
     [else #f])))

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

    (make-interval l u l? u?)]
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
 (check-equal? (interval-intersect (interval 1 1 #t #t) (interval 1 2 #f #f)) #f)
 (check-equal? (interval-intersect (interval 1 2 #t #t) #f) #f)
 (check-equal? (interval-intersect #f #f) #f))

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

    (make-interval l u l? u?)]
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
 (check-equal? (interval-union (interval 0 1 #t #f) (interval 2 3 #f #t)) (interval 0 3 #t #t))
 (check-equal? (interval-union #f #f) #f)
 (check-equal? (interval-union #f (interval -inf.0 +inf.0 #f #f)) (interval -inf.0 +inf.0 #f #f)))

(define (interval-invert intvl)
  (match intvl
    [(interval -inf.0 +inf.0 _ _) #f]
    [(interval -inf.0 u _ u?) (interval u +inf.0 (not u?) #f)]
    [(interval l +inf.0 l? _) (interval -inf.0 l #f (not l?))]
    [_ (interval -inf.0 +inf.0 #f #f)]))

(module+ test
  (check-equal? (interval-invert (interval -inf.0 +inf.0 #f #f)) #f)
  (check-equal? (interval-invert (interval -inf.0 1 #f #f)) (interval 1 +inf.0 #t #f))
  (check-equal? (interval-invert (interval 1 +inf.0 #t #f)) (interval -inf.0 1 #f #f))
  (check-equal? (interval-invert (interval 1 2 #t #f)) (interval -inf.0 +inf.0 #f #f))
  (check-equal? (interval-invert #f) (interval -inf.0 +inf.0 #f #f)))


;; An `intervals` is a list of intervals: nonoverlapping, sorted, and with non-empty gaps between each

(define/contract (intervals-normalize ivals)
  (-> (listof interval?) (listof interval?))
  ;; Could be faster, but not asymptotically so
  (foldl intervals-union (list) (map list ivals)))

(define/contract (intervals-union ivals1 ivals2)
  (-> (listof interval?) (listof interval?) (listof interval?))
  (if (or (null? ivals1) (null? ivals2))
      (append ivals1 ivals2) ; Picks out the non-null one
      (match-let ([(interval lo1 hi1 lo1? hi1?) (car ivals1)]
                  [(interval lo2 hi2 lo2? hi2?) (car ivals2)])
        (cond 
         [(or (< hi1 lo2) (and (= hi1 lo2) (not hi1?) (not lo2?)))
          (cons (car ivals1) (intervals-union (cdr ivals1) ivals2))]
         [(or (< hi2 lo1) (and (= hi2 lo1) (not hi2?) (not lo1?)))
          (cons (car ivals2) (intervals-union ivals1 (cdr ivals2)))]
         [else
          ;; The termination argument here is that we are now taking the union of one fewer things, total
          ;; It is valid because here we are guaranteed that the intervals have no gap between them
          (intervals-union
           (list (interval-union (car ivals1) (car ivals2)))
           (intervals-union (cdr ivals1) (cdr ivals2)))]))))

(define/contract (intervals-intersect ivals1 ivals2)
  (-> (listof interval?) (listof interval?) (listof interval?))
  (if (or (null? ivals1) (null? ivals2))
      '()
      (match-let ([(interval lo1 hi1 lo1? hi1?) (car ivals1)]
                  [(interval lo2 hi2 lo2? hi2?) (car ivals2)])
        (define intersection (interval-intersect (car ivals1) (car ivals2)))
        (define rest
          (cond
          [(< hi1 hi2) (intervals-intersect (cdr ivals1) ivals2)]
          [(= hi1 hi2) (intervals-intersect (cdr ivals1) (cdr ivals2))]
          [else        (intervals-intersect ivals1 (cdr ivals2))]))
        (if intersection
            (cons intersection rest)
            rest))))

(define/contract (intervals-invert ivals)
  (-> (listof interval?) (listof interval?))
  (append
   (match ivals
     [(list)
      (interval -inf.0 +inf.0 #f #f)]
     [(list (interval lo hi lo? hi?) rest ...)
      (if (= lo -inf.0)
          '()
          (list (interval -inf.0 lo #f (not lo?))))])
   (let loop ([ivals ivals])
     (match ivals
       [(list) '()]
       [(list (interval lo hi lo? hi?))
        (if (= hi +inf.0)
            '()
            (list (interval hi +inf.0 (not hi?) #f)))]
       [(list (interval _ lo _ lo?) (and next (interval hi _ hi? _)) rest ...)
        (cons (interval lo hi (not lo?) (not hi?)) (loop (cons next rest)))]))))



(define (make-range-table x . intvls)
  (make-hash (list (cons x (intervals-normalize (filter identity intvls))))))

(define (make-empty-range-table)
  (make-hash))

;; NOTE: an range-table can also be #f for an invalid range-table 
(define (make-null-range-table)
  #f)

(define (range-table-ref rt x)
  (if rt
      (hash-ref rt x (list (interval -inf.0 +inf.0 #f #f)))
      '()))

(module+ test
  (define rt-x1 (make-range-table 'x (interval 1 3 #t #t)))
  (define rt-x2 (make-range-table 'x (interval 2 4 #t #t)))
  (define rt-x3 (make-range-table 'x (interval -3 -1 #f #f)))
  (define rt-y2 (make-range-table 'y (interval 2 4 #t #t)))
  (define rt-x-neginf (make-range-table 'x (interval -inf.0 1 #f #t)))
  (define rt-x-posinf (make-range-table 'x (interval 1 +inf.0 #t #f)))
  (define rt-x1y1 (make-range-table 'x (interval 1 3 #t #t)))
  (hash-set! rt-x1y1 'y (list (interval 2 4 #t #t)))

  (check-equal? (list (interval -inf.0 +inf.0 #f #f)) (range-table-ref rt-x1 'y))
  (check-equal? (range-table-ref rt-x2 'x) (range-table-ref rt-y2 'y))
  (check-equal? (range-table-ref #f 'x) '()))

(define (range-table-intersect table1 table2)
  (cond
    [(not table1) #f]
    [(not table2) #f]
    [else
     (define new-range-table (make-hash))
     (for ([key1 (hash-keys table1)])
       (if (hash-has-key? table2 key1)
           (hash-set! new-range-table key1 (intervals-intersect (hash-ref table1 key1) (hash-ref table2 key1)))
           (hash-set! new-range-table key1 (hash-ref table1 key1))))
     (for ([key2 (hash-keys table2)] #:unless (hash-has-key? new-range-table key2))
       (hash-set! new-range-table key2 (hash-ref table2 key2)))
     new-range-table]))

(module+ test  
  (check-equal? (list (interval 2 3 #t #t)) (hash-ref (range-table-intersect rt-x1 rt-x2) 'x))
  (check-equal? (list (interval 1 3 #t #t)) (hash-ref (range-table-intersect rt-x1 rt-y2) 'x))
  (check-equal? (list (interval 2 4 #t #t)) (hash-ref (range-table-intersect rt-x1 rt-y2) 'y))
  (check-equal? #f (range-table-intersect rt-x1 #f))
  (check-equal? '() (hash-ref (range-table-intersect rt-x3 rt-x1) 'x)))

(define (range-table-union table1 table2)
  (cond
    [(not table1) table2]
    [(not table2) table1]
    [else
     (define new-range-table (make-hash))
     (for ([key1 (hash-keys table1)] #:when (hash-has-key? table2 key1))
       (hash-set! new-range-table key1 (intervals-union (hash-ref table1 key1) (hash-ref table2 key1))))
     new-range-table]))

(module+ test
  (check-equal? (list (interval 1 4 #t #t)) (hash-ref (range-table-union rt-x1 rt-x2) 'x))
  (check-true (hash-empty? (range-table-union rt-x1 rt-y2)))
  (check-equal? rt-x1 (range-table-union rt-x1 #f)))

(define (range-table-invert table)
  (cond
    [(and table (= (count (compose not null?) (hash-values table)) 1))
     (match-define (list (list var itvls ...)) (filter (compose not null? cdr) (hash->list table)))
     (apply make-range-table var (intervals-invert itvls))]
    [else
     (make-empty-range-table)]))

(module+ test
  (check-equal? (range-table-invert rt-x-neginf) (make-range-table 'x (interval 1 +inf.0 #f #f)))
  (check-equal? (range-table-invert rt-x-posinf) (make-range-table 'x (interval -inf.0 1 #f #f)))
  (check-equal? (range-table-invert rt-x1y1) (make-empty-range-table))
  (check-equal? (range-table-invert #f) (make-empty-range-table)))

(define (flip-cmp cmp)
  (match cmp
    ['< '>]
    ['> '<]
    ['<= '>=]
    ['>= '<=]
    ['== '==]))

(define (parse-cmp cmp)
  (match cmp ['< <] ['> >] ['<= <=] ['>= >=] ['== =]))

(define (condition->range-table condition)
  (match condition
    [(list (and (or '< '> '<= '>= '==) cmp) (? number? a) (? number? b))
     (if ((parse-cmp cmp) a b)
         (make-empty-range-table)
         (make-null-range-table))]
    ['TRUE (make-empty-range-table)]
    ['FALSE (make-null-range-table)]
    [`(== ,(? variable? var) ,(? number? num))
     (make-range-table var (make-interval num num #t #t))]
    [`(< ,(? variable? var) ,(? number? num))
     (make-range-table var (make-interval -inf.0 num #f #f))]
    [`(< (fabs ,(? variable? var)) ,(? number? num))
     (make-range-table var (make-interval (- num) num #f #f))]
    [`(<= ,(? variable? var) ,(? number? num))
     (make-range-table var (make-interval -inf.0 num #f #t))]
    [`(<= (fabs ,(? variable? var)) ,(? number? num))
     (make-range-table var (make-interval (- num) num #t #t))]
    [`(== (fabs ,(? variable? var)) ,(? number? num))
     (make-range-table var (make-interval (- num) (- num) #t #t) (make-interval num num #t #t))]
    [`(> ,(? variable? var) ,(? number? num))
     (make-range-table var (make-interval num +inf.0 #f #f))]
    [`(> (fabs ,(? variable? var)) ,(? number? num))
     (make-range-table var (make-interval num +inf.0 #f #f) (make-interval -inf.0 (- num) #f #f))]
    [`(>= ,(? variable? var) ,(? number? num))
     (make-range-table var (make-interval num +inf.0 #t #f))]
    [`(>= (fabs ,(? variable? var)) ,(? number? num))
     (make-range-table var (make-interval num +inf.0 #t #f) (make-interval -inf.0 (- num) #f #t))]
    [(list (and (or '< '<= '== '>= '>) cmp) (? number? num) var) ; don't check for variable? here b/c fabs
     (condition->range-table (list (flip-cmp cmp) var num))]
    [(list (and (or '< '<= '== '>= '>) cmp) _ _) ; handle case of complex expressions
     (make-empty-range-table)]
    [(list (and (or '< '<= '> '>=) cmp) exprs ...)
     (if (not (equal? (filter number? exprs) (sort (filter number? exprs) (parse-cmp cmp))))
       #f
       (let
         ([from-left (last-number exprs)]
          [from-right (reverse (last-number (reverse exprs)))])
         (foldl range-table-intersect
                (make-empty-range-table)
                (for/list ([left from-left] [expr exprs] [right from-right]
                          #:unless (number? expr))
                  (range-table-intersect
                   (if left
                       (condition->range-table (list cmp left expr))
                       (make-empty-range-table))
                   (if right
                       (condition->range-table (list cmp expr right))
                       (make-empty-range-table)))))))]
    [(list '== exprs ...)
     (define num (get-all-equal-value exprs))
     (if num
         (foldl range-table-intersect
                (make-empty-range-table)
                (map (lambda (x) (make-range-table x (make-interval num num #t #t)))
                     (filter variable? exprs)))
         (make-null-range-table))]
    [(list '!= expr1 expr2) ; == and != are not inverses for more than two arguments
     (range-table-invert (condition->range-table `(== ,expr1 ,expr2)))]
     
    [`(and ,conds ...)
     (foldl range-table-intersect (make-empty-range-table) (map condition->range-table conds))]
    [`(or ,conds ...)
     (foldl range-table-union (make-null-range-table) (map condition->range-table conds))]
    [`(not ,cond1) (range-table-invert (condition->range-table cond1))]
    [_
     (make-empty-range-table)]))

(define (get-all-equal-value lst)
  (let ([nums (filter number? lst)])
    (if (foldl (lambda (x y) (and x y)) #t (map (lambda (x) (= x (car nums))) nums))
        (car nums)
        #f)))

(define (last-number lst)
  (let loop ([lst lst] [last #f])
    (match lst
      ['() '()]
      [(cons (? number? x) rest)
       (cons x (loop rest x))]
      [(cons _ rest)
       (cons last (loop rest last))])))

(module+ test
  (check-equal? (condition->range-table '(== 1 x 1 1 y 1 1 1)) (make-hash (list (list 'x (interval 1 1 #t #t)) (list 'y (interval 1 1 #t #t)))))
  (check-equal? (condition->range-table '(== 1 x 1 2 y 1 1 1)) #f)
  (check-equal? (condition->range-table '(> x 1)) (make-hash (list (list 'x (interval 1 +inf.0 #f #f)))))
  (check-equal? (condition->range-table '(>= x 1)) (make-hash (list (list 'x (interval 1 +inf.0 #t #f)))))
  (check-equal? (condition->range-table '(<= x 1)) (make-hash (list (list 'x (interval -inf.0 1 #f #t)))))
  (check-equal? (condition->range-table '(< x 1)) (make-hash (list (list 'x (interval -inf.0 1 #f #f)))))
  (check-equal? (condition->range-table '(< 1 1)) #f)
  (check-equal? (condition->range-table '(< 1 2)) (make-hash))
  (check-equal? (condition->range-table '(< x 1)) (make-hash (list (list 'x (interval -inf.0 1 #f #f)))))
  (check-equal? (condition->range-table '(< x y 2)) (make-hash (list (list 'x (interval -inf.0 2 #f #f)) (list 'y (interval -inf.0 2 #f #f)))))
  (check-equal? (condition->range-table '(< 1 x y 2)) (make-hash (list (list 'x (interval 1.0 2.0 #f #f)) (list 'y (interval 1.0 2.0 #f #f)))))
  (check-equal? (range-table-ref (condition->range-table '(< 1 2 3)) 'x) (list (interval -inf.0 +inf.0 #f #f)))
  (check-equal? (range-table-ref (condition->range-table '(< 10 2 3)) 'x) '())
  (check-equal? (range-table-ref (condition->range-table '(< 0 x 4 3)) 'x) '())
  (check-equal? (condition->range-table '(and (< x 1) (> x -1))) (make-hash (list (list 'x (interval -1.0 1.0 #f #f)))))
  (check-equal? (condition->range-table '(or (< x 1) (> x -1))) (make-hash (list (list 'x (interval -inf.0 +inf.0 #f #f)))))
  (check-equal? (condition->range-table '(or (< x -1) (> x 1))) (make-hash (list (list 'x (interval -inf.0 -1 #f #f) (interval 1 +inf.0 #f #f)))))
  (check-equal? (condition->range-table '(or (< x 1) (< x 2) (> x -1))) (make-hash (list (list 'x (interval -inf.0 +inf.0 #f #f)))))
  (check-equal? (condition->range-table '(and (< x 1) (< x 2) (> x -1))) (make-hash (list (list 'x (interval -1.0 1.0 #f #f)))))
  (check-equal? (condition->range-table '(not (< x 1))) (make-hash (list (list 'x (interval 1 +inf.0 #t #f)))))
  (check-equal? (condition->range-table '(not (> x 1))) (make-hash (list (list 'x (interval -inf.0 1 #f #t)))))
  (check-equal? (condition->range-table '(and (not (> x 1)) (not (< x -2)))) (make-hash (list (list 'x (interval -2.0 1.0 #t #t)))))
  ; this following two test tells us that we could have two representations of empty range-table
  ; therefore we use range-table-ref to hide the internal representation of range-table
  (check-equal? (range-table-ref (condition->range-table '(or (not (> x 1)) (not (< x -2)))) 'x) (list (interval -inf.0 +inf.0 #f #f)))
  (check-equal? (range-table-ref (condition->range-table '(not (> x +inf.0))) 'x) (list (interval -inf.0 +inf.0 #f #f)))
  (check-equal? (condition->range-table '(< (fabs x) 3)) (make-hash (list (list 'x (interval -3 3 #f #f)))))
  (check-equal? (condition->range-table '(<= (fabs x) 3)) (make-hash (list (list 'x (interval -3 3 #t #t)))))
  (check-equal? (condition->range-table '(> (fabs x) 3)) (make-hash (list (list 'x (interval -inf.0 -3 #f #f) (interval 3 +inf.0 #f #f)))))
  (check-equal? (condition->range-table '(>= 3 (fabs x))) (make-hash (list (list 'x (interval -3 3 #t #t)))))
  (check-equal? (condition->range-table '(<= 3 (fabs x))) (make-hash (list (list 'x (interval -inf.0 -3 #f #t) (interval 3 +inf.0 #t #f))))))
