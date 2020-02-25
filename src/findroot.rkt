#lang racket
(require math/bigfloat)
(require "biginterval.rkt")
(require "programs.rkt")
(require "interface.rkt")

(define (done? ival)
  (and (not (ival-err? ival))
       (<= -1 (- (bigfloat->ordinal (ival-lo ival)) (bigfloat->ordinal (ival-hi ival))) 1)))

(define (could-zero? ival)
  (and (bf<= (ival-lo ival) 0.bf) (bf>= (ival-hi ival) 0.bf) (not (ival-err ival))))

(define (find-roots prog accuracy)
  (define repr (get-representation 'binary64))
  (parameterize ([*var-reprs* (map (λ (x) (cons x repr)) (program-variables prog))]
                 [bf-precision accuracy])
    (define fn (eval-prog prog 'ival repr))
    (define initial-ranges (map (const (ival -inf.bf +inf.bf #f #f)) (program-variables prog)))
    (for/list ([ivl (in-list (find-roots-interval fn initial-ranges 0))])
      ivl)))

(define (find-roots-interval fn ranges n)
  (define range (list-ref ranges n))
  (define n* (if (= (+ n 1) (length ranges)) 0 (+ n 1)))
  (cond
   [(not (could-zero? (apply fn ranges)))
    '()]
   [(andmap done? ranges)
    (list ranges)]
   [(done? range)
    (find-roots-interval fn ranges n*)]
   [else
    (define midpoint
      (ordinal->bigfloat
       (floor (/ (+ (bigfloat->ordinal (ival-lo range))
                    (bigfloat->ordinal (ival-hi range)))
                 2))))
    (define range-lo (struct-copy ival range [hi midpoint]))
    (define range-hi (struct-copy ival range [lo midpoint]))
    (append
     (find-roots-interval fn (list-set ranges n range-lo) n*)
     (find-roots-interval fn (list-set ranges n range-hi) n*))]))
