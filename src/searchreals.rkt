#lang racket
(require math/bigfloat rival)
(require "common.rkt" "programs.rkt" "interface.rkt")

(module+ test (require rackunit))

(provide find-intervals hyperrect-weight)

(struct search-space (true false other))

(define (make-search-space . ranges)
  (search-space '() '() ranges))

(define (repr-round repr dir point)
  ((representation-repr->bf repr)
   (parameterize ([bf-rounding-mode dir])
     ((representation-bf->repr repr) point))))

(define (hyperrect-weight hyperrect reprs)
  (apply * (for/list ([interval (in-list hyperrect)] [repr (in-list reprs)])
             (define ->ordinal (compose (representation-repr->ordinal repr)
                                        (representation-bf->repr repr)))
             (+ 1 (- (->ordinal (ival-hi interval)) (->ordinal (ival-lo interval)))))))

(define (midpoint repr lo hi)
  ; Midpoint is taken in repr-space, but values are stored in bf
  (define <-ordinal (compose (representation-repr->bf repr) (representation-ordinal->repr repr)))
  (define ->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))

  (define lower (<-ordinal (floor (/ (+ (->ordinal hi) (->ordinal lo)) 2))))
  (define higher (repr-round repr 'up (bfnext lower))) ; repr-next

  (and (bf>= lower lo) (bf<= higher hi) ; False if lo and hi were already close together
       (cons lower higher)))

(define (total-weight reprs hyperrects)
  (exact->inexact (apply + (map (curryr hyperrect-weight reprs) hyperrects))))

(define (search-step ival-fn space reprs split-var)
  (match-define (search-space true false other) space)
  (define-values (true* false* other*)
    (for/fold ([true* true] [false* false] [other* '()]) ([rect (in-list other)])
      (define res (apply ival-fn rect))
      (cond
       [(or (ival-err res) (not (ival-hi res)))
        (values true* (cons rect false*) other*)]
       [(and (not (ival-err? res)) (ival-lo res))
        (values (cons rect true*) false* other*)]
       [else
        (define range (list-ref rect split-var))
        (define repr (list-ref reprs split-var))
        (match (midpoint repr (ival-lo range) (ival-hi range))
          [(cons midleft midright)
           (define rect-lo (list-set rect split-var (ival (ival-lo range) midleft)))
           (define rect-hi (list-set rect split-var (ival midright (ival-hi range))))
           (values true* false* (list* rect-lo rect-hi other*))]
          [#f
           (values true* false* (cons rect other*))])])))
  (search-space true* false* other*))

(define (find-intervals ival-fn rects #:reprs reprs #:fuel [depth 128])
  (define num-vars (length (first rects)))
  (if (= num-vars 0)
      (map (curryr cons 'other) rects)
      (let loop ([space (apply make-search-space rects)] [n 0])
        (match-define (search-space true false other) space)

        (define wt (total-weight reprs true))
        (define wf (total-weight reprs false))
        (define wo (total-weight reprs other))
        #;(eprintf "~a: ~a T + ~a F + ~a O :: ~a saved, ~a good\n"
                 n (length true) (length false) (length other)
                 (/ wf (+ wt wf wo)) (/ wt (+ wt wo)))

        (define n* (remainder n num-vars))
        (if (or (>= n depth) (empty? (search-space-other space)))
            (append (search-space-true space) (search-space-other space))
            (loop (search-step ival-fn space reprs n*) (+ n 1))))))

