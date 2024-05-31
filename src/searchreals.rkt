#lang racket
(require math/bigfloat rival)
(require "syntax/types.rkt" "timeline.rkt" "errors.rkt" "pretty-print.rkt" "float.rkt" "config.rkt" "ground-truth.rkt")

(provide find-intervals hyperrect-weight)

(struct search-space (true false other))

(define (make-search-space . ranges)
  (search-space '() '() ranges))

(define (repr-round repr dir point)
  ((representation-repr->bf repr)
   (parameterize ([bf-rounding-mode dir])
     ((representation-bf->repr repr) point))))

(define (total-weight reprs)
  (expt 2 (apply + (map representation-total-bits reprs))))

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

(define (search-step ival-fn space ctx split-var)
  (match-define (search-space true false other) space)
  (define reprs (context-var-reprs ctx))
  (define-values (true* false* other*)
    (for/fold ([true* true] [false* false] [other* '()]) ([rect (in-list other)])
      (match-define (ival err err?) (rival-analyze ival-fn (list->vector rect)))
      (when (eq? err 'unsamplable)
        (warn 'ground-truth #:url "faq.html#ground-truth"
              "could not determine a ground truth"
              #:extra
              (for/list ([var (context-vars ctx)] [repr reprs] [ival rect])
                (define val
                  (value->string
                   ((representation-bf->repr repr)
                    (bigfloat-pick-point (ival-lo ival) (ival-hi ival)))
                   repr))
                (format "~a = ~a" var val))))
      (cond
       [err
        (values true* (cons (cons err rect) false*) other*)]
       [(not err?)
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

(define (make-sampling-table ctx true false other)
  (define reprs (context-var-reprs ctx))
  (define denom (total-weight reprs))
  (define true-weight (apply + (map (curryr hyperrect-weight reprs) true)))
  (define other-weight (apply + (map (curryr hyperrect-weight reprs) other)))
  (define out (make-hash))
  (hash-set! out 'valid (exact->inexact (/ true-weight denom)))
  (hash-set! out 'unknown (exact->inexact (/ other-weight denom)))
  (for ([(reason rect) (in-dict false)])
    (define weight (exact->inexact (/ (hyperrect-weight rect reprs) denom)))
    (hash-update! out reason (curry + weight) 0))
  (define total (apply + (hash-values out)))
  (hash-update! out 'precondition (curry + (- 1 total)) 0)
  (make-immutable-hash (hash->list out)))

(define (find-intervals ival-fn rects #:ctx ctx #:fuel [depth 128])
  (if (or (null? rects) (null? (first rects)))
      (map (curryr cons 'other) rects)
      (let loop ([space (apply make-search-space rects)] [n 0])
        (match-define (search-space true false other) space)
        (timeline-push! 'sampling n (make-sampling-table ctx true false other))

        (define n* (remainder n (length (first rects))))
        (if (or (>= n depth) (empty? (search-space-other space))
                (>= (length other) (expt 2 depth)))
            (cons
             (append (search-space-true space) (search-space-other space))
             (make-sampling-table ctx true false other))
            (loop (search-step ival-fn space ctx n*) (+ n 1))))))

