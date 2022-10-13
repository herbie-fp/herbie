#lang racket
(require math/bigfloat rival)
(require "syntax/types.rkt" "timeline.rkt")

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
  (define whole-space (expt 2 (apply + (map representation-total-bits reprs))))
  (exact->inexact (/ (apply + (map (curryr hyperrect-weight reprs) hyperrects)) whole-space)))

(define (search-step ival-fn space reprs split-var)
  (match-define (search-space true false other) space)
  (define-values (true* false* other*)
    (for/fold ([true* true] [false* false] [other* '()]) ([rect (in-list other)])
      (define res (apply ival-fn rect))
      (match-define (ival err err?) (apply ival-and (map ival-error? res)))
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

(define (find-intervals ival-fn rects #:reprs reprs #:fuel [depth 128])
  (if (or (null? rects) (null? (first rects)))
      (map (curryr cons 'other) rects)
      (let loop ([space (apply make-search-space rects)] [n 0])
        (match-define (search-space true false other) space)

        (define wt (total-weight reprs true))
        (define wo (total-weight reprs other))
        ;; Since the initial rects need not be whole space (but the
        ;; missing area is implicitly "false") we don't measure the
        ;; size of the "false" set.
        (define wf (- 1 wt wo))
        (timeline-push! 'sampling n wt wo wf)

        (define n* (remainder n (length (first rects))))
        (if (or (>= n depth) (empty? (search-space-other space)))
            (append (search-space-true space) (search-space-other space))
            (loop (search-step ival-fn space reprs n*) (+ n 1))))))

