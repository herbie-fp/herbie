#lang racket

(require "../utils/errors.rkt"
         "../syntax/float.rkt"
         "../utils/pretty-print.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../syntax/rival.rkt")

(provide find-intervals
         hyperrect-weight)

(struct search-space (true false other))
(struct search-node (rect [hint #:mutable] [children #:mutable] [dropped #:mutable]))

(define (make-search-space . ranges)
  (search-space '()
                '()
                (for/list ([range (in-list ranges)])
                  (search-node range #f #f #f))))

(define (total-weight reprs)
  (expt 2 (apply + (vector->list (vector-map representation-total-bits reprs)))))

(define (hyperrect-weight hyperrect reprs)
  (apply *
         (for/list ([interval (in-list hyperrect)]
                    [repr (in-vector reprs)])
           (define ->ordinal
             (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))
           (+ 1 (- (->ordinal (ival-hi interval)) (->ordinal (ival-lo interval)))))))

(define (search-step compiler space split-var)
  (define vars (real-compiler-vars compiler))
  (define reprs (real-compiler-var-reprs compiler))
  (match-define (search-space true false other) space)
  (define-values (true* false* other*)
    (for/fold ([true* true]
               [false* false]
               [other* '()])
              ([node (in-list other)])
      (define rect (search-node-rect node))
      (match-define (list err-ival hint* converged?)
        (real-compiler-analyze compiler (list->vector rect) (search-node-hint node)))
      (set-search-node-hint! node hint*)
      (define err (ival-lo err-ival))
      (define err? (ival-hi err-ival))
      (when (eq? err 'unsamplable)
        (warn 'ground-truth
              #:url "faq.html#ground-truth"
              "could not determine a ground truth"
              #:extra (for/list ([var (in-vector vars)]
                                 [repr (in-vector reprs)]
                                 [iv rect])
                        (define val
                          (value->string ((representation-bf->repr repr)
                                          (bigfloat-pick-point (ival-lo iv) (ival-hi iv)))
                                         repr))
                        (format "~a = ~a" var val))))
      (cond
        [err
         (set-search-node-dropped! node #t)
         (values true* (cons node false*) other*)]
        [(and (not err?) converged?) (values (cons node true*) false* other*)]
        [else
         (define range (list-ref rect split-var))
         (define repr (vector-ref reprs split-var))
         (match (two-midpoints repr (ival-lo range) (ival-hi range))
           [(cons midleft midright)
            (define rect-lo (list-set rect split-var (ival (ival-lo range) midleft)))
            (define rect-hi (list-set rect split-var (ival midright (ival-hi range))))
            (define node-lo (search-node rect-lo hint* #f #f))
            (define node-hi (search-node rect-hi hint* #f #f))
            (set-search-node-children! node (list node-lo node-hi))
            (values true* false* (list* node-lo node-hi other*))]
           [#f (values true* false* (cons node other*))])])))
  (search-space true* false* other*))

(define (make-sampling-table reprs true false other)
  (define denom (total-weight reprs))
  (define (weight nodes)
    (for/sum ([node (in-list nodes)]) (hyperrect-weight (search-node-rect node) reprs)))
  (define true-weight (weight true))
  (define false-weight (weight false))
  (define other-weight (weight other))
  (define out (make-hash))
  (hash-set! out 'valid (exact->inexact (/ true-weight denom)))
  (hash-set! out 'unknown (exact->inexact (/ other-weight denom)))
  (hash-set! out 'invalid (exact->inexact (/ false-weight denom)))
  (define total (apply + (hash-values out)))
  (hash-update! out 'precondition (curry + (- 1 total)) 0)
  (make-immutable-hash (hash->list out)))

(define (find-intervals compiler rects #:fuel [depth 128])
  (define var-reprs (real-compiler-var-reprs compiler))
  (define initial-space (apply make-search-space rects))
  (define roots (search-space-other initial-space))
  (let loop ([space initial-space]
             [n 0])
    (match-define (search-space true false other) space)
    (define table (make-sampling-table var-reprs true false other))
    (timeline-push! 'sampling n table)

    (define n* (remainder n (vector-length var-reprs)))
    (if (or (>= n depth) (empty? (search-space-other space)) (>= (length other) (expt 2 depth)))
        (let* ([results (map collect-kept-regions roots)]
               [regions (append-map second results)])
          (list (map first regions) (map second regions) table))
        (loop (search-step compiler space n*) (+ n 1)))))

(define (collect-kept-regions node)
  (if (search-node-dropped node)
      (list #f '())
      (match (search-node-children node)
        [(list left right)
         (define left-result (collect-kept-regions left))
         (define right-result (collect-kept-regions right))
         (if (and (first left-result) (first right-result))
             (list #t (list (list (search-node-rect node) (search-node-hint node))))
             (list #f (append (second left-result) (second right-result))))]
        [_ (list #t (list (list (search-node-rect node) (search-node-hint node))))])))

(module+ test
  (require rackunit)

  (define false-region (search-node 'false-region 'false-hint #f #t))
  (define kept-subtree
    (search-node 'kept-subtree
                 'subtree-hint
                 (list (search-node 'left 'leaf-hint #f #f) (search-node 'right 'leaf-hint #f #f))
                 #f))
  (define root (search-node 'parent 'parent-hint (list false-region kept-subtree) #f))
  (check-equal? (collect-kept-regions root) (list #f (list (list 'kept-subtree 'subtree-hint))))
  (set-search-node-dropped! false-region #f)
  (check-equal? (collect-kept-regions root) (list #t (list (list 'parent 'parent-hint)))))
