#lang racket
(require "config.rkt" "float.rkt" racket/hash)
(provide timeline-event! timeline-log! timeline-push! timeline-adjust!
         timeline-load! timeline-extract timeline->json
         timeline-merge timeline-relink *timeline-disabled*)
(module+ debug (provide *timeline*))

;; This is a box so we can get a reference outside the engine, and so
;; access its value even in a timeout.
(define *timeline* (box '()))
(define *timeline-disabled* (make-parameter true))

(register-reset (λ () (set-box! *timeline* '())))

(define (timeline-event! type)
  (unless (*timeline-disabled*)
    (define initial (hash 'type type 'time (current-inexact-milliseconds)))
    (define b (make-hash (hash->list initial))) ; convert to mutable hash
    (set-box! *timeline* (cons b (unbox *timeline*)))))

(define (timeline-log! key value)
  (unless (*timeline-disabled*)
    (define h (car (unbox *timeline*)))
    (when (hash-has-key? h key)
      (error 'timeline "Attempting to log key ~a to timeline twice (value ~a)" key value))
    (hash-set! h key value)))

(define (timeline-push! key . values)
  (unless (*timeline-disabled*)
    (define val (if (= (length values) 1) (car values) values))
    (define (try-cons x)
      (if (list? x)
          (cons val x)
          (error 'timeline "Attempting to push onto a timeline non-list ~a (value ~a)" key x)))
    (hash-update! (car (unbox *timeline*)) key  try-cons '())))

(define (timeline-adjust! type key value)
  (unless (*timeline-disabled*)
    (for/first ([cell (unbox *timeline*)] #:when (equal? (hash-ref cell 'type) type))
      (hash-set! cell key value)
      true)))

(define (timeline-load! value)
  (set! *timeline* value))

(define (timeline-extract)
  (reverse (unbox *timeline*)))

(define (timeline->json timeline repr)
  (for/list ([event timeline] [next (cdr timeline)])
    (for/hash ([(k v) (in-dict event)])
      (define v*
        (match k
          ['type (~a v)]
          ['time (- (dict-ref next 'time) v)]
          ['method (list (~a v))]
          ['locations
           (for/list ([(expr error) (in-dict v)])
             (hash 'expr (~a expr) 'error error))]
          ['rules
           (for/hash ([(rule count) (in-dict v)])
             (values rule count))]
          ['times
           (for/list ([(expr times) (in-dict v)])
             (cons (~a expr) times))]
          ['outcomes
           (for/list ([(outcome number) (in-dict v)])
             (match-define (cons count time) number)
             (match-define (list prog category prec) outcome)
             (hash 'count count 'time time
                   'program (~a prog) 'category (~a category) 'precision prec))]
          ['bstep
           (define n->js (curryr value->json repr))
           (map (λ (x) (map (curryr apply '()) (list n->js n->js identity n->js) x)) v)]
          [(or 'accuracy 'oracle 'baseline 'name)
           (list v)]
          ['link (list (path->string v))]
          ['sampling (reverse v)]
          [(or 'filtered 'inputs 'outputs 'kept 'min-error 'egraph)
           v]))

      (values k v*))))

(define (timeline-relink link timeline)
  (for/list ([event (in-list timeline)])
    (for/hash ([(k v) (in-hash event)])
      (if (equal? k 'link)
          (values k (map (λ (p) (path->string (build-path link p))) v))
          (values k v)))))

(define (average . values)
  (/ (apply + values) (length values)))

(define (timeline-merge . timelines)
  ;; The timelines in this case are JSON objects, as above
  (define types (make-hash))
  (for* ([tl (in-list timelines)] [event tl])
    (define data (dict-ref! types (dict-ref event 'type) (make-hash)))
    (for ([(k v) (in-dict event)])
      (define v*
        (match k
          ['type v]
          ['time (+ v (dict-ref data k 0))]
          ['method (append v (dict-ref data k '()))]
          ['rules (hash-union v (dict-ref data k #hash()) #:combine +)]
          ['times (sort (append v (dict-ref data k '())) > #:key cadr)]
          ['outcomes
           (define (key x) (map (curry hash-ref x) '(program category precision)))
           (for/list ([rows (group-by key (append v (dict-ref data k '())))])
             (hash 'program (dict-ref (car rows) 'program)
                   'category (dict-ref (car rows) 'category)
                   'precision (dict-ref (car rows) 'precision)
                   'count (apply + (map (curryr dict-ref 'count) rows))
                   'time (apply + (map (curryr dict-ref 'time) rows))))]
          [(or 'accuracy 'oracle 'baseline 'name 'link)
           (append v (dict-ref data k '()))]
          ['filtered
           (match-define (list from1 to1) v)
           (match-define (list from2 to2) (dict-ref data k '(0 0)))
           (list (+ from1 from2) (+ to1 to2))]
          ['sampling
           (if (dict-has-key? data k)
               (let loop ([l1 v] [l2 (dict-ref data k)])
                 (match-define (list n1 wt1 wo1 wf1) (car l1))
                 (match-define (list n2 wt2 wo2 wf2) (car l2))
                 (define rec (list n1 (+ wt1 wt2) (+ wo1 wo2) (+ wf1 wf2)))
                 (match* ((cdr l1) (cdr l2))
                   [('() '()) (list rec)]
                   [('() l2*) (cons rec (loop (list (list (+ n1 1) wt1 wo1 wf1)) l2*))]
                   [(l1* '()) (cons rec (loop l1* (list (list (+ n2 1) wt2 wo2 wf2))))]
                   [(l1* l2*) (cons rec (loop l1* l2*))]))
               v)]
          [(or 'locations 'bstep
               'inputs 'outputs
               'kept 'min-error
               'egraph)
           (void)]))
      (unless (void? v*)
        (dict-set! data k v*))))
  
  (sort (dict-values types) > #:key (curryr dict-ref 'time)))
