#lang racket
(require "config.rkt" "float.rkt" racket/hash json)
(provide timeline-event! timeline-log! timeline-push! timeline-adjust!
         timeline-load! timeline-extract
         timeline-merge timeline-relink *timeline-disabled*)
(module+ debug (provide *timeline*))

;; This is a box so we can get a reference outside the engine, and so
;; access its value even in a timeout.
;; Important: Use 'eq?' based hash tables, process may freeze otherwise
(define *timeline* (box '()))
(define *timeline-disabled* (make-parameter true))

(register-reset (λ () (set-box! *timeline* '())))

(define (timeline-event! type)
  (unless (*timeline-disabled*)
    (define initial (hasheq 'type (~a type) 'time (current-inexact-milliseconds)))
    (define b (make-hasheq (hash->list initial))) ; convert to mutable hash
    (set-box! *timeline* (cons b (unbox *timeline*)))))

(define/contract (timeline-log! key value)
  (-> symbol? jsexpr? void?)

  (unless (*timeline-disabled*)
    (define h (car (unbox *timeline*)))
    (when (hash-has-key? h key)
      (error 'timeline "Attempting to log key ~a to timeline twice (value ~a)" key value))
    (hash-set! h key value)))

(define/contract (timeline-push! key . values)
  (-> symbol? jsexpr? ... void?)
  (unless (*timeline-disabled*)
    (define val (if (= (length values) 1) (car values) values))
    (define (try-cons x)
      (if (list? x)
          (cons val x)
          (error 'timeline "Attempting to push onto a timeline non-list ~a (value ~a)" key x)))
    (hash-update! (car (unbox *timeline*)) key  try-cons '())))

(define (timeline-adjust! type key . values)
  (-> symbol? symbol? jsexpr? ... void?)
  (unless (*timeline-disabled*)
    (for/first ([cell (unbox *timeline*)] #:when (equal? (hash-ref cell 'type) type))
      (hash-set! cell key values)
      true)
    (void)))

(define (timeline-load! value)
  (set! *timeline* value))

(define (timeline-extract repr)
  (define end (hasheq 'time (current-inexact-milliseconds)))
  (reverse
   (for/list ([evt (unbox *timeline*)] [next (cons end (unbox *timeline*))])
     (define evt* (hash-copy evt))
     (hash-update! evt* 'time (λ (v) (- (hash-ref next 'time) v)))
     evt*)))

(define (timeline-relink link timeline)
  (for/list ([event (in-list timeline)])
    (for/hasheq ([(k v) (in-hash event)])
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
          ['compiler
           (list
            (list (apply + (map first (append (dict-ref data k '()) v)))
                  (apply + (map second (append (dict-ref data k '()) v)))))]
          ['outcomes
           (hash-union v (dict-ref data k #hash()) #:combine (curry map +))]
          [(or 'accuracy 'oracle 'baseline 'name 'link)
           (append v (dict-ref data k '()))]
          ['sampling
           (if (dict-has-key? data k)
               (let loop ([l1 (sort v < #:key first)] [l2 (sort (dict-ref data k) < #:key first)])
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
