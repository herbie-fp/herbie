#lang racket
(require "config.rkt" "float.rkt" racket/hash json)
(provide timeline-event! timeline-push! timeline-adjust!
         timeline-load! timeline-extract timeline-compact! timeline-start!
         timeline-merge timeline-relink *timeline-disabled*)
(module+ debug (provide *timeline*))

;; This is a box so we can get a reference outside the engine, and so
;; access its value even in a timeout.
;; Important: Use 'eq?' based hash tables, process may freeze otherwise
(define *timeline* (box '()))
(define *timeline-disabled* (make-parameter true))
(define *timeline-timers* (mutable-set))

(register-reset (λ () (set-box! *timeline* '())))

(define (timeline-event! type)
  (unless (*timeline-disabled*)
    (define b (make-hasheq (list (cons 'type (~a type))
                                 (cons 'time (current-inexact-milliseconds)))))
    (set-box! *timeline* (cons b (unbox *timeline*)))))

(define/contract (timeline-push! key . values)
  (-> symbol? jsexpr? ... void?)
  (unless (*timeline-disabled*)
    (define val (if (= (length values) 1) (car values) values))
    (hash-update! (car (unbox *timeline*)) key (curry cons val) '())))

(define/contract (timeline-adjust! type key . values)
  (-> symbol? symbol? jsexpr? ... void?)
  (unless (*timeline-disabled*)
    (for/first ([cell (unbox *timeline*)] #:when (equal? (hash-ref cell 'type) (~a type)))
      (hash-set! cell key values)
      true)
    (void)))

(define (timeline-start! key . values)
  (define tstart (current-inexact-milliseconds))
  (define (end!)
    (define tend (current-inexact-milliseconds))
    (apply timeline-push! key (append values (list (- tend tstart))))
    (set-remove! *timeline-timers* end!))
  (set-add! *timeline-timers* end!)
  end!)

(define (timeline-load! value)
  (set! *timeline* value))

(define (timeline-extract repr)
  (for ([end! (set->list *timeline-timers*)]) (end!))
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

(define timeline-types (make-hasheq))

(define-syntax define-timeline
  (syntax-rules ()
    [(_ name [field] ...)
     (hash-set! timeline-types 'name append)]
    [(_ name [field type] ...)
     (hash-set! timeline-types 'name
                (procedure-rename (make-merger type ...) 'name))]
    [(_ name #:custom fn)
     (hash-set! timeline-types 'name fn)]
    [(_ name #:unmergable)
     (hash-set! timeline-types 'name #f)]))

(define (make-merger . fields)
  (λ tables
    (define rows (apply append tables))
    (define groups (make-hash))
    (for ([row rows])
      (define-values (values key*) (partition cdr (map cons row fields)))
      (define key (map car key*))
      (if (hash-has-key? groups key)
          (hash-update! groups key
                        (λ (old)
                          (for/list ([value2 old] [(value1 fn) (in-dict values)])
                            (fn value2 value1))))
          (hash-set! groups key (map car values))))
    (for/list ([(k v) (in-hash groups)])
      (let loop ([fields fields] [k k] [v v])
        (match* (fields k v)
          [((cons #f f*) (cons k k*) v)
           (cons k (loop f* k* v))]
          [((cons _ f*) k (cons v v*))
           (cons v (loop f* k v*))]
          [('() '() '())
           '()])))))

(define (merge-sampling-tables l1 l2)
  (let loop ([l1 (sort l1 < #:key first)] [l2 (sort l2 < #:key first)])
    (match-define (list n1 wt1 wo1 wf1) (car l1))
    (match-define (list n2 wt2 wo2 wf2) (car l2))
    (define rec (list n1 (+ wt1 wt2) (+ wo1 wo2) (+ wf1 wf2)))
    (match* ((cdr l1) (cdr l2))
            [('() '()) (list rec)]
            [('() l2*) (cons rec (loop (list (list (+ n1 1) wt1 wo1 wf1)) l2*))]
            [(l1* '()) (cons rec (loop l1* (list (list (+ n2 1) wt2 wo2 wf2))))]
            [(l1* l2*) (cons rec (loop l1* l2*))])))

(define-timeline type #:custom (λ (a b) a))
(define-timeline time #:custom +)

(define-timeline method [method])
(define-timeline rules [rule false] [count +])
(define-timeline times [input false] [count +])
(define-timeline compiler [before +] [after +])
(define-timeline outcomes [name false] [prec false] [category false] [time +] [count +])
(define-timeline accuracy [accuracy])
(define-timeline oracle [oracle])
(define-timeline baseline [baseline])
(define-timeline count [input +] [output +])
(define-timeline alts #:unmergable)
(define-timeline sampling #:custom merge-sampling-tables)
(define-timeline symmetry #:unmergable)
(define-timeline remove-preprocessing #:unmergable)
(define-timeline locations #:unmergable)
(define-timeline bstep #:unmergable)
(define-timeline kept #:unmergable)
(define-timeline min-error #:unmergable)
(define-timeline egraph #:unmergable)
(define-timeline egraph-stop [reason false] [count +])

(define (timeline-merge . timelines)
  ;; The timelines in this case are JSON objects, as above
  (define types (make-hash))
  (for* ([tl (in-list timelines)] [event tl])
    (define data (hash-ref! types (hash-ref event 'type) (make-hash)))
    (for ([(k v) (in-dict event)] #:when (hash-ref timeline-types k #f))
      (if (hash-has-key? data k)
          (hash-update! data k (λ (old) ((hash-ref timeline-types k) v old)))
          (hash-set! data k v))))
  
  (sort (hash-values types) > #:key (curryr hash-ref 'time)))

(define (timeline-compact! key)
  (unless (*timeline-disabled*)
    (define fn (hash-ref timeline-types key #f))
    (when fn
      (hash-update! (car (unbox *timeline*)) key (curryr fn '()) '()))))
