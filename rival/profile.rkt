#lang racket
(require profile/analyzer)
(provide profile->json)

;;; Contents copied from herbie-fp/herbie, src/profile.rkt, aefdd770 Jun 2024.

(define (profile->json p)
  (define nodes (cons (profile-*-node p) (profile-nodes p)))
  (define loc-hash
    (for/hash ([node (in-list nodes)]
               [n (in-naturals)])
      (values (node-loc node) n)))
  (define node-hash
    (for/hash ([node (in-list nodes)])
      (values (node-loc node) node)))

  (hash 'total_time
        (exact->inexact (profile-total-time p))
        'cpu_time
        (exact->inexact (profile-cpu-time p))
        'sample_number
        (profile-sample-number p)
        'thread_times
        (for/list ([(id time) (in-dict (profile-thread-times p))])
          (hash 'id id 'time (exact->inexact time)))
        'nodes
        (for/list ([node nodes])
          (hash 'id
                (and (node-id node) (~a (node-id node)))
                'src
                (and (node-src node) (srcloc->string (node-src node)))
                'thread_ids
                (node-thread-ids node)
                'total
                (exact->inexact (node-total node))
                'self
                (exact->inexact (node-self node))
                'callers
                (map (curry edge->json loc-hash) (node-callers node))
                'callees
                (map (curry edge->json loc-hash) (node-callees node))))))

(define (node-loc node)
  (cons (node-id node) (node-src node)))

(define (edge->json loc-hash edge)
  (hash 'total
        (exact->inexact (edge-total edge))
        'caller
        (hash-ref loc-hash (node-loc (edge-caller edge)))
        'caller_time
        (exact->inexact (edge-caller-time edge))
        'callee
        (hash-ref loc-hash (node-loc (edge-callee edge)))
        'callee_time
        (exact->inexact (edge-callee-time edge))))

(define (string->srcloc s)
  (match-define (list path-parts ... (app string->number line) (app string->number col))
    (string-split s ":" #:trim? #f))
  (define path (string->path (string-join path-parts ":")))
  (srcloc path line (and line col) #f #f))
