#lang racket
(require profile/sampler
         profile/analyzer
         setup/dirs)
(provide profile-merge
         profile->json
         json->profile
         profile-thunk)

;; One annoyance with profiling in Racket is that if function f calls
;; (map g ...), we'll get profile edges (f, map) and (map, g). So if
;; you want to know what "f" spends its time doing, you won't know for
;; sure (imagine it calls map more than once), and if you want to know
;; what "g" is called by, good luck.
;;
;; We thus apply some clever filtering to stacks: if the stack is (f
;; map g), we'll create the edge (f, g), but if it's (f map) we'll
;; still do the edge (f, map) to record the overhead of map.

;; The specific test is whether or not it's part of the Racket
;; standard library (called the "collects")
(define collects-dir (path->string (find-collects-dir)))

(define (profile-focus? id src)
  (define dominated?
    (cond
      [(not src) #f]
      [else
       (define path (srcloc-source src))
       (cond
         [(path? path) (string-prefix? (path->string path) collects-dir)]
         [(string? path) (string-prefix? path "...")] ; abbreviated collects paths
         [else #f])]))
  (not dominated?))

;; Filter a stack for edge computation. Keep non-focused functions at the
;; front (self position), then filter them out once we hit a focused function.
(define (filter-stack focus? stack)
  (let loop ([stack stack]
             [filtering? #f])
    (cond
      [(null? stack) '()]
      [else
       (define entry (car stack))
       (define focused? (focus? (car entry) (cdr entry)))
       (cond
         [focused? (cons entry (loop (cdr stack) #t))]
         [filtering? (loop (cdr stack) #t)]
         [else (cons entry (loop (cdr stack) #f))])])))

;; Filter all stacks in samples using the focus predicate
(define (filter-samples focus? cpu-time+samples)
  (define cpu-time (car cpu-time+samples))
  (define samples (cdr cpu-time+samples))
  (cons cpu-time
        (for/list ([sample (in-list samples)])
          (define thread-id (car sample))
          (define thread-time (cadr sample))
          (define stack (cddr sample))
          (list* thread-id thread-time (filter-stack focus? stack)))))

(define (profile-thunk thunk renderer #:delay [delay-secs 0.05])
  (define sampler (create-sampler (current-thread) delay-secs))
  (define result
    (with-handlers ([void (λ (e)
                            (eprintf "profiled thunk error: ~a\n"
                                     (if (exn? e)
                                         (exn-message e)
                                         (format "~e" e))))])
      (thunk)))
  (sampler 'stop)
  (define raw-samples (sampler 'get-snapshots))
  (define filtered-samples (filter-samples profile-focus? raw-samples))
  (renderer (analyze-samples filtered-samples))
  result)

(define (profile-merge . ps)
  (define nodes (make-hash))
  (define root-node (node #f #f '() 0 0 '() '()))
  (hash-set! nodes (cons #f #f) root-node)
  (for* ([p (in-list ps)]
         [n (profile-nodes p)])
    (hash-set! nodes (node-loc n) (node (node-id n) (node-src n) '() 0 0 '() '())))
  (for* ([p ps]
         [node (profile-nodes p)])
    (profile-add nodes node))
  (for ([p ps])
    (profile-add nodes (profile-*-node p)))
  (hash-remove! nodes (cons #f #f))
  (profile (apply + (map profile-total-time ps))
           (apply + (map profile-cpu-time ps))
           (apply + (map profile-sample-number ps))
           (apply merge-thread-times (map profile-thread-times ps))
           (hash-values nodes)
           root-node))

(define (translate table node)
  (hash-ref table (node-loc node)))

(define (profile-add table node)
  (define node* (translate table node))
  (set-node-thread-ids! node* (set-union (node-thread-ids node*) (node-thread-ids node)))
  (set-node-total! node* (+ (node-total node*) (node-total node)))
  (set-node-self! node* (+ (node-self node*) (node-self node)))
  (for ([e (node-callers node)])
    (define caller* (translate table (edge-caller e)))
    (match (findf (λ (e2) (eq? (edge-caller e2) caller*)) (node-callers node*))
      [#f
       (define e* (struct-copy edge e [caller caller*] [callee node*]))
       (set-node-callers! node* (cons e* (node-callers node*)))]
      [e*
       (set-edge-total! e* (+ (edge-total e) (edge-total e*)))
       (set-edge-caller-time! e* (+ (edge-caller-time e) (edge-caller-time e*)))
       (set-edge-callee-time! e* (+ (edge-callee-time e) (edge-callee-time e*)))]))
  (for ([e (node-callees node)])
    (define callee* (translate table (edge-callee e)))
    (match (findf (λ (e2) (eq? (edge-callee e2) callee*)) (node-callees node*))
      [#f
       (define e* (struct-copy edge e [callee callee*] [caller node*]))
       (set-node-callees! node* (cons e* (node-callees node*)))]
      [e*
       (set-edge-total! e* (+ (edge-total e) (edge-total e*)))
       (set-edge-caller-time! e* (+ (edge-caller-time e) (edge-caller-time e*)))
       (set-edge-callee-time! e* (+ (edge-callee-time e) (edge-callee-time e*)))])))

(define (node-loc node)
  (cons (node-id node) (node-src node)))

(define (merge-thread-times . ts)
  (define h (make-hash))
  (for* ([t (in-list ts)]
         [(id time) (in-dict t)])
    (hash-update! h id (curry + time) 0))
  h)

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

(define (json->profile j)
  (define nodes
    (for/vector ([n (hash-ref j 'nodes)])
      (node (hash-ref n 'id)
            (and (hash-ref n 'src) (string->srcloc (hash-ref n 'src)))
            (hash-ref n 'thread_ids)
            (hash-ref n 'total)
            (hash-ref n 'self)
            '()
            '())))
  (for ([n (in-list (hash-ref j 'nodes))]
        [n* (in-vector nodes)])
    (set-node-callees! n*
                       (for/list ([e (hash-ref n 'callees)])
                         (edge (hash-ref e 'total)
                               (vector-ref nodes (hash-ref e 'caller))
                               (hash-ref e 'caller_time)
                               (vector-ref nodes (hash-ref e 'callee))
                               (hash-ref e 'callee_time))))
    (set-node-callers! n*
                       (for/list ([e (hash-ref n 'callers)])
                         (edge (hash-ref e 'total)
                               (vector-ref nodes (hash-ref e 'caller))
                               (hash-ref e 'caller_time)
                               (vector-ref nodes (hash-ref e 'callee))
                               (hash-ref e 'callee_time)))))
  (profile (hash-ref j 'total_time)
           (hash-ref j 'cpu_time)
           (hash-ref j 'sample_number)
           (for/list ([t (hash-ref j 'thread_times)])
             (cons (hash-ref t 'id) (hash-ref t 'time)))
           (rest (vector->list nodes))
           (vector-ref nodes 0)))

(module+ main
  (require profile/render-text
           json)
  (command-line #:args (file) (render (json->profile (call-with-input-file file read-json)) 'total)))
