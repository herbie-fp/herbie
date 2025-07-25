#lang racket

(require racket/match)

; Nodes are implemented as a hash in this example
(struct batch ([nodes #:mutable] [index #:mutable] cache [mappings #:mutable] [timestamp #:mutable]))

(define (make-batch)
  (batch (make-hasheq) (make-hash) (make-hasheq) (make-hasheq (list (cons 0 (vector)))) 0))

(struct batchref (batch timestamp idx) #:transparent)

(define (find-up-to-date-index br)
  (match-define (batchref b timestamp idx) br)
  (define mappings (batch-mappings b))
  (define curr-timestamp (batch-timestamp b))
  (define idx* idx)
  (for ([n (in-range timestamp (add1 curr-timestamp))])
    (define curr-mappings (hash-ref mappings n))
    (set! idx* (vector-ref curr-mappings idx*)))
  idx*)

(define (debatchref br)
  (define idx (find-up-to-date-index br))
  (batch-extract (batchref-batch br) idx))

(define (batch-extract batch reg)
  (define (unmunge reg)
    (define node (hash-ref (batch-nodes batch) reg))
    (expr-recurse node unmunge))
  (unmunge reg))

(define (expr-recurse expr f)
  (match expr
    ;[(approx spec impl) (approx (f spec) (f impl))]
    ;[(hole precision spec) (hole precision (f spec))]
    [(list op args ...) (cons op (map f args))]
    [_ expr]))

(define (batch-push! b term)
  (define hashcons (batch-index b))
  (hash-ref! hashcons
             term
             (lambda ()
               (define idx (hash-count hashcons))
               (hash-set! hashcons term idx)
               (hash-set! (batch-nodes b) idx term)
               idx)))

; Mappings impemented as vector here - that's bad but works
(define (add-nodes-to-remapping! b)
  (define ts (batch-timestamp b))
  (define remappings (batch-mappings b))
  (define curr-map-len (vector-length (hash-ref (batch-mappings b) ts)))
  (unless (equal? (batch-length b) curr-map-len)
    (hash-set! remappings ts (build-vector (batch-length b) identity))))

(define (batch-add! b expr)
  (define cache (batch-cache b))
  (define (munge prog)
    (hash-ref! cache prog (lambda () (batch-push! b (expr-recurse prog munge)))))
  (batchref b (batch-timestamp b) (munge expr)))

(define (batch-length b)
  (hash-count (batch-nodes b)))

(define (batch-current-remapping b)
  (hash-ref (batch-mappings b) (batch-timestamp b)))

(define (batch-get-remapping b t)
  (hash-ref (batch-mappings b) t))

(define (batch-increase-timestamp! b)
  (define timestamp* (add1 (batch-timestamp b)))
  (set-batch-timestamp! b timestamp*)
  (define mappings (batch-mappings b))
  (hash-set! mappings timestamp* (vector)))

(define (deref x)
  (match-define (batchref b _ idx)
    x) ; it is not exactly a batchref anymore - timestamp here does not really matter
  (expr-recurse (hash-ref (batch-nodes b) idx) (lambda (ref) (batchref b -1 ref))))

(define (batch-replace! b f)
  (define remapping (batch-current-remapping b))
  ; Create mapping for every node
  (define len (batch-length b))
  (define mapping (make-vector (batch-length b) -1))
  (for ([idx (range 0 len)])
    (define node (hash-ref (batch-nodes b) idx))
    (define replacement (f (expr-recurse node (lambda (x) (batchref b -1 x)))))
    (define final-idx
      (let loop ([expr replacement])
        (match expr
          [(batchref b* _ idx)
           (unless (eq? b* b)
             (error 'batch-replace "Replacement ~a references the wrong batch ~a" replacement b*))
           (when (= -1 (vector-ref mapping idx))
             (error 'batch-replace "Replacement ~a references unknown index ~a" replacement idx))
           (vector-ref mapping idx)]
          [_ (batch-push! b (expr-recurse expr loop))]))) ; pushing to the same batch!
    (vector-set! mapping idx final-idx))
  (for ([idx (in-vector remapping)]
        [n (in-naturals)])
    (vector-set! remapping n (vector-ref mapping idx))))

;; Our Taylor expander prefers sin, cos, exp, log, neg over trig, htrig, pow, and subtraction
(define (expand-taylor! input-batch)
  (batch-replace!
   input-batch
   (lambda (node)
     (match node
       [(list '- ref1 ref2) `(+ ,ref1 (neg ,ref2))]
       [(list 'pow base (app deref 1/2)) `(sqrt ,base)]
       [(list 'pow base (app deref 1/3)) `(cbrt ,base)]
       [(list 'pow base (app deref 2/3)) `(cbrt (* ,base ,base))]
       [(list 'pow base power)
        #:when (exact-integer? (deref power))
        `(pow ,base ,power)]
       [(list 'pow base power) `(exp (* ,power (log ,base)))]
       [(list 'tan arg) `(/ (sin ,arg) (cos ,arg))]
       [(list 'cosh arg) `(* 1/2 (+ (exp ,arg) (/ 1 (exp ,arg))))]
       [(list 'sinh arg) `(* 1/2 (+ (exp ,arg) (/ -1 (exp ,arg))))]
       [(list 'tanh arg) `(/ (+ (exp ,arg) (neg (/ 1 (exp ,arg)))) (+ (exp ,arg) (/ 1 (exp ,arg))))]
       [(list 'asinh arg) `(log (+ ,arg (sqrt (+ (* ,arg ,arg) 1))))]
       [(list 'acosh arg) `(log (+ ,arg (sqrt (+ (* ,arg ,arg) -1))))]
       [(list 'atanh arg) `(* 1/2 (log (/ (+ 1 ,arg) (+ 1 (neg ,arg)))))]
       [_ node]))))

(define (replace-foo! input-batch)
  (batch-replace! input-batch
                  (lambda (node)
                    (match node
                      [(list 'foo x) `(foo1111 ,x)]
                      [_ node]))))

(define (replace-every-op-to-bar! input-batch)
  (batch-replace! input-batch
                  (lambda (node)
                    (match node
                      [(list op args ...) (cons 'bar args)]
                      [_ node]))))

(define (batch-printf b)
  (printf "Batch-info:\n\tTimestamp: ~a\n\tNodes: ~a\n\t" (batch-timestamp b) (batch-nodes b))
  (define ts (batch-timestamp b))
  (for ([t (in-range 0 ts)])
    (printf "Remappings at timestamp ~a: ~a\n\t" t (batch-get-remapping b t)))
  (printf "Remappings at timestamp ~a: ~a\n\n" ts (batch-get-remapping b ts)))

(define (print-break)
  (printf "-------------------------------------\n\n"))

(module+ main
  (define b (make-batch))

  ;; ----------------------------- Timestamp 0 --------------------------
  (define exprs '((cosh x) (atanh x) (+ 100 (pow x 1/3)) (foo (bar 2 3))))
  (define batchrefs0 (map (curry batch-add! b) exprs))
  ; Manually adding remapping for now - ideally it will be automatically
  (add-nodes-to-remapping! b)
  (printf "Initial batch looks like:\n")
  (batch-printf b)
  (print-break)

  (printf "Starting doing updates at timestamp 0!\n")
  ; Update #1
  (expand-taylor! b) ; Important: this update is in-place - no new batch is allocated
  (batch-printf b)
  (print-break)

  ; Update #2
  (printf "Some more updates at timestamp 0!\n")
  (replace-foo! b)
  (batch-printf b)
  (print-break)

  (printf "After update expressions look like:\n")
  (pretty-print (map debatchref batchrefs0))
  (print-break)

  ;; ----------------------------- Timestamp 1 --------------------------

  (printf "Adding more expression, timestamp 1!\n")
  (batch-increase-timestamp! b) ; Manually increasing timestamp for now
  (define batchrefs1 (map (curry batch-add! b) exprs))
  (add-nodes-to-remapping! b)
  (batch-printf b)
  (print-break)

  (printf "Doing some updates (foo -> foo11111)!\n")
  ; Update #3
  (replace-foo! b)
  (batch-printf b)
  (print-break)

  (printf "expressions inserted at timestamp 1 look like:\n")
  (pretty-print (map debatchref batchrefs1))
  (print-break)

  (printf "Replacing every op to 'bar !\n")
  ; Update #4
  (replace-every-op-to-bar! b)
  (batch-printf b)
  (print-break)

  (printf "Now, expressions from timestamp 0 look like:\n")
  (pretty-print (map debatchref batchrefs0))
  (print-break)

  (printf "Expressions from timestamp 1 look like:\n")
  (pretty-print (map debatchref batchrefs1))
  (print-break))
