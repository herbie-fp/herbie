#lang racket

(provide make-dvector
         dvector-add!
         dvector-ref
         in-dvector
         dvector-lengthgit s
         dvector-copy
         (rename-out [create-dvector dvector]))

(define starting-length 128)

(struct dvector ([vec #:mutable] [tail-pt #:mutable] [len #:mutable])
  #:methods gen:custom-write
  [(define (write-proc dvec port mode)
     (define elems
       (for/list ([elem (in-dvector dvec)])
         (~a elem)))
     (fprintf port "'#d(~a)" (string-join elems " ")))]
  #:property prop:equal+hash
  (list (λ (a b eq?) ; equal? override
          (and (dvector? b) (eq? (dvector-vec a) (dvector-vec b))))
        (λ (a hc) ; hash-code
          (+ (hc (dvector-vec a)) (dvector-tail-pt a) (dvector-len a)))
        (λ (a hc) ; secondary-hash-code
          (+ (hc (dvector-vec a)) (* 7 (dvector-tail-pt a)) (+ 1 (dvector-len a))))))

(define (make-dvector [size 0] [v -1])
  (define size*
    (cond
      [(< size starting-length) starting-length]
      [else
       (let loop ([size* (* starting-length 2)])
         (if (< size size*)
             size*
             (loop (* size* 2))))]))
  (dvector (make-vector size* v) size size*))

(define (create-dvector . args)
  (define dvec (make-dvector))
  (for ([arg args])
    (dvector-add! dvec arg))
  dvec)

(define (dvector-extend! dvec)
  (match-define (dvector vec tail-pt len) dvec)
  (define len* (* 2 len))
  (define vec* (make-vector len*))
  (vector-copy! vec* 0 vec)
  (set-dvector-vec! dvec vec*)
  (set-dvector-len! dvec len*))

(define (dvector-add! dvec elem)
  (match-define (dvector vec tail-pt len) dvec)
  (cond
    [(equal? tail-pt len)
     (dvector-extend! dvec)
     (dvector-add! dvec elem)]
    [else
     (vector-set! vec tail-pt elem)
     (set-dvector-tail-pt! dvec (add1 tail-pt))]))

(define (dvector-ref dvec idx)
  (vector-ref (dvector-vec dvec) idx))

(define (dvector-copy dvec)
  (match-define (dvector vec tail-pt len) dvec)
  (dvector (vector-copy vec) tail-pt len))

(define (in-dvector dvec [start 0] [end (dvector-tail-pt dvec)] [step 1])
  (when (or (< (dvector-tail-pt dvec) end) (> start (dvector-tail-pt dvec)))
    (error "in-divector is out of index"))
  (in-vector (dvector-vec dvec) start end step))

(define (dvector-length dvec)
  (dvector-tail-pt dvec))

(module+ test
  (require rackunit)

  ;; Test: Create a new dvector
  (define dv (make-dvector))

  (define dv1 (make-dvector)) ; default
  (check-equal? (dvector-tail-pt dv1) 0)
  (check-equal? (dvector-len dv1) starting-length)
  (check-equal? (vector-length (dvector-vec dv1)) starting-length)

  (define dv2 (make-dvector 10 5))
  (check-equal? (dvector-tail-pt dv2) 10)
  (check-equal? (dvector-len dv2) starting-length)
  (check-equal? (vector-length (dvector-vec dv2)) starting-length)
  (check-equal? (vector-ref (dvector-vec dv2) 0) 5)

  ;; Large input triggers dynamic size growth
  (define dv3 (make-dvector 300))
  (check-true (> (dvector-len dv3) 300))
  (check-equal? (dvector-tail-pt dv3) 300)

  ;; Custom equal? behavior: only same vector => equal
  (define dv4 dv2) ; same instance
  (define dv5 (make-dvector 10 5)) ; same content, different vector
  (check-true (equal? dv2 dv4))

  (check-equal? (dvector-length dv) 0)
  (check-equal? (vector-length (dvector-vec dv)) 128)

  ;; Test: Adding one element
  (dvector-add! dv 'a)
  (check-equal? (dvector-length dv) 1)
  (check-equal? (dvector-ref dv 0) 'a)

  ;; Test: Adding multiple elements within capacity
  (for ([i (in-range 1 128)])
    (dvector-add! dv i))

  (check-equal? (dvector-length dv) 128)
  (check-equal? (dvector-ref dv 1) 1)
  (check-equal? (dvector-ref dv 127) 127)

  ;; Test: Adding element to trigger extension
  (dvector-add! dv 'extended)
  (check-equal? (dvector-length dv) 129)
  (check-equal? (vector-length (dvector-vec dv)) 256) ; Should have doubled

  (dvector-add! dv 'new)
  (check-equal? (dvector-length dv) 130)
  (check-equal? (dvector-ref dv 129) 'new)

  ;; Test: in-dvector (implicit end)
  (define values
    (for/list ([x (in-dvector dv)])
      x))
  (check-equal? (length values) 130)
  (check-equal? (first values) 'a)
  (check-equal? (last values) 'new)

  ;; Test: in-dvector with range and step
  (define stepped
    (for/list ([x (in-dvector dv 0 10 2)])
      x))
  (check-equal? stepped (list 'a 2 4 6 8))

  ;; Test: Reference out of bounds
  (check-exn exn:fail? (lambda () (dvector-ref dv 999)))

  ;; Test: Length after many additions
  (for ([i (in-range 200)])
    (dvector-add! dv i))
  (check-equal? (dvector-length dv) 330)
  (check-equal? (vector-length (dvector-vec dv)) 512)) ; Should have extended again
