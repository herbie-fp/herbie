#lang racket

(provide make-dvector
         dvector-add!
         dvector-ref
         in-dvector
         dvector-length
         dvector-copy)

(define starting-length 128)

(struct dvector ([vec #:mutable] [tail-pt #:mutable] [len #:mutable]))

(define (make-dvector)
  (dvector (make-vector starting-length) 0 starting-length))

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
