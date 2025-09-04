#lang racket

(provide make-dvector
         dvector-add!
         dvector-set!
         dvector-ref
         in-dvector
         dvector-length
         dvector-capacity
         dvector-copy
         create-dvector)

(define starting-length 128)
(define default-filling-value #f)

(struct dvector ([vec #:mutable] [length #:mutable])
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
          (+ (hc (dvector-vec a)) (dvector-length a)))
        (λ (a hc) ; secondary-hash-code
          (+ (hc (dvector-vec a)) (* 7 (+ 1 (dvector-length a)))))))

(define (make-dvector [size 0] [v default-filling-value])
  (define size*
    (cond
      [(< size starting-length) starting-length]
      [else
       (let loop ([size* (* starting-length 2)])
         (if (< size size*)
             size*
             (loop (* size* 2))))]))
  (dvector (make-vector size* v) size))

(define (create-dvector . args)
  (define dvec (make-dvector))
  (for ([arg args])
    (dvector-add! dvec arg))
  dvec)

(define (dvector-capacity dvec)
  (vector-length (dvector-vec dvec)))

(define (dvector-extend! dvec)
  (match-define (dvector vec _) dvec)
  (define cap (dvector-capacity dvec))
  (define vec* (make-vector (* 2 cap) default-filling-value))
  (vector-copy! vec* 0 vec)
  (set-dvector-vec! dvec vec*))

(define (dvector-add! dvec elem)
  (match-define (dvector vec len) dvec)
  (cond
    [(equal? len (dvector-capacity dvec))
     (dvector-extend! dvec)
     (dvector-add! dvec elem)]
    [else
     (vector-set! vec len elem)
     (set-dvector-length! dvec (add1 len))]))

(define (dvector-set! dvec idx elem)
  (match-define (dvector vec len) dvec)
  (cond
    [(>= idx (dvector-capacity dvec))
     (dvector-extend! dvec)
     (dvector-set! dvec idx elem)]
    [else
     (vector-set! vec idx elem)
     (set-dvector-length! dvec (max len (add1 idx)))]))

(define (dvector-ref dvec idx)
  (vector-ref (dvector-vec dvec) idx))

(define (dvector-copy dvec)
  (match-define (dvector vec len) dvec)
  (dvector (vector-copy vec) len))

(define (in-dvector dvec [start 0] [end (dvector-length dvec)] [step 1])
  (when (or (< (dvector-length dvec) (or end (dvector-length dvec))) (> start (dvector-length dvec)))
    (error "in-dvector is out of index"))
  (in-vector (dvector-vec dvec) start (or end (dvector-length dvec)) step))

(module+ test
  (require rackunit)

  ;; Test: Create a new dvector
  (define dv (make-dvector))

  (define dv1 (make-dvector)) ; default
  (check-equal? (dvector-length dv1) 0)
  (check-equal? (dvector-capacity dv1) starting-length)
  (check-equal? (vector-length (dvector-vec dv1)) starting-length)

  (define dv2 (make-dvector 10 5))
  (check-equal? (dvector-length dv2) 10)
  (check-equal? (dvector-capacity dv2) starting-length)
  (check-equal? (vector-length (dvector-vec dv2)) starting-length)
  (check-equal? (vector-ref (dvector-vec dv2) 0) 5)

  ;; Large input triggers dynamic size growth
  (define dv3 (make-dvector 300))
  (check-true (> (dvector-capacity dv3) 300))
  (check-equal? (dvector-length dv3) 300)

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
