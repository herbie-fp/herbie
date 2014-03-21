#lang racket

(require math/bigfloat)
(require data/order)

(provide reap println ->flonum *precision* cotan ordinary-float?
         list= list< enumerate take-up-to *debug* debug debug-reset pipe 1+)

; Precision for approximate evaluation
(define *precision* (make-parameter real->double-flonum))

(define (println . args)
  (for ([val args])
    (if (string? val)
        (display val)
        (write val)))
  (newline)
  (let ([possible-returns (filter (negate string?) args)])
    (when (not (null? possible-returns))
      (last possible-returns))))

(define *debug* (make-parameter #f))
(define *log* '())

(define *tags*
  #hasheq([enter . "> "]
          [exit . "< "]
          [info . ";; "]))

(define (debug #:from from #:tag (tag #f) . args)
  (set! *log*
        (cons (list* from tag args) *log*))
  (when (*debug*)
      (display (hash-ref *tags* tag "; "))
      (write from)
      (display ": ")
      (for/list ([arg args])
        (display " ")
        ((if (string? arg) display write) arg))
      (newline)))

(define (debug-reset)
  (set! *log* '()))

(define-syntax (reap stx)
  "A reap/sow abstraction for filters and maps."
  (syntax-case stx ()
    [(_ [sow] body ...)
     #'(let* ([store '()]
              [sow (λ (elt) (set! store (cons elt store)) elt)])
         body ...
         (reverse store))]))

(define (->flonum x)
  (cond
   [(real? x) ((*precision*) x)]
   [(bigfloat? x) ((*precision*) (bigfloat->flonum x))]
   [(complex? x)
    (if (= (imag-part x) 0)
        (->flonum (real-part x))
        +nan.0)]
   [else (error "Invalid number" x)]))

; Functions used by our benchmarks
(define (cotan x)
  (/ 1 (tan x)))

(define (ordinary-float? x)
  (not (or (infinite? x) (nan? x))))

(define (=-or-nan? x1 x2)
  (or (= x1 x2)
      (and (nan? x1) (nan? x2))))

(define (1+ x)
  (+ 1 x))

(define (list= l1 l2)
  (and l1 l2 (andmap =-or-nan? l1 l2)))

(define (list< list1 list2)
  "Compares lists lexicographically."
  ; Who picked this terrible API design of returning '< or '>
  (eq? (datum-order list1 list2) '<))

(define (enumerate . l)
  (apply map list (range (length (car l))) l))

(define (take-up-to l k)
  ; This is unnecessarily slow. It is O(l), not O(k).
  ; To be honest, it just isn't that big a deal for now.
  (take l (min k (length l))))

;; Pipes an initial values through a list of funcs.
(define (pipe initial funcs)
  ((apply compose (reverse funcs)) initial))
