#lang racket

(require math/bigfloat
         (only-in math/private/bigfloat/mpfr [bigfloat->flonum direct-bigfloat->flonum])
         math/base
         math/flonum
         "../utils/errors.rkt")

(provide (struct-out representation)
         (struct-out tuple-representation)
         repr->prop
         tuple-representation-base
         uniform-tuple-shape
         shift
         unshift
         <bool>
         <binary32>
         <binary64>
         (struct-out context)
         *context*
         context-extend
         context-lookup
         contexts-union
         make-representation
         make-tuple-representation)

;; Representations

(struct representation
        (name type bf->repr repr->bf ordinal->repr repr->ordinal total-bits special-value?)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation ~a>" (representation-name repr)))])

(struct tuple-representation representation (slots) #:transparent)

;; The scalar representation at a tuple's first leaf; the identity on scalars.
(define (tuple-representation-base repr)
  (if (tuple-representation? repr)
      (tuple-representation-base (first (tuple-representation-slots repr)))
      repr))

;; The dimensions of a tuple tree that is homogeneous at every level,
;; e.g. '(2 3) for a pair of triples, or '() for a scalar. Returns #f for
;; a tuple that mixes representations. Such tuples round-trip through the
;; FPCore dimension syntax for arguments, e.g. (x 2 3).
(define (uniform-tuple-shape repr)
  (match repr
    [(? tuple-representation?)
     (match-define (cons slot rest) (tuple-representation-slots repr))
     (define shape (uniform-tuple-shape slot))
     (and shape (andmap (curry equal? slot) rest) (cons (add1 (length rest)) shape))]
    [_ '()]))

;; Converts a representation into a rounding property
(define (repr->prop repr)
  (match repr
    [(? tuple-representation?) (repr->prop (first (tuple-representation-slots repr)))]
    [(? representation?)
     (match (representation-type repr)
       ['bool '()]
       ['real (list (cons ':precision (representation-name repr)))])]))

(define (make-representation #:name name
                             #:bf->repr bf->repr
                             #:repr->bf repr->bf
                             #:ordinal->repr ordinal->repr
                             #:repr->ordinal repr->ordinal
                             #:total-bits total-bits
                             #:special-value? special-value?)
  (representation name 'real bf->repr repr->bf ordinal->repr repr->ordinal total-bits special-value?))

(define (make-tuple-representation #:slots slots)
  ;; A tuple needs at least one slot: its first slot decides the rounding
  ;; context of the whole value (see `repr->prop`).
  (when (null? slots)
    (raise-herbie-error "Tuples require at least one slot"))
  (define tuple-ty `(tuple ,@(map representation-type slots)))
  (define name `(tuple ,@(map representation-name slots)))
  ;; TODO: tuples inherit unused scalar conversion slots.
  (define total-bits (apply + (map representation-total-bits slots)))
  (tuple-representation name tuple-ty void void void void total-bits void slots))

(module hairy racket/base
  (require (only-in math/private/bigfloat/mpfr get-mpfr-fun _mpfr-pointer _rnd_t bf-rounding-mode))
  (require ffi/unsafe)
  (provide bigfloat->float32)
  (define mpfr-get-flt (get-mpfr-fun 'mpfr_get_flt (_fun _mpfr-pointer _rnd_t -> _float)))
  (define (bigfloat->float32 x)
    (mpfr-get-flt x (bf-rounding-mode))))
(require (submod "." hairy))

(define (float32->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f #f))

(define (float32->ordinal x)
  (if (negative? x)
      (- (float32->bit-field (- x)))
      (float32->bit-field (abs x))))

(define (bit-field->float32 x)
  (floating-point-bytes->real (integer->integer-bytes x 4 #f #f) #f))

(define (ordinal->float32 x)
  (if (negative? x)
      (- (bit-field->float32 (- x)))
      (bit-field->float32 x)))

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

;; Does not use make-representation to define a repr of bool
(define <bool>
  (representation 'bool 'bool identity identity (curry = 0) (lambda (x) (if x 0 -1)) 1 (const #f)))

(define <binary32>
  (make-representation #:name 'binary32
                       #:bf->repr bigfloat->float32
                       #:repr->bf (lambda (x)
                                    (parameterize ([bf-precision 24])
                                      (bf x)))
                       #:ordinal->repr ordinal->float32
                       #:repr->ordinal float32->ordinal
                       #:total-bits 32
                       #:special-value? nan?))

(define <binary64>
  (make-representation #:name 'binary64
                       #:bf->repr direct-bigfloat->flonum
                       #:repr->bf (lambda (x)
                                    (parameterize ([bf-precision 53])
                                      (bf x)))
                       #:ordinal->repr ordinal->flonum
                       #:repr->ordinal flonum->ordinal
                       #:total-bits 64
                       #:special-value? nan?))

;; Contexts

(struct context (vars repr var-reprs) #:transparent)

;; Current context
(define *context* (make-parameter #f))

(define (context-extend ctx var repr)
  (struct-copy context
               ctx
               [vars (cons var (context-vars ctx))]
               [var-reprs (cons repr (context-var-reprs ctx))]))

(define (contexts-union ctxs)
  (unless ((non-empty-listof context?) ctxs)
    (raise-arguments-error 'contexts-union "expected a non-empty list of contexts" "ctxs" ctxs))
  (define out-repr (context-repr (first ctxs)))
  (define seen-reprs (make-hash))
  (for ([ctx (in-list ctxs)])
    (unless (equal? out-repr (context-repr ctx))
      (raise-arguments-error 'contexts-union "contexts must agree on output repr" "ctxs" ctxs))
    (for ([var (in-list (context-vars ctx))]
          [repr (in-list (context-var-reprs ctx))])
      (match (hash-ref seen-reprs var #f)
        [#f (hash-set! seen-reprs var repr)]
        [repr*
         #:when (equal? repr* repr)
         (void)]
        [_
         (raise-arguments-error 'contexts-union
                                "contexts must agree on shared variable reprs"
                                "ctxs"
                                ctxs)])))
  (context (hash-keys seen-reprs #t) out-repr (hash-values seen-reprs #t)))

(define (context-lookup ctx var)
  (dict-ref (map cons (context-vars ctx) (context-var-reprs ctx)) var))

(module+ test
  (require rackunit)

  (define ctx1 (context '(x y) <binary64> (list <binary64> <binary64>)))
  (define ctx2 (context '(y z) <binary64> (list <binary64> <binary64>)))
  (define ctx* (contexts-union (list ctx1 ctx2)))

  (check-equal? (context-vars ctx*) '(x y z))
  (check-equal? (context-var-reprs ctx*) (list <binary64> <binary64> <binary64>))
  (check-equal? (context-repr ctx*) <binary64>)

  (check-exn exn:fail?
             (lambda () (contexts-union (list ctx1 (context '(y) <binary64> (list <binary32>))))))

  (check-exn exn:fail?
             (lambda () (contexts-union (list ctx1 (context '(z) <binary32> (list <binary64>)))))))
