#lang racket

(require math/bigfloat
         math/base
         math/flonum
         "../utils/errors.rkt")

(provide (struct-out representation)
         (struct-out array-representation)
         repr->prop
         shift
         unshift
         <bool>
         <binary32>
         <binary64>
         (struct-out context)
         *context*
         context-extend
         context-lookup
         make-representation
         make-array-representation)

;; Representations

(struct representation
        (name type bf->repr repr->bf ordinal->repr repr->ordinal total-bits special-value?)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation ~a>" (representation-name repr)))])

(struct array-representation representation (elem dims) #:transparent)

;; Converts a representation into a rounding property
(define (repr->prop repr)
  (match repr
    [(? representation?)
     (match (representation-type repr)
       ['bool '()]
       ['real (list (cons ':precision (representation-name repr)))]
       ['array (repr->prop (array-representation-elem repr))])]))

(define (make-representation #:name name
                             #:bf->repr bf->repr
                             #:repr->bf repr->bf
                             #:ordinal->repr ordinal->repr
                             #:repr->ordinal repr->ordinal
                             #:total-bits total-bits
                             #:special-value? special-value?)
  (representation name 'real bf->repr repr->bf ordinal->repr repr->ordinal total-bits special-value?))

(define (make-array-representation #:elem elem-repr #:dims dims)
  (unless (and (list? dims) (not (null? dims)) (andmap exact-positive-integer? dims))
    (raise-herbie-error "Arrays require one or more positive dimensions, got ~a" dims))
  (define name
    (string->symbol
     (format "array~a-~a" (representation-name elem-repr) (string-join (map ~a dims) "x"))))
  (define len (apply * dims))
  (define elem-bf->repr (representation-bf->repr elem-repr))
  (define elem-repr->bf (representation-repr->bf elem-repr))
  (define elem-ordinal->repr (representation-ordinal->repr elem-repr))
  (define elem-repr->ordinal (representation-repr->ordinal elem-repr))
  (define elem-special? (representation-special-value? elem-repr))
  (define total-bits (* len (representation-total-bits elem-repr)))
  (array-representation name
                        'array
                        (lambda (xs)
                          (for/vector ([x (in-vector xs)])
                            (elem-bf->repr x)))
                        (lambda (xs)
                          (for/vector ([x (in-vector xs)])
                            (elem-repr->bf x)))
                        (lambda (xs)
                          (for/vector ([x (in-vector xs)])
                            (elem-ordinal->repr x)))
                        (lambda (xs)
                          (for/vector ([x (in-vector xs)])
                            (elem-repr->ordinal x)))
                        total-bits
                        (lambda (xs)
                          (for/or ([x (in-vector xs)])
                            (elem-special? x)))
                        elem-repr
                        dims))

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
                       #:bf->repr bigfloat->flonum
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

(define (context-lookup ctx var)
  (dict-ref (map cons (context-vars ctx) (context-var-reprs ctx)) var))
