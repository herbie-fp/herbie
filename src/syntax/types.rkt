#lang racket

(require math/bigfloat
         math/base
         math/flonum
         "../utils/errors.rkt")

(provide (struct-out representation)
         (struct-out array-representation)
         repr->prop
         array-representation-base
         array-representation-shape
         array-representation-size
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

(struct array-representation representation (elem len) #:transparent)

(define (array-representation-base repr)
  (let loop ([repr repr])
    (if (array-representation? repr)
        (loop (array-representation-elem repr))
        repr)))

(define (array-representation-shape repr)
  (if (array-representation? repr)
      (cons (array-representation-len repr)
            (array-representation-shape (array-representation-elem repr)))
      '()))

(define (array-representation-size repr)
  (if (array-representation? repr)
      (* (array-representation-len repr) (array-representation-size (array-representation-elem repr)))
      1))

(define (array-type elem-type len)
  `(array ,elem-type ,len))

;; Converts a representation into a rounding property
(define (repr->prop repr)
  (match repr
    [(? array-representation?) (repr->prop (array-representation-base repr))]
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

(define (make-array-representation #:elem elem-repr #:len len)
  (unless (exact-positive-integer? len)
    (raise-herbie-error "Arrays require a positive length, got ~a" len))
  (define array-ty (array-type (representation-type elem-repr) len))
  (define name (string->symbol (format "array~a-~a" (representation-name elem-repr) len)))
  (define total-bits (* len (representation-total-bits elem-repr)))
  ;; TODO: Array representations currently inherit scalar conversion slots.
  ;; These should not be called for arrays; we'll clean up the hierarchy later.
  (array-representation name array-ty void void void void total-bits void elem-repr len))

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
