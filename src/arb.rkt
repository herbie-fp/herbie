#lang racket
(require (for-syntax racket))

(require ffi/unsafe ffi/unsafe/alloc)

(define arb_t-size 48)
(define arb-precision (make-parameter 80))
(define libarb (ffi-lib "/usr/local/lib/libarb.dylib"))

(struct _arb (ptr)
        #:methods gen:custom-write
        [(define (write-proc a port mode)
           (define ptr (_arb-ptr a))
           (define s (_arb-get-str ptr (arb-precision) 0))
           (fprintf port "(arb ~s)" s))])

(define _arb_init (get-ffi-obj 'arb_init libarb (_fun _pointer -> _void)))
(define _arb_clear (get-ffi-obj 'arb_clear libarb (_fun _pointer -> _void)))
(define _arb-set-str (get-ffi-obj 'arb_set_str libarb (_fun _pointer _string _slong -> _void)))
(define _arb-get-str (get-ffi-obj 'arb_get_str libarb (_fun _pointer _slong _ulong -> _string)))

(define _arb-alloc
  ((allocator (λ (v) (_arb_clear (_arb-ptr v))))
   (λ ()
     (define mem (malloc arb_t-size 'atomic))
     (_arb_init mem)
     (_arb mem))))

(define (string->arb s)
  (define v (_arb-alloc))
  (_arb-set-str (_arb-ptr v) s (arb-precision))
  v)

(define (arb x)
  (cond
   [(string? x) (string->arb x)]
   [(integer? x) (string->arb (~a x))]
   [(and (rational? x) (not (exact? x)))
    (string->arb (~a x))]
   [(and (rational? x) (exact? x))
    (arb-div (string->arb (~a (numerator x))) (string->arb (~a (denominator x))))]
   [(nan? x)
    (string->arb "nan")]
   [(infinite? x)
    (string->arb "inf")]
   [else
    (error 'arb "Unknown value ~a" x)]))

(define-syntax define-arb-function
  (λ (stx)
    (syntax-case stx ()
      [(_ (name n ...))
       (let* ([num-arguments (- (length (syntax-e (cadr (syntax-e stx)))) 1)]
              [ffi-name (datum->syntax #'name (string->symbol (string-replace (~a (syntax-e #'name)) "-" "_")))]
              [args (build-list num-arguments (const #'_pointer))])
         #`(define name
             (let ([ffi-fn (get-ffi-obj '#,ffi-name libarb (_fun _pointer #,@args _slong -> _void))])
               (procedure-rename
                (λ (n ...)
                  (define v (_arb-alloc))
                  (ffi-fn (_arb-ptr v) (_arb-ptr n) ... (arb-precision))
                  v)
                'name))))])))

(define-arb-function (arb-neg x))
(define-arb-function (arb-abs x))
(define-arb-function (arb-add x y))
(define-arb-function (arb-sub x y))
(define-arb-function (arb-mul x y))
(define-arb-function (arb-fma x y z))
(define-arb-function (arb-inv x))
(define-arb-function (arb-div x y))
(define-arb-function (arb-exp x))
(define-arb-function (arb-log x))

(define (arb-fmod x y)
  (error 'arb-fmod "Unimplmeented"))
