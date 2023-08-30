#lang racket
(require (for-syntax racket))
(require math/bigfloat rival)

(require ffi/unsafe 
         ffi/unsafe/alloc
         racket/runtime-path)

(provide arb 
    (rename-out [_arb? arb?]) 
    arb-neg 
    arb-abs 
    arb-add 
    arb-sub 
    arb-mul 
    arb-fma 
    arb-inv 
    arb-div 
    arb-exp 
    arb-log
    arb->ival
    ival->arb
    arb-if
    arb-acos
    arb-acosh
    arb-asin
    arb-asinh
    arb-atan
    arb-atanh
    arb-cbrt
    arb-ceil
    arb-cos
    arb-cosh
    arb-erf
    arb-erfc
    arb-exp
    arb-exp2
    arb-expm1
    arb-fabs
    arb-floor
    arb-lgamma
    arb-log
    arb-log10
    arb-log1p
    arb-log2
    arb-logb
    arb-rint
    arb-round
    arb-sin
    arb-sinh
    arb-sqrt
    arb-tan
    arb-tanh
    arb-tgamma
    arb-trunc
    arb-atan2
    arb-copysign
    arb-fdim
    arb-fmax
    arb-fmin
    arb-fmod
    arb-hypot
    arb-pow
    arb-remainder
    arb-==
    arb-!=
    arb-<
    arb->
    arb-<=
    arb->=
    arb-not
    arb-or
    arb-and
    arb-pi
    arb-e
    arb-bool
    mpfr->arb
    boolean->arb
    arb-lo
    arb-hi)

(define arb_t-size 48)
(define arb-precision (make-parameter 80))
;;(define libarb (ffi-lib "/usr/local/lib/libarb"))

(define-runtime-path libarb-so
  (case (system-type)
    [(macosx) '(so "libflint-arb.dylib")] ;; To be implemented
    [(windows) '(so "libflint-arb.dll")] ;; To be implemented
    [else '(so "libflint-arb")]))

(define libarb (ffi-lib libarb-so '("2" "") #:fail (λ () #f)))

(struct _arb (ptr)
        #:methods gen:custom-write
        [(define (write-proc a port mode)
           (define ptr (_arb-ptr a))
           (define s (_arb-get-str ptr (arb-precision) 0))
           (fprintf port "(arb ~s)" s))])
           

(define _mpfr_t _pointer)
(define _arb_t _pointer)

(define _arb_init (get-ffi-obj 'arb_init libarb (_fun _arb_t -> _void)))
(define _arb_clear (get-ffi-obj 'arb_clear libarb (_fun _arb_t -> _void)))
(define _arb-set-str (get-ffi-obj 'arb_set_str libarb (_fun _arb_t _string _slong -> _void)))
(define _arb-get-str (get-ffi-obj 'arb_get_str libarb (_fun _arb_t _slong _ulong -> _string)))
(define _arb-set-interval-mpfr (get-ffi-obj 'arb_set_interval_mpfr libarb (_fun _arb_t _mpfr_t _mpfr_t _slong -> _void)))
(define _arb-get-interval-mpfr (get-ffi-obj 'arb_get_interval_mpfr libarb (_fun _mpfr_t _mpfr_t _arb_t -> _void)))

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
   [(bigfloat? x)
    (string->arb (bigfloat->string x))]
   [(boolean? x)
    (ival x)]
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
          
(define (arb->ival ar)
  (bf-precision (arb-precision))
  (define a (bf 3))
  (define b (bf 3))
  (_arb-get-interval-mpfr a b (_arb-ptr ar))
  (ival a b))
  
;; Not that simple here, ival can be booleans!!
(define (ival->arb iv)
  (define ar (_arb-alloc))
  (define a (ival-lo iv))
  (define b (ival-hi iv))
  ;; Ideally this condition should never succeed
  (if (eq? (bigfloat-precision a) (bigfloat-precision b)) void (error "Precisions of ival's endpoints do not match"))
  (define prec (bigfloat-precision a))
  (_arb-set-interval-mpfr (_arb-ptr ar) a b prec)
  ar)
  
(define (mpfr->arb a b)
  (define ar (_arb-alloc))
  ;; Ideally this condition should never succeed
  (if (eq? (bigfloat-precision a) (bigfloat-precision b)) void (error "Precisions of ival's endpoints do not match"))
  (define prec (bigfloat-precision a))
  (_arb-set-interval-mpfr (_arb-ptr ar) a b prec)
  ar)
  
;; I guess it is a very slow way
(define (arb-lo x)
  (ival-lo (arb->ival x)))
(define (arb-hi x)
  (ival-hi (arb->ival x)))
  
(define (boolean->arb a b)
  (ival a b))
  
(define-arb-function (arb-add x y))
(define-arb-function (arb-sub x y))
(define-arb-function (arb-mul x y))
(define-arb-function (arb-fma x y z))
(define-arb-function (arb-div x y))
(define-arb-function (arb-pow x y))
(define-arb-function (arb-atan2 x y))


(define (arb-fmax x y)
   (ival->arb(ival-fmax (arb->ival x) (arb->ival y))))
   
(define (arb-if c x y)
  (ival-if (arb->ival c) x y))
  
(define (arb-copysign x y)
  (ival->arb(ival-copysign (arb->ival x) (arb->ival y))))

(define (arb-fdim x y)
  (ival->arb(ival-fdim (arb->ival x) (arb->ival y))))
  
(define (arb-fmin x y)
  (ival->arb(ival-fmin (arb->ival x) (arb->ival y))))
  
(define (arb-fmod x y)
  (error 'arb-fmod "Unimplemented"))
  
(define (arb-hypot x y)
  (error 'arb-hypot "Unimplemented"))
  

(define (arb-remainder x y)
  (error 'arb-remainder "Unimplemented"))

;; 1D
(define-arb-function (arb-neg x))
(define-arb-function (arb-abs x))
(define-arb-function (arb-inv x))
(define-arb-function (arb-exp x))
(define-arb-function (arb-acos x))
(define-arb-function (arb-acosh x))
(define-arb-function (arb-asin x))
(define-arb-function (arb-asinh x))
(define-arb-function (arb-atan x))
(define-arb-function (arb-atanh x))
(define-arb-function (arb-ceil x))
(define-arb-function (arb-cos x))
(define-arb-function (arb-cosh x))
(define-arb-function (arb-expm1 x))

;; This function is to be checked
(define-arb-function (arb-floor x))

;; To be checked
(define-arb-function (arb-lgamma x))
(define-arb-function (arb-log x))

(define-arb-function (arb-log1p x))
(define-arb-function (arb-sin x))
(define-arb-function (arb-sinh x))
(define-arb-function (arb-sqrt x))
(define-arb-function (arb-tan x))
(define-arb-function (arb-tanh x))


(define (arb-log2 x)
  (error 'arb-log10 "Unimplemented"))
(define (arb-log10 x)
  (error 'arb-log10 "Unimplemented"))
(define (arb-trunc x)
  (error 'arb-trunc "Unimplemented"))
(define (arb-tgamma x)  ;; just gamma
  (error 'arb-tgamma "Unimplemented"))
(define (arb-round x)
  (error 'arb-round "Unimplemented"))
(define (arb-rint x)
  (error 'arb-rint "Unimplemented"))
(define (arb-logb x)
  (error 'arb-logb "Unimplemented"))
(define (arb-fabs x)
  (error 'arb-fabs "Unimplemented"))
(define (arb-exp2 x)
  (error 'arb-exp2 "Unimplemented"))
(define (arb-cbrt x)
  (error 'arb-cbrt "Unimplemented"))
(define (arb-erf x)
  (error 'arb-erf "Unimplemented"))
(define (arb-erfc x)
  (error 'arb-erfc "Unimplemented"))

  
(define (arb-==)
  (error 'arb-== "Unimplemented"))
(define (arb-!=)
  (error 'arb-!= "Unimplemented"))
(define (arb-<)
  (error 'arb-< "Unimplemented"))
(define (arb->)
  (error 'arb-> "Unimplemented"))
(define (arb-<=)
  (error 'arb-<= "Unimplemented"))
(define (arb->=)
  (error 'arb->= "Unimplemented"))
  
(define (arb-not x)
  (error 'arb-not "Unimplemented"))
(define (arb-and x)
  (error 'arb-and "Unimplemented"))
(define (arb-or x)
  (error 'arb-or "Unimplemented"))
  
(define (arb-pi)
  (arb pi.bf))
  
(define (arb-e)
  (arb (bfexp 1.bf)))
  
(define (arb-bool b)
  (arb b))
