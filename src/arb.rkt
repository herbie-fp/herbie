#lang racket
(require (for-syntax racket))
(require math/bigfloat rival)

(require ffi/unsafe 
         ffi/unsafe/alloc
         racket/runtime-path)

(provide arb 
    (rename-out [_arb? arb?] [_arb-prec arb-prec] [_arb-div arb-div] [_arb_clear arb-clear] [_arb-sqrt arb-sqrt] 
                [_arb-log arb-log] [_arb-tan arb-tan] [_arb-log1p arb-log1p] [arb-const-pi arb-pi]
                [arb-const-e arb-e] [pow arb-pow]) 
    arb-neg 
    arb-abs 
    arb-add 
    arb-sub 
    arb-mul 
    arb-fma 
    arb-inv
    arb-exp
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
    arb-log10
    arb-log2
    arb-logb
    arb-rint
    arb-round
    arb-sin
    arb-sinh
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
    arb-bool
    mpfr->arb
    boolean->arb
    arb-lo
    arb-hi
    arb-error?
    arb-fix?
    _arb-ptr
    _arb-contains-negative 
    _arb-contains-nonpositive
    _arb-get-interval-mpfr
    arb-not-error
    arb-inf
    arb-pos-inf
    arb-neg-inf
    arb->arf
    arf-equal
    arf-is-finite
    arf-is-nan
    _arf-ptr
    _arb-err
    _arb-err?
    arb-set-round
    arf-get-d)

(define arb_t-size 48)
(define arf_t-size 128)

(define-runtime-path libarb-so
  (case (system-type)
    [(macosx) '(so "libflint-arb.dylib")] ;; To be implemented
    [(windows) '(so "libflint-arb.dll")] ;; To be implemented
    [else '(so "libflint-arb")]))

(define libarb (ffi-lib libarb-so '("2" "13" "1" "") #:fail (λ () #f)))

(struct _arb (ptr prec err? err)
        #:methods gen:custom-write
        [(define (write-proc a port mode)
           (define ptr (_arb-ptr a))
           (define s (_arb-get-str ptr (_arb-prec a) 0))
           (fprintf port "(arb ~s) ~e ~e" s (_arb-err? a) (_arb-err a)))])

(struct _arf (ptr prec)
        #:methods gen:custom-write
        [(define (write-proc a port mode)
           (define ptr (_arf-ptr a))
           (define s (_arf-get-str ptr (_arf-prec a)))
           (fprintf port "(arf ~s)" s ))])
           

(define _mpfr_t _pointer)
(define _arb_t _pointer)
(define _arf_t _pointer)
(define _arf_rnd_t _int)

(define _arb_init (get-ffi-obj 'arb_init libarb (_fun _arb_t -> _void)))
(define _arb_clear (get-ffi-obj 'arb_clear libarb (_fun _arb_t -> _void)))
(define _arb-set-str (get-ffi-obj 'arb_set_str libarb (_fun _arb_t _string _slong -> _void)))
(define _arb-get-str (get-ffi-obj 'arb_get_str libarb (_fun _arb_t _slong _ulong -> _string)))
(define _arb-set-interval-mpfr (get-ffi-obj 'arb_set_interval_mpfr libarb (_fun _arb_t _mpfr_t _mpfr_t _slong -> _void)))
(define _arb-get-interval-mpfr (get-ffi-obj 'arb_get_interval_mpfr libarb (_fun _mpfr_t _mpfr_t _arb_t -> _void)))

(define _arb-is-exact (get-ffi-obj 'arb_is_exact libarb (_fun _arb_t -> _int)))
(define _arb-contains-negative (get-ffi-obj 'arb_contains_negative libarb ( _fun _arb_t -> _int)))
(define _arb-is-negative (get-ffi-obj 'arb_is_negative libarb ( _fun _arb_t -> _int)))
(define _arb-is-positive (get-ffi-obj 'arb_is_positive libarb ( _fun _arb_t -> _int)))
(define _arb-is-nonpositive (get-ffi-obj 'arb_is_nonpositive libarb ( _fun _arb_t -> _int)))
(define _arb-is-nonnegative (get-ffi-obj 'arb_is_nonnegative libarb ( _fun _arb_t -> _int)))
(define _arb-contains-nonpositive (get-ffi-obj 'arb_contains_nonpositive libarb ( _fun _arb_t -> _int)))
(define _arb-contains-positive (get-ffi-obj 'arb_contains_positive libarb ( _fun _arb_t -> _int)))
(define _arb-contains-zero (get-ffi-obj 'arb_contains_zero libarb ( _fun _arb_t -> _int)))
(define _arb-is-zero (get-ffi-obj 'arb_is_zero libarb ( _fun _arb_t -> _int)))
(define _arb-is-int (get-ffi-obj 'arb_is_int libarb ( _fun _arb_t -> _int)))
(define _arb-contains-int (get-ffi-obj 'arb_contains_int libarb ( _fun _arb_t -> _int)))

(define _arb-zero-pm-inf (get-ffi-obj 'arb_zero_pm_inf libarb ( _fun _arb_t -> _void)))
(define _arb-neg-inf (get-ffi-obj 'arb_neg_inf libarb ( _fun _arb_t -> _void)))
(define _arb-pos-inf (get-ffi-obj 'arb_pos_inf libarb ( _fun _arb_t -> _void)))
(define _arb-set (get-ffi-obj 'arb_set libarb ( _fun _arb_t _arb_t -> _void)))
(define _arb-get-interval-arf (get-ffi-obj 'arb_get_interval_arf libarb ( _fun _arf_t _arf_t _arb_t _slong -> _void)))
(define _arb-set-round (get-ffi-obj 'arb_set_round libarb ( _fun _arb_t _arb_t _slong -> _void)))
(define _arb-set-si (get-ffi-obj 'arb_set_si libarb ( _fun _arb_t _slong -> _void)))


(define _arf_init (get-ffi-obj 'arb_init libarb (_fun _arf_t -> _void)))
(define _arf_clear (get-ffi-obj 'arb_clear libarb (_fun _arf_t -> _void)))
(define _arf-load-str (get-ffi-obj 'arf_load_str libarb (_fun _arf_t _string -> _int)))
(define _arf-get-str (get-ffi-obj 'arf_get_str libarb (_fun _arf_t _slong -> _string)))
(define _arf-equal (get-ffi-obj 'arf_equal libarb ( _fun _arf_t _arf_t -> _int)))
(define _arf-is-finite (get-ffi-obj 'arf_is_finite libarb ( _fun _arf_t -> _int)))
(define _arf-is-nan (get-ffi-obj 'arf_is_nan libarb ( _fun _arf_t -> _int)))
(define _arf-get-d (get-ffi-obj 'arf_get_d libarb ( _fun _arf_t _arf_rnd_t -> _double)))

(define _arb-alloc
  ((allocator (λ (v) (_arb_clear (_arb-ptr v))))
   (λ ([err? #f] [err #f])
     (define mem (malloc arb_t-size 'atomic))
     (_arb_init mem)
     (_arb mem (bf-precision) err? err))))

(define _arf-alloc
  ((allocator (λ (v) (_arf_clear (_arf-ptr v))))
   (λ ()
     (define mem (malloc arf_t-size 'atomic))
     (_arf_init mem)
     (_arf mem (bf-precision)))))

(define (string->arb s)
  (define err? (bfnan? (string->bigfloat s)))
  (define v (_arb-alloc err? #f))
  (_arb-set-str (_arb-ptr v) s (_arb-prec v))
  v)

(define (arb x)
  (cond
   [(string? x) (arb (string->bigfloat x))]
   [(integer? x) (arb-set-si x)]
   [(and (rational? x) (not (exact? x)))
    (string->arb (~a x))]
   [(and (rational? x) (exact? x))
    (arb-div (string->arb (~a (numerator x))) (string->arb (~a (denominator x))))]
   [(bigfloat? x)
    (if (bfinfinite? x)
        (if (bfpositive? x)
            (arb-pos-inf)
            (arb-neg-inf))
        (mpfr->arb x x))]
   [(boolean? x)
    (ival x)]
   [(nan? x)
    (string->arb "nan")]
   [(infinite? x)
    (if (positive? x)
        (arb-pos-inf)
        (arb-neg-inf))]
   [else
    (error 'arb "Unknown value ~a" x)]))

(define (arb-pos-inf)
  (define v (_arb-alloc #f #f))
  (_arb-pos-inf (_arb-ptr v))
  v)

(define (arb-neg-inf)
  (define v (_arb-alloc #f #f))
  (_arb-neg-inf (_arb-ptr v))
  v)

(define (arb-inf)
  (define v (_arb-alloc #f #f))
  (_arb-zero-pm-inf (_arb-ptr v))
  v)


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
                (λ (n ... [err? #f] [err #f])
                    (define v (_arb-alloc (or err? (_arb-err? n) ...) (or err (_arb-err n) ...)))
                    (ffi-fn (_arb-ptr v) (_arb-ptr n) ... (_arb-prec v))
                  v)
                'name))))])))
          
(define-syntax define-arb-function-additional-arg
  (λ (stx)
    (syntax-case stx ()
      [(_ (name n ... k))
       (let* ([num-arguments (- (length (syntax-e (cadr (syntax-e stx)))) 2)]
              [ffi-name (datum->syntax #'name (string->symbol (string-replace (~a (syntax-e #'name)) "-" "_")))]
              [args (append (build-list num-arguments (const #'_pointer)) (cons #'_ulong '()))])
         #`(define name
             (let ([ffi-fn (get-ffi-obj '#,ffi-name libarb (_fun _pointer #,@args _slong -> _void))])
               (procedure-rename
                (λ (n ... k [err? #f] [err #f])
                  (define v (_arb-alloc (or err? (_arb-err? n) ...) (or err (_arb-err n) ...)))
                  (ffi-fn (_arb-ptr v) (_arb-ptr n) ... k (_arb-prec v))
                  v)
                'name))))])))

;(define-syntax define-arb-cmp-function)

(define (arb-is-int x)
  (if (zero? (_arb-is-int (_arb-ptr x))) #f #t))

(define (arb-is-positive x)
  (if (zero? (_arb-is-positive (_arb-ptr x))) #f #t))

(define (arb-is-nonnegative x)
  (if (zero? (_arb-is-nonnegative (_arb-ptr x))) #f #t))

(define (arb-contains-int x)
  (if (zero? (_arb-contains-int (_arb-ptr x))) #f #t))

(define (arb-contains-zero x)
  (if (zero? (_arb-contains-int (_arb-ptr x))) #f #t))

(define (arb-contains-nonpositive x)
  (if (zero? (_arb-contains-nonpositive (_arb-ptr x))) #f #t))

(define (arb-contains-positive x)
  (if (zero? (_arb-contains-positive (_arb-ptr x))) #f #t))

(define (arb-contains-negative x)
  (if (zero? (_arb-contains-negative (_arb-ptr x))) #f #t))

(define (arb-is-negative x)
  (if (zero? (_arb-is-negative (_arb-ptr x))) #f #t))

(define (arb-is-zero x)
  (if (zero? (_arb-is-zero (_arb-ptr x))) #f #t))

(define (arb-set-si x)
  (define v (_arb-alloc #f #f))
  (_arb-set-si (_arb-ptr v) x)
  v)

;; 4 = RND_NEAREST (got from https://github.com/fredrik-johansson/arb/blob/master/fmpr.h)
(define (arf-get-d x)
  (_arf-get-d (_arf-ptr x) 4))
  

(define (arb-set-round x prec)
  (parameterize ([bf-precision prec])
    (define v (_arb-alloc (_arb-err? x) (_arb-err x)))
    (_arb-set-round (_arb-ptr v) (_arb-ptr x) prec)
    v))
  
(define (arb-copy-with-flags ar err? err)
  (define v (_arb-alloc (or err? (_arb-err? ar)) (or err (_arb-err ar))))
  (_arb-set (_arb-ptr v) (_arb-ptr ar))
  v)

(define (arb->ival ar)
  (if (ival? ar) ar
    (parameterize ([bf-precision (_arb-prec ar)])
      (let ([a (bf 3)] [b (bf 3)])
        (_arb-get-interval-mpfr a b (_arb-ptr ar))
        (ival-then
          (ival-assert (ival (not (or (_arb-err? ar) (bfnan? a) (bfnan? b))) (not (_arb-err ar))) #t)
          (ival (if (bfnan? a) (bf "-inf") (bfcopy a)) (if (bfnan? b) (bf "+inf") (bfcopy b))))))))


;; Not that simple here, ival can be booleans
(define (ival->arb iv)
  (if (_arb? iv) iv
    (parameterize ([bf-precision 
                   (if (> (bigfloat-precision (ival-lo iv)) (bigfloat-precision (ival-hi iv))) 
                       (bigfloat-precision (ival-lo iv)) 
                       (bigfloat-precision (ival-hi iv)))])
      (let ([ar (_arb-alloc (ival-err? iv) (ival-err iv))] [a (ival-lo iv)] [b (ival-hi iv)])
        (_arb-set-interval-mpfr (_arb-ptr ar) (bfcopy a) (bfcopy b) (bf-precision))
        ar))))


;; This function is to be corrected from the precision point
(define (mpfr->arb a b)
  (define ar (_arb-alloc (or (bfnan? a) (bfnan? b)) #f))
  (if (bf< a b)
      (_arb-set-interval-mpfr (_arb-ptr ar) (bfcopy a) (bfcopy b) (bf-precision))
      (_arb-set-interval-mpfr (_arb-ptr ar) (bfcopy b) (bfcopy a) (bf-precision)))
  ar)

(define (arb->arf ar)
  (define a (_arf-alloc))
  (define b (_arf-alloc))
  (_arb-get-interval-arf (_arf-ptr a) (_arf-ptr b) (_arb-ptr ar) (_arb-prec ar))
  (list a b))

  
(define (arb-fix? x)
  (if (zero? (_arb-is-exact (_arb-ptr x))) #f #t))

(define (arf-equal x y)
  (if (zero? (_arf-equal (_arf-ptr x) (_arf-ptr y))) #f #t))
 
(define (arf-is-finite x)
  (if (zero? (_arf-is-finite (_arf-ptr x))) #f #t))

(define (arf-is-nan x)
  (if (zero? (_arf-is-nan (_arf-ptr x))) #f #t))

;; Veeery slow (not a surprise)
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

(define (_arb-div x y)
  (define err? (or (_arb-err? x) (_arb-err? y)
                (if
                 (zero? (_arb-contains-zero (_arb-ptr y))) #f #t)))
  (define err (or (_arb-err x) (_arb-err y)
                  (if (zero? (_arb-is-zero (_arb-ptr y))) #f #t)))
  (arb-div x y err? err))
  
(define (_arb-sqrt x)
  (define err? (or (_arb-err? x)
                   (if (eq? (_arb-contains-negative (_arb-ptr x)) 1) #t #f)
               ))
  (define err (or (_arb-err x)
                  (if (eq? (_arb-is-negative (_arb-ptr x)) 1) #t #f)
              ))
  (arb-sqrt x err? err))
  
(define (_arb-log x)
  (define err? (or (_arb-err? x)
                   (if (eq? (_arb-contains-nonpositive (_arb-ptr x)) 1) #t #f)
               ))
  (define err (or (_arb-err x)
                  (if (eq? (_arb-is-nonpositive (_arb-ptr x)) 1) #t #f)
              ))
  (arb-log x err? err))
  
(define (_arb-tan x)
  (if (arb-fix? (arb-floor (arb-sub (arb-div x (arb-const-pi)) (arb (bf "0.5")))))
    (arb-tan x)
    (arb "nan")))

(define-arb-function (arb-pow x y))
(define (pow x y)
    (cond
      [(arb-is-negative x)
       (arb-pow x y
                (and (arb-contains-int y) (not (arb-is-int y))) ; if y contains integers, but y is not integer
                (not (arb-contains-int y)))]                    ; if y does not contain any integers
      [(and (arb-contains-negative x) (arb-contains-positive x))
       (arb-pow x y
                (and (arb-contains-int y) (not (arb-is-int y))) ; if y contains integers, but y is not integer
                #f)]
      [(arb-is-nonnegative x)
       (arb-pow x y
                (and (arb-contains-zero x) (arb-contains-negative y))
                (and (arb-is-zero x) (arb-is-negative y)))]))
     
;(define (arb-pow x y)
;  (ival->arb (ival-pow (arb->ival x) (arb->ival y))))

(define-arb-function (arb-atan2 x y))
(define-arb-function (arb-hypot x y))


(define (arb-fmax x y)
   (ival->arb (ival-fmax (arb->ival x) (arb->ival y))))
   
(define (arb-if c x y)
  (ival->arb(ival-if (arb->ival c) (arb->ival x) (arb->ival y))))
  
(define (arb-copysign x y)
  (ival->arb(ival-copysign (arb->ival x) (arb->ival y))))

(define (arb-fdim x y)
  (ival->arb(ival-fdim (arb->ival x) (arb->ival y))))
  
(define (arb-fmin x y)
  (ival->arb(ival-fmin (arb->ival x) (arb->ival y))))
  
(define (arb-fmod x y)
  (ival->arb(ival-fmod (arb->ival x) (arb->ival y))))
  
(define (arb-fabs x)
  (ival->arb(ival-fabs (arb->ival x))))

(define (arb-remainder x y)
  (ival->arb(ival-remainder (arb->ival x) (arb->ival y))))
  
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
(define-arb-function (arb-cos x))
(define-arb-function (arb-cosh x))
(define-arb-function (arb-expm1 x))
(define-arb-function (arb-floor x))
(define-arb-function (arb-ceil x))
(define-arb-function (arb-lgamma x))
(define-arb-function (arb-log x))
(define-arb-function (arb-log1p x))
(define-arb-function (arb-sin x))
(define-arb-function (arb-sinh x))
(define-arb-function (arb-sqrt x))
(define-arb-function (arb-tan x))
(define-arb-function (arb-tanh x))
(define-arb-function (arb-gamma x))

; Consts
(define-arb-function (arb-const-pi))
(define-arb-function (arb-const-e))

(define (arb-tgamma x)
  (arb-gamma x))
(define-arb-function-additional-arg (arb-root x k))
(define-arb-function-additional-arg (arb-log-base-ui x k))

(define (arb-cbrt x)
  (arb-root x 3))

(define (arb-log2 x)
  (define err? (or (_arb-err? x) 
                   (if (eq? (_arb-contains-nonpositive (_arb-ptr x)) 1) #t #f)
               ))
  (define err (or (_arb-err x)
                  (if (eq? (_arb-is-nonpositive (_arb-ptr x)) 1) #t #f)
              ))
  (arb-log-base-ui x 2 err? err))
  
(define (arb-log10 x)
  (define err? (or (_arb-err? x) 
                   (if (eq? (_arb-contains-nonpositive (_arb-ptr x)) 1) #t #f)
               ))
  (define err (or (_arb-err x)
                  (if (eq? (_arb-is-nonpositive (_arb-ptr x)) 1) #t #f)
              ))
  (arb-log-base-ui x 10 err? err))
  
(define (_arb-log1p x)
  (define err? (or (_arb-err? x) 
                   (if (eq? (_arb-contains-nonpositive (_arb-ptr (arb-add x (arb (bf 1))))) 1) 
                     #t 
                     #f)))
  (define err (or (_arb-err x)
                  (if (eq? (_arb-is-nonpositive (_arb-ptr (arb-add x (arb (bf 1))))) 1) 
                    #t 
                    #f)))
  (arb-log1p x err? err)) 
  
(define (arb-exp2 x)
  (arb-pow (arb 2.bf) x))
  
(define (arb-trunc x)
  (ival->arb(ival-trunc (arb->ival x))))
(define (arb-round x)
  (ival->arb(ival-round (arb->ival x))))
(define (arb-rint x)
  (ival->arb(ival-rint (arb->ival x))))
(define (arb-logb x)
  (ival->arb(ival-logb (arb->ival x))))

(define (arb-erf x)
  (ival->arb(ival-erf (arb->ival x))))
(define (arb-erfc x)
  (ival->arb(ival-erfc (arb->ival x))))
  
(define arb-==
  (lambda xs
    (apply ival-==(map arb->ival xs))))
    
(define arb-!=
  (lambda xs
    (apply ival-!=(map arb->ival xs))))
    
(define arb-<
  (lambda xs
    (apply ival-<(map arb->ival xs))))
    
(define arb->
  (lambda xs
    (apply ival->(map arb->ival xs))))
    
(define arb-<=
  (lambda xs
    (apply ival-<= (map arb->ival xs))))
    
(define arb->=
  (lambda xs
    (apply ival->= (map arb->ival xs))))
  
(define (arb-not x)
  (ival-not (arb->ival x)))
  
(define (arb-error? x)
  (ival-error? (arb->ival x)))

(define (arb-not-error x)
  (if (_arb? x) 
      (ival (not (_arb-err? x)) (not (_arb-err x)))
      (ival (not (ival-err? x)) (not (ival-err x)))))


(define arb-and
  (lambda xs
    (apply ival-and (map arb->ival xs))))
    
(define arb-or
  (lambda xs
    (apply ival-or (map arb->ival xs))))
  
(define (arb-bool b)
  (arb b))

;(define (arb-then-imitate a . as)
;  (ival (ival-lo (last (cons a as))) (ival-hi (last (cons a as)))
;        (or (ival-err? a) (ormap ival-err? as))
;        (or (ival-err a) (ormap ival-err as))))
