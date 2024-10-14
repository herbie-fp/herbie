#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector)

(provide (all-defined-out))

(define qd-lib (ffi-lib "./QD/src/.libs/libqd"))

(define dde1 2.718281828459045091e+00)
(define dde2 1.445646891729250158e-16)
(define ddpi1 3.141592653589793116e+00)
(define ddpi2 1.224646799147353207e-16)

(define-ffi-definer define-qd qd-lib)

(define-syntax define-dd-binary
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-qd name (_fun _pointer _pointer _pointer -> _void)) ...)]))

(define-syntax define-dd-unary
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-qd name (_fun _pointer _pointer -> _void)) ...)]))

(define-qd c_dd_nroot (_fun _pointer _int _pointer -> _void))

(define-dd-binary c_dd_add c_dd_sub c_dd_mul c_dd_div)

(define-dd-unary c_dd_sqrt
                 c_dd_sqr
                 c_dd_abs
                 c_dd_exp
                 c_dd_log
                 c_dd_log10
                 c_dd_sin
                 c_dd_cos
                 c_dd_tan
                 c_dd_aint)

(define (nan.dd)
  (values +nan.0 +nan.0))

(define (+inf.dd)
  (values +inf.0 0.0))

(define (-inf.dd)
  (values -inf.0 0.0))

(define log2-hi 6.931471805599452862e-01)
(define log2-lo 2.319046813846299558e-17)

(define (log2.dd)
  (values log2-hi log2-lo))

(define (0.dd)
  (values 0.0 0.0))

(define (dd= x1 x2 y1 [y2 0])
  (and (= x1 y1) (= x2 y2)))

(define (dd> x1 x2 y1 [y2 0])
  (or (> x1 y1) (and (= x1 y1) (> x2 y2))))

(define (dd< x1 x2 y1 [y2 0])
  (dd> y1 y2 x1 x2))

(define (dd>= x1 x2 y1 [y2 0])
  (or (> x1 y1) (and (= x1 y1) (>= x2 y2))))

(define (dd<= x1 x2 y1 [y2 0])
  (dd>= y1 y2 x1 x2))

(define (ddzero? x1 [x2 0])
  (dd= x1 x2 0))

(define (ddinfinite? x1 [x2 0])
  (infinite? x1))

(define (ddposinf? x1 [x2 0])
  (dd= x1 x2 +inf.0))

(define (ddneginf? x1 [x2 0])
  (dd= x1 x2 -inf.0))

(define (ddfinite? x1 [x2 0])
  (nor (ddinfinite? x1 x2) (ddnan? x1 x2)))

(define (ddpositive? x1 [x2 0])
  (dd> x1 x2 0.0))

(define (ddnegative? x1 [x2 0])
  (dd< x1 x2 0.0))

(define (ddnan? x1 [x2 0])
  (nan? x1))

(define (ddsamesign? x1 x2 y1 [y2 0])
  (or (and (ddpositive? x1 x2) (ddpositive? y1 y2)) (and (ddnegative? x1 x2) (ddnegative? y1 y2))))

(define (pi.dd)
  (values 3.141592653589793116e+00 1.224646799147353207e-16))

(define (dd+ x1 x2 y1 [y2 0.0])
  (cond
    ; x + +-inf = +-inf
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2)) (values y1 y2)]
    ; +-inf + x = +-inf
    [(and (ddinfinite? x1 x2) (ddfinite? y1 y2)) (values x1 x1)]
    ; +-inf + +-inf = +inf
    ; +-inf + -+inf = +nan
    [(and (ddinfinite? x1 x2) (ddinfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (values x1 x2)
         (nan.dd))]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_add (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd- x1 x2 y1 [y2 0.0])
  (cond
    ; x - +-inf = +-inf
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2)) (values (- y1) y2)]
    ; +-inf - x = +-inf
    [(and (ddinfinite? x1 x2) (ddfinite? y1 y2)) (values x1 x2)]
    ; +-inf - +-inf = +nan
    ; +-inf - -+inf = +inf
    [(and (ddinfinite? x1 x2) (ddinfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (nan.dd)
         (values x1 x2))]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_sub (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd* x1 x2 y1 [y2 0.0])
  (cond
    ; 0 * inf = nan
    [(and (ddzero? x1 x2) (ddinfinite? y1 y2)) (nan.dd)]
    [(and (ddzero? y1 y2) (ddinfinite? x1 x2)) (nan.dd)]
    ; inf * inf = inf
    [(and (ddinfinite? x1 x2) (ddinfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (+inf.dd)
         (-inf.dd))]
    ; x * inf = inf
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (+inf.dd)
         (-inf.dd))]
    [(and (ddinfinite? x1 x2) (ddfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (+inf.dd)
         (-inf.dd))]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_mul (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd/ x1 x2 y1 [y2 0.0])
  (cond
    [(and (ddinfinite? x1 x2) (ddinfinite? y1 y2)) (nan.dd)]
    [(and (ddinfinite? x1 x2) (ddfinite? y1 y2)) (values x1 x2)]
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2)) (0.dd)]
    [(and (ddzero? x1 x2) (ddzero? x1 x2)) (nan.dd)]
    [(and (ddpositive? x1 x2) (ddzero? x1 x2)) (+inf.dd)]
    [(and (ddnegative? x1 x2) (ddzero? x1 x2)) (-inf.dd)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_div (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddsqrt x1 [x2 0])
  (cond
    [(ddposinf? x1 x2) (values x1 x2)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define c (make-f64vector 2))
     (c_dd_sqrt (f64vector->cpointer a) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddcbrt x1 [x2 0])
  (cond
    [(ddinfinite? x1 x2) (+inf.dd)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define c (make-f64vector 2))
     (c_dd_nroot (f64vector->cpointer a) 3 (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddint? x1 [x2 0])
  (let*-values ([(a1 a2) (ddaint x1 x2)])
    (dd= x1 x2 a1 a2)))

(define (ddneg x1 [x2 0])
  (dd- 0.0 0.0 x1 x2))

(define (ddlog x1 [x2 0.0])
  (cond
    [(ddzero? x1 x2) (-inf.dd)]
    [(ddposinf? x1 x2) (+inf.dd)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define c (make-f64vector 2))
     (c_dd_log (f64vector->cpointer a) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddlog2 x1 [x2 0.0])
  (cond
    [(ddzero? x1 x2) (-inf.dd)]
    [(ddposinf? x1 x2) (+inf.dd)]
    [else
     (let*-values ([(a1 a2) (ddlog x1 x2)]
                   [(b1 b2) (log2.dd)])
       (dd/ a1 a2 b1 b2))]))

(define (ddexp2 x1 [x2 0])
  (let*-values ([(l1 l2) (log2.dd)]
                [(e1 e2) (dd* x1 x2 l1 l2)])
    (ddexp e1 e2)))

(define (ddexpt x1 x2 y1 [y2 0.0])
  (define isint? (ddint? y1 y2))
  (cond
    [(ddzero? y1 y2) (values 1.0 0.0)]
    [(dd>= x1 x2 0.0 0.0)
     (let*-values ([(a1 a2) (ddlog x1 x2)]
                   [(a1 a2) (dd* y1 y2 a1 a2)])
       (ddexp a1 a2))]
    [(and (ddnegative? x1 x2) (ddposinf? y1 y2)) (+inf.dd)]
    [(and (ddnegative? x1 x2) (ddneginf? y1 y2)) (-inf.dd)]
    [(and (ddneginf? x1 x2) (ddpositive? y1 y2))
     (if isint?
         (if (even? (exact-round y1))
             (+inf.dd)
             (-inf.dd))
         (+inf.dd))]
    [(and (ddneginf? x1 x2) (ddnegative? y1 y2)) (values 0.0 0.0)]
    [(ddint? y1 y2)
     ;; ignore the low part
     (let*-values ([(e1 _) (ddaint y1 y2)]
                   [(mod2) (even? (exact-round e1))]
                   [(a1 a2) (ddabs x1 x2)]
                   [(a1 a2) (ddlog a1 a2)]
                   [(a1 a2) (dd* y1 y2 a1 a2)]
                   [(a1 a2) (ddexp a1 a2)])
       (if mod2
           (values a1 a2)
           (ddneg a1 a2)))]
    [else (nan.dd)]))

(define-syntax define-dd-unary-fn
  (syntax-rules ()
    [(_ [name1 name2] ...)
     (begin
       (define (name1 x1 [x2 0])
         (define a (list->f64vector (list x1 x2)))
         (define b (make-f64vector 2))
         (name2 (f64vector->cpointer a) (f64vector->cpointer b))
         (apply values (f64vector->list b))) ...)]))

(define-dd-unary-fn [ddabs c_dd_abs]
                    [ddsin c_dd_sin]
                    [ddcos c_dd_cos]
                    [ddtan c_dd_tan]
                    [ddaint c_dd_aint]
                    [ddexp c_dd_exp])
