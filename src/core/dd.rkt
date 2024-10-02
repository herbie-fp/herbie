#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         rackunit)

(provide (all-defined-out))

(define qd-lib (ffi-lib "./QD/src/.libs/libqd"))

(define log2-hi 6.931471805599452862e-01)
(define log2-lo 2.319046813846299558e-17)
(define dde1 2.718281828459045091e+00)
(define dde2 1.445646891729250158e-16)
(define ddpi1 3.141592653589793116e+00)
(define ddpi2 1.224646799147353207e-16)

(define-ffi-definer define-qd qd-lib)

(define-syntax define-dd-binary
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-qd name
         (_fun _pointer _pointer _pointer -> _void))
       ...)]))

(define-syntax define-dd-unary
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-qd name
         (_fun _pointer _pointer -> _void))
       ...)]))

(define-qd c_dd_nroot
  (_fun _pointer _int _pointer -> _void))

; Example usage:
(define-dd-binary
  c_dd_add
  c_dd_sub
  c_dd_mul
  c_dd_div)

(define-dd-unary
  c_dd_sqrt
  c_dd_sqr
  c_dd_abs
  c_dd_exp
  c_dd_log
  c_dd_log10
  c_dd_sin
  c_dd_cos
  c_dd_tan
  c_dd_aint)

(define-syntax define-dd-binary-fn
  (syntax-rules ()
    [(_ [name1 name2] ...)
     (begin
       (define (name1 x1 x2 y1 [y2 0])
         (define a (list->f64vector (list x1 x2)))
         (define b (list->f64vector (list y1 y2)))
         (define c (make-f64vector 2))
         (name2 (f64vector->cpointer a)
                (f64vector->cpointer b)
                (f64vector->cpointer c))
         (apply values (f64vector->list c)))
       ...)]))

(define-syntax define-dd-unary-fn
  (syntax-rules ()
    [(_ [name1 name2] ...)
     (begin
       (define (name1 x1 [x2 0])
         (define a (list->f64vector (list x1 x2)))
         (define b (make-f64vector 2))
         (name2 (f64vector->cpointer a)
                (f64vector->cpointer b))
         (apply values (f64vector->list b)))
       ...)]))

(define (ddnroot x1 x2 y)
  (define a (list->f64vector (list x1 x2)))
  (define b (make-f64vector 2))
  (c_dd_nroot (f64vector->cpointer a)
              y
              (f64vector->cpointer b))
  (apply values (f64vector->list b)))

(define (ddlog x1 [x2 0.0])
  (if (ddzero? x1 x2)
      (values -inf.0 0.0)
      (let* ([a (list->f64vector (list x1 x2))]
             [b (make-f64vector 2)])
        (begin (c_dd_log (f64vector->cpointer a)
                         (f64vector->cpointer b))
               (apply values (f64vector->list b))))))

(define (dd+ x1 x2 y1 [y2 0.0])
  (cond
    [(and (ddfinite? x1 x2) (dd= y1 y2 +inf.0))
     (values +inf.0 0.0)]
    [(and (ddfinite? x1 x2) (dd= y1 y2 -inf.0))
     (values -inf.0 0.0)]
    [(and (ddfinite? y1 y2) (dd= x1 x2 +inf.0))
     (values +inf.0 0.0)]
    [(and (ddfinite? y1 y2) (dd= x1 x2 -inf.0))
     (values -inf.0 0.0)]
    [(and (dd= x1 x2 +inf.0) (dd= y1 y2 +inf.0))
     (values +inf.0 0.0)]
    [(and (dd= x1 x2 -inf.0) (dd= y1 y2 -inf.0))
     (values -inf.0 0.0)]
    [(and (dd= x1 x2 +inf.0) (dd= y1 y2 -inf.0))
     (values +nan.0 0.0)]
    [(and (dd= x1 x2 -inf.0) (dd= y1 y2 +inf.0))
     (values +nan.0 0.0)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_add (f64vector->cpointer a)
               (f64vector->cpointer b)
               (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd- x1 x2 y1 [y2 0.0])
  (cond
    [(and (ddfinite? x1 x2) (dd= y1 y2 +inf.0))
     (values -inf.0 0.0)]
    [(and (ddfinite? x1 x2) (dd= y1 y2 -inf.0))
     (values +inf.0 0.0)]
    [(and (ddfinite? y1 y2) (dd= x1 x2 +inf.0))
     (values +inf.0 0.0)]
    [(and (ddfinite? y1 y2) (dd= x1 x2 -inf.0))
     (values -inf.0 0.0)]
    [(and (dd= x1 x2 +inf.0) (dd= y1 y2 +inf.0))
     (values +nan.0 0.0)]
    [(and (dd= x1 x2 -inf.0) (dd= y1 y2 -inf.0))
     (values +nan.0 0.0)]
    [(and (dd= x1 x2 +inf.0) (dd= y1 y2 -inf.0))
     (values +inf.0 0.0)]
    [(and (dd= x1 x2 -inf.0) (dd= y1 y2 +inf.0))
     (values -inf.0 0.0)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_sub (f64vector->cpointer a)
               (f64vector->cpointer b)
               (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd* x1 x2 y1 [y2 0.0])
  (cond
    [(and (ddzero? x1 x2) (ddinfinite? y1 y2))
     (values +nan.0 0.0)]
    [(and (ddzero? y1 y2) (ddinfinite? x1 x2))
     (values +nan.0 0.0)]
    [(and (dd+inf? x1 x2) (dd+inf? x1 x2))
     (values +inf.0 0.0)]
    [(and (dd-inf? x1 x2) (dd+inf? x1 x2))
     (values -inf.0 0.0)]
    [(and (dd+inf? x1 x2) (dd-inf? x1 x2))
     (values -inf.0 0.0)]
    [(and (dd-inf? x1 x2) (dd-inf? x1 x2))
     (values +inf.0 0.0)]
    [(and (ddpositive? x1 x2) (dd+inf? y1 y2))
     (values +inf.0 0.0)]
    [(and (ddpositive? x1 x2) (dd-inf? y1 y2))
     (values -inf.0 0.0)]
    [(and (ddnegative? x1 x2) (dd+inf? y1 y2))
     (values -inf.0 0.0)]
    [(and (ddnegative? x1 x2) (dd-inf? y1 y2))
     (values +inf.0 0.0)]
    [(and (ddpositive? y1 y2) (dd+inf? x1 x2))
     (values +inf.0 0.0)]
    [(and (ddpositive? y1 y2) (dd-inf? x1 x2))
     (values -inf.0 0.0)]
    [(and (ddnegative? y1 y2) (dd+inf? x1 x2))
     (values -inf.0 0.0)]
    [(and (ddnegative? y1 y2) (dd-inf? x1 x2))
     (values +inf.0 0.0)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_mul (f64vector->cpointer a)
               (f64vector->cpointer b)
               (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd/ x1 x2 y1 [y2 0])
  (cond
    [(and (ddzero? x1 x2) (ddinfinite? y1 y2))
     (values 0.0 0.0)]
    [(and (ddinfinite? x1 x2) (ddzero? y1 y2))
     (values x1 x2)]
    [(and (ddinfinite? x1 x2) (ddfinite? y1 y2))
     (values x1 x2)]
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2))
     (values 0.0 0.0)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_div (f64vector->cpointer a)
               (f64vector->cpointer b)
               (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddsqrt x1 [x2 0])
  (cond
    [(dd+inf? x1 x2)(values x1 x2)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define c (make-f64vector 2))
     (c_dd_sqrt (f64vector->cpointer a)
                (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddexp x1 [x2 0])
  (define a (list->f64vector (list x1 x2)))
  (define c (make-f64vector 2))
  (c_dd_exp (f64vector->cpointer a)
             (f64vector->cpointer c))
  (apply values (f64vector->list c)))

(define-dd-unary-fn
  [ddsqr c_dd_sqr]
  [ddabs c_dd_abs]
  [ddlog10 c_dd_log10]
  [ddsin c_dd_sin]
  [ddcos c_dd_cos]
  [ddtan c_dd_tan]
  [ddaint c_dd_aint])

(define (ddint? x1 [x2 0])
  (let*-values
    ([(a1 a2) (ddaint x1 x2)])
    (dd= x1 x2 a1 a2)))

(define (ddneg x1 [x2 0])
  (dd- 0.0 0.0 x1 x2))

(define (ddlog2 x1 [x2 0])
  (if (ddzero? x1 x2)
      (values -inf.0 0.0)
      (let*-values
          ([(a1 a2) (ddlog x1 x2)])
        (dd/ a1 a2 log2-hi log2-lo))))

(define (ddexp2 x1 [x2 0])
  (let*-values
       ([(e1 e2) (dd* x1 x2 log2-hi log2-lo)])
    (ddexp e1 e2)))

(define (ddexpt x1 x2 y1 [y2 0.0])
  (cond
    [(ddzero? x1 x2) (values 1.0 0.0)]
    [(dd> x1 x2 0.0 0.0)
     (let*-values
         ([(a1 a2) (ddlog x1 x2)]
          [(a1 a2) (dd* y1 y2 a1 a2)])
       (ddexp a1 a2))]
    [(nan? y1) (values +nan.0 0.0)]
    [(and (infinite? y1) (positive? y1))
     (values +inf.0 0.0)]
    [(and (infinite? y1) (negative? y1))
     (values 0.0 0.0)]
    [(ddint? y1 y2)
     ;; ignore the low part
     (let*-values
         ([(e1 _) (ddaint y1 y2)]
          [(mod2) (even? (exact-round e1))]
          [(a1 a2) (ddabs x1 x2)]
          [(a1 a2) (ddlog a1 a2)]
          [(a1 a2) (dd* y1 y2 a1 a2)]
          [(a1 a2) (ddexp a1 a2)])
       (if mod2
           (values a1 a2)
           (ddneg a1 a2)))]
    [else (values +nan.0 0.0)]))

(define (ddzero? x1 [x2 0])
  (dd= x1 x2 0))

(define (ddinfinite? x1 [x2 0])
  (infinite? x1))

(define (dd+inf? x1 [x2 0])
  (dd= x1 x2 +inf.0))

(define (dd-inf? x1 [x2 0])
  (dd= x1 x2 -inf.0))

(define (ddfinite? x1 [x2 0])
  (and (not (ddinfinite? x1 x2)) (not (ddnan? x1 x2))))

(define (ddpositive? x1 [x2 0])
  (dd> x1 x2 0.0))

(define (ddnegative? x1 [x2 0])
  (dd< x1 x2 0.0))

(define (ddnan? x1 [x2 0])
  (nan? x1))

(define (ddpi)
  (values 3.141592653589793116e+00 1.224646799147353207e-16))

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