#lang racket

(require math/base
         math/flonum
         racket/struct
         "../syntax/syntax.rkt"
         "dd.rkt")

(provide (all-defined-out))

(define (xnor a b)
  (not (xor a b)))

(struct logfloat (r1 r2 s e1 e2)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lambda (_) 'ddlf)
                                     (lambda (obj)
                                       (list (logfloat-r1 obj)
                                             (logfloat-r2 obj)
                                             (logfloat-s obj)
                                             (logfloat-e1 obj)
                                             (logfloat-e2 obj)))))])

(define (lf x1 [x2 0.0])
  (let*-values ([(e1 e2) (ddabs x1 x2)]
                [(e1 e2) (ddlog2 e1 e2)])
    (logfloat x1 x2 (dd>= x1 x2 0.0 0.0) e1 e2)))

(define +max.lf (logfloat +max.0 0.0 #true 1024.0 0.0))
(define +min.lf (logfloat +min.0 0.0 #true -1074.0 0.0))

(define (lf-normalize x)
  (match-define (logfloat x1 x2 s e1 e2) x)
  (define-values (l1 l2) (ddexp2 x1 x2))
  (define-values (n1 n2) (ddneg l1 l2))
  (if (or (ddnan? x1 x2) (ddzero? x1 x2) (ddinfinite? x1 x2))
      (if s
          (logfloat l1 l2 s e1 e2)
          (logfloat n1 n2 s e1 e2))
      x))

(define (lfzero? x)
  (match-define (logfloat x1 x2 _ e1 e2) x)
  (and (ddzero? x1 x2) (ddinfinite? e1 e2)))

(define (lf= x y [flag #t])
  (match-define (logfloat x1 x2 xs ex1 ex2) x)
  (match-define (logfloat y1 y2 ys ey1 ey2) y)
  (if flag
      (and (eq? xs ys) (dd= ex1 ex2 ey1 ey2))
      (dd= x1 x2 y1 y2)))

(define (lf> x y [flag #t])
  (match-define (logfloat x1 x2 xs ex1 ex2) x)
  (match-define (logfloat y1 y2 ys ey1 ey2) y)
  (if flag
      (cond
        ; Both +ve
        [(and xs ys) (dd> ex1 ex2 ey1 ey2)]

        ; Both -ve
        [(nor xs ys) (dd> ey1 ey2 ex1 ex2)]

        [else xs])
      (dd> x1 x2 y1 y2)))

(define (lf< x y [flag #t])
  (lf> y x flag))

(define (lf>= x y [flag #t])
  (or (lf> x y flag) (lf= x y flag)))

(define (lf<= x y [flag #t])
  (lf>= y x [flag #t]))

(define (lfmax x y [flag #t])
  (if (lf>= x y flag) x y))

(define (lfmin x y [flag #t])
  (if (lf<= x y flag) x y))

(define (lfabs x)
  (match-define (logfloat x1 x2 _ ex1 ex2) x)
  (let*-values ([(x1 x2) (ddabs x1 x2)])
    (logfloat x1 x2 #true ex1 ex2)))

;; QUESTION: is inf inf _ inf 0 an overflow
(define (lfoverflow? x)
  (lf> (lfabs x) +max.lf))

;; QUESTION: is 0 0 _ -inf 0 an underflow
(define (lfunderflow? x)
  (lf> (lfabs x) +min.lf))

(define (lfnan? x)
  (match-define (logfloat x1 x2 _ ex1 ex2) x)
  (when (and (not (ddnan? x1 x2)) (ddnan? ex1 ex2))
    (eprintf "[ERROR::lfnan?] ~a\n" x))
  (and (ddnan? x1 x2) (ddnan? ex1 ex2)))
