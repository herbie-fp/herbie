#lang racket

(require math/flonum
         math/bigfloat
         "../core/rival.rkt"
         "types.rkt"
         "../platforms/runtime/libm.rkt"
         "../utils/common.rkt")

(provide correct-rounding
         from-libm
         from-mpfr
         generator?)

(define (generator? obj)
  (equal? (object-name obj) 'generator))

; ----------------------- RIVAL GENERATOR ---------------------------

(define (correct-rounding)
  (define (generate-correctly-rounded-function spec ctx)
    (define compiler (make-real-compiler (list spec) (list ctx)))
    (define fail ((representation-bf->repr (context-repr ctx)) +nan.bf))
    (lambda pt
      (define-values (_ exs) (real-apply compiler (list->vector pt)))
      (if exs
          (first exs)
          fail)))
  (procedure-rename generate-correctly-rounded-function 'generator))

; ----------------------- LIBM GENERATOR ----------------------------

;; Reprs -> ctype
(define (repr->ctype repr)
  (match (representation-name repr)
    ['binary64 'double]
    ['binary32 'float]
    ['integer 'integer]
    [_ (error 'repr->ctype "Unresolved c-type for representation ~a" (representation-name repr))]))

(define (from-libm name)
  (define (generate-libm-function spec ctx)
    (let ([itypes (map repr->ctype (context-var-reprs ctx))]
          [otype (repr->ctype (context-repr ctx))])
      (define fl (make-libm-runtime name itypes otype))
      (unless fl
        (error 'libm-generator "Could not find libm implementation of `~a ~a ~a`" otype name itypes))
      fl))
  (procedure-rename generate-libm-function 'generator))

; ----------------------- MPFR GENERATOR ----------------------------

(define (repr->bf x type)
  ((representation-repr->bf type) x))

(define (bf->repr x type)
  ((representation-bf->repr type) x))

(define (from-mpfr name)
  (define (generate-mpfr-function spec ctx)
    (let* ([itypes (context-var-reprs ctx)]
           [otype (context-repr ctx)]
           [op (dynamic-require '(lib "math/bigfloat") name)]
           [working-precision (representation-total-bits otype)])
      (lambda pt
        (define pt* (map repr->bf pt itypes))
        (define bf-out
          (parameterize ([bf-precision working-precision])
            (apply op pt*)))
        (bf->repr bf-out otype))))
  (procedure-rename generate-mpfr-function 'generator))
