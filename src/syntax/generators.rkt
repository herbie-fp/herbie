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

;; MPFR must use at least 2 bits of precision
(define minimum-mpfr-precision 2)

;; Reprs -> precision
(define (repr->precision repr)
  (max minimum-mpfr-precision (representation-total-bits repr)))

(define (from-mpfr name)
  (define (generate-mpfr-function spec ctx)
    (let ([iprecs (map repr->precision (context-var-reprs ctx))]
          [oprec (repr->precision (context-repr ctx))]
          [op (dynamic-require '(lib "math/bigfloat") name)])
      (unless (procedure-arity-includes? op (length iprecs) #t)
        (error 'from-mpfr
               "MPFR procedure `~a` accepts ~a arguments, but ~a is provided"
               name
               (procedure-arity op)
               (length iprecs)))
      (lambda pt
        (define pt*
          (for/list ([p (in-list pt)]
                     [iprec (in-list iprecs)])
            (parameterize ([bf-precision iprec])
              (bf p))))
        (bigfloat->flonum (parameterize ([bf-precision oprec])
                            (apply op pt*))))))
  (procedure-rename generate-mpfr-function 'generator))
