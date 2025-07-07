#lang racket

(require math/flonum
         math/bigfloat
         ffi/unsafe)

(require "../core/rival.rkt"
         "types.rkt"
         "../utils/common.rkt")

(provide correct-rounding
         from-libm
         from-bigfloat
         (struct-out generator))

(struct generator (gen))

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
  (generator generate-correctly-rounded-function))

; ----------------------- LIBM GENERATOR ----------------------------

;; Looks up a function `name` with type signature `itype -> ... -> otype`
;; in the system libm and returns the FFI function or `#f` if
;; the procedure cannot be found.
;; ```
;; (make-libm (<name> <itype> ... <otype))
;; ```
(define (make-libm name itypes otype)
  ; Repr matching
  (define (repr->ffi repr)
    (match (representation-name repr)
      ['binary64 _double]
      ['binary32 _float]
      ['integer _int]
      [else (raise-syntax-error 'repr->type "unknown type" repr)]))
  (get-ffi-obj name #f (_cprocedure (map repr->ffi itypes) (repr->ffi otype)) (const #f)))

(define (from-libm name)
  (define (generate-libm-function spec ctx)
    (let ([itypes (context-var-reprs ctx)]
          [otype (context-repr ctx)])
      (define fl (make-libm name itypes otype))
      (unless fl
        (error 'libm-generator "Could not find libm implementation of `~a ~a ~a`" otype name itypes))
      fl))
  (generator generate-libm-function))

; ----------------------- BIGFLOAT GENERATOR ------------------------

(define (repr->bf x type)
  ((representation-repr->bf type) x))

(define (bf->repr x type)
  ((representation-bf->repr type) x))

(define (from-bigfloat name)
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
  (generator generate-mpfr-function))
