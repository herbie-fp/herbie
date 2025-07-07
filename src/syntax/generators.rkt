#lang racket

(require math/flonum
         math/bigfloat
         ffi/unsafe)

(require "../core/rival.rkt"
         "types.rkt"
         "../utils/common.rkt")

(provide correct-rounding
         make-libm
         from-libm
         from-bigfloat
         (struct-out generator))

(struct generator (gen) #:prefab)

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
;; in the system libm and binds to `id` the FFI function or `#f` if
;; the procedure cannot be found.
;; ```
;; (make-libm (<name> <itype> ... <otype))
;; ```
(define-syntax (make-libm stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'make-libm why stx sub-stx))
  (define (ctype->ffi type)
    (syntax-case type (double float integer)
      [double #'_double]
      [float #'_float]
      [integer #'_int]
      [_ (oops! "unknown type" type)]))
  (syntax-case stx ()
    [(_ (name itype ... otype))
     (begin
       (unless (identifier? #'name)
         (oops! "expected identifier" #'name))
       (with-syntax ([(itype ...) (map ctype->ffi (syntax->list #'(itype ...)))]
                     [otype (ctype->ffi #'otype)])
         #'(get-ffi-obj 'name #f (_fun itype ... -> otype) (const #f))))]))

(define (make-libm-runtime name itypes otype)
  ; Ctype matching
  (define (ctype->ffi repr)
    (match repr
      ['double _double]
      ['float _float]
      ['integer _int]
      [else (raise-syntax-error 'repr->type "unknown type" repr)]))
  (get-ffi-obj name #f (_cprocedure (map ctype->ffi itypes) (ctype->ffi otype)) (const #f)))

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
