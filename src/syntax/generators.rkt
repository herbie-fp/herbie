#lang racket

(require math/flonum
         math/bigfloat
         ffi/unsafe)

(require "../core/rival.rkt"
         "types.rkt"
         "../utils/common.rkt")

(provide from-rival
         from-ffi
         from-libm
         from-bigfloat
         define-generator
         (struct-out generator))

(struct generator (gen))

(define-syntax-rule (define-generator ((name args ...) spec ctx)
                      body ...)
  (define (name args ...)
    (generator (lambda (spec ctx)
                 body ...))))

; ----------------------- RIVAL GENERATOR ---------------------------

(define-generator ((from-rival) spec ctx)
  (define compiler (make-real-compiler (list spec) (list ctx)))
  (define fail ((representation-bf->repr (context-repr ctx)) +nan.bf))
  (lambda pt
    (define-values (_ exs) (real-apply compiler (list->vector pt)))
    (if exs
        (first exs)
        fail)))

; ----------------------- FFI GENERATOR -----------------------------

;; Looks up a function `name` with type signature `itype -> ... -> otype`
;; in the given FFI library and returns the function or `#f` if it
;; cannot be found.
;; ```
;; (make-ffi <lib> (<name> <itype> ... <otype>))
;; ```
(define (make-ffi lib name itypes otype)
  ; Repr matching
  (define (repr->ffi repr)
    (match (representation-name repr)
      ['binary64 _double]
      ['binary32 _float]
      ['integer _int]
      [else (raise-syntax-error 'repr->type "unknown type" repr)]))
  (get-ffi-obj name lib (_cprocedure (map repr->ffi itypes) (repr->ffi otype)) (const #f)))

(define-generator ((from-ffi lib name) spec ctx)
  (let ([itypes (context-var-reprs ctx)]
        [otype (context-repr ctx)])
    (define fl (make-ffi lib name itypes otype))
    (unless fl
      (error 'ffi-generator "Could not find FFI implementation of `~a ~a ~a`" otype name itypes))
    fl))

(define libm-lib (ffi-lib #f))
(define (from-libm name)
  (from-ffi libm-lib name))

; ----------------------- BIGFLOAT GENERATOR ------------------------

(define (repr->bf x type)
  ((representation-repr->bf type) x))

(define (bf->repr x type)
  ((representation-bf->repr type) x))

(define-generator ((from-bigfloat name) spec ctx)
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
