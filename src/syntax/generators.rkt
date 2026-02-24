#lang racket

(require math/flonum
         math/bigfloat
         ffi/unsafe)

(require "rival.rkt"
         "../config.rkt"
         "types.rkt"
         "batch.rkt")

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

(define/reset caches
              '()
              (lambda ()
                (for ([cache (caches)])
                  (hash-clear! cache))))

(define-generator ((from-rival #:cache? [cache? #t]) spec ctx)
  (define-values (batch brfs) (progs->batch (list spec)))
  (define compiler (make-real-compiler batch brfs (list ctx)))
  (define fail ((representation-bf->repr (context-repr ctx)) +nan.bf))
  (define (compute . pt)
    (define-values (_ exs) (real-apply compiler (list->vector pt)))
    (if exs
        (first exs)
        fail))
  (cond
    [cache?
     (define cache (make-hash))
     (caches (cons cache (caches)))
     (lambda pt (hash-ref! cache pt (lambda () (apply compute pt))))]
    [else compute]))

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
    (or (make-ffi lib name itypes otype)
        (error 'ffi-generator "Could not find FFI implementation of `~a ~a ~a`" otype name itypes))))

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
