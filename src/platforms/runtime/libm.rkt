#lang racket

;; libm runtime support
;; The builtin plugins prefer to use the underlying libm rather
;; than Racket's implementations

(require ffi/unsafe)

(provide make-libm
         make-libm-runtime
         libm-optimize)

; Libm's accelerators
(define (libm-optimize spec)
  (match spec
    [`(- 1 (erf ,x)) `(erfc ,x)]
    [`(- (exp ,x) 1) `(expm1 ,x)]
    [`(log (+ 1 ,x)) `(log1p ,x)]
    [`(sqrt (+ (* ,x ,x) (* ,y ,y))) `(hypot ,x ,y)]
    [`(+ (* ,x ,y) ,z) `(fma ,x ,y ,z)]
    [_ spec]))

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
