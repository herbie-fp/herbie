#lang racket

;; libm runtime support
;; The builtin plugins prefer to use the underlying libm rather
;; than Racket's implementations

(require ffi/unsafe)
(require "utils.rkt")

(provide define-libm
         define-libm-impl)

;; Looks up a function `name` with type signature `itype -> ... -> otype`
;; in the system libm and binds to `id` the FFI function or `#f` if
;; the procedure cannot be found.
;; ```
;; (define-libm <id> (<name> <itype> ... <otype))
;; ```
(define-syntax (define-libm stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'define-libm why stx sub-stx))
  (define (ctype->ffi type)
    (syntax-case type (double float integer)
      [double #'_double]
      [float #'_float]
      [integer #'_int]
      [_ (oops! "unknown type" type)]))
  (syntax-case stx ()
    [(_ id (name itype ... otype))
     (begin
       (unless (identifier? #'id)
         (oops! "expected identifier" #'id))
       (unless (identifier? #'name)
         (oops! "expected identifier" #'name))
       (with-syntax ([(itype ...) (map ctype->ffi (syntax->list #'(itype ...)))]
                     [otype (ctype->ffi #'otype)])
         #'(define id (get-ffi-obj 'name #f (_fun itype ... -> otype) (const #f)))))]))

;; Define a Herbie operator implementation whose underlying
;; procedure is a libm function.
;; ```
;; (define-libm-impl <libm-name> (<op> <impl> <itype> ...) <otype> <attrib> ...)
;; ```
(define-syntax (define-libm-impl stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'define-libm-impl why stx sub-stx))
  (define (repr->type repr)
    (syntax-case repr (binary64 binary32 integer)
      [binary64 #'double]
      [binary32 #'float]
      [integer #'integer]
      [_ (oops! "unknown type" repr)]))
  (syntax-case stx ()
    [(_ cname (op name itype ...) otype fields ...)
     (let ([op #'op]
           [name #'name]
           [cname #'cname]
           [itypes (syntax->list #'(itype ...))])
       (unless (identifier? op)
         (oops! "expected identifier" op))
       (unless (identifier? name)
         (oops! "expected identifier" name))
       (unless (identifier? cname)
         (oops! "expected identifier" cname))
       (with-syntax ([op op]
                     [name name]
                     [cname cname]
                     [(var ...) (build-list (length itypes) (lambda (i) (string->symbol (format "x~a" i))))]
                     [(itype ...) itypes]
                     [(citype ...) (map repr->type itypes)]
                     [cotype (repr->type #'otype)])
         #'(begin
             (define-libm proc (cname citype ... cotype))
             (when proc
               (define-operator-impl (name [var : itype] ...)
                                     otype
                                     #:spec (op var ...)
                                     #:fl proc
                                     fields ...)))))]))
