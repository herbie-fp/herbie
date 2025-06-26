#lang racket

;; libm runtime support
;; The builtin plugins prefer to use the underlying libm rather
;; than Racket's implementations

(require ffi/unsafe
         "../../syntax/syntax.rkt")

(provide make-libm
         make-libm-impl
         make-libm-impls/binary32
         make-libm-impls/binary64)

;; Looks up a function `name` with type signature `itype -> ... -> otype`
;; in the system libm and binds to `id` the FFI function or `#f` if
;; the procedure cannot be found.
;; ```
;; (define-libm <id> (<name> <itype> ... <otype))
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
       (unless (identifier? #'id)
         (oops! "expected identifier" #'id))
       (unless (identifier? #'name)
         (oops! "expected identifier" #'name))
       (with-syntax ([(itype ...) (map ctype->ffi (syntax->list #'(itype ...)))]
                     [otype (ctype->ffi #'otype)])
         #'(get-ffi-obj 'name #f (_fun itype ... -> otype) (const #f))))]))

;; Define a Herbie operator implementation whose underlying
;; procedure is a libm function.
;; ```
;; (define-libm-impl <libm-name> (<op> <impl> <itype> ...) <otype> <attrib> ...)
;; ```
(define-syntax (make-libm-impl stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'make-libm-impl why stx sub-stx))

  (define (repr->type repr)
    (let ([sym (syntax-e repr)])
      (cond
        [(eq? sym 'binary64) #'double]
        [(eq? sym 'binary32) #'float]
        [(eq? sym 'integer) #'integer]
        [else (oops! "unknown type" repr)])))

  (syntax-case stx ()
    [(_ cname (op name itype ...) otype cost fields ...)
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
                     [(var ...) (build-list (length itypes)
                                            (lambda (i) (string->symbol (format "x~a" i))))]
                     [(itype ...) itypes]
                     [(citype ...) (map repr->type itypes)]
                     [cotype (repr->type #'otype)])
         #'(let* ([proc (make-libm (cname citype ... cotype))]
                  [impl (make-operator-impl (name [var : itype] ...)
                                            otype
                                            #:spec (op var ...)
                                            #:fpcore (! :precision otype (op var ...))
                                            #:fl proc
                                            #:cost cost
                                            fields ...)])
             impl)))]))

; Define binary32 implementations with libm's fl

; Same as
; (define fabsf.libm (make-libm fabsf.libm (fabsf float float)))
; (define fabs.f32
;   (make-operator-impl (fabs.f32 [x : binary32]) binary32 #:spec (fabs x) #:fl fabsf.libm #:cost 1))
; (platform-register-implementation! herbie20-platform fabs.f32)

(define-syntax (make-libm-impl/binary32 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype cost attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f32" (syntax->datum #'op)))]
                   [cname (string->symbol (format "~af" (syntax->datum #'op)))])
       #'(make-libm-impl cname (op impl itype ...) otype cost attrib ...))]))

(define-syntax-rule (make-libm-impls/binary32* (itype ... otype) [name cost] ...)
  (list (make-libm-impl/binary32 name (itype ...) otype cost) ...))

(define-syntax-rule (make-libm-impls/binary32 [(itype ... otype) ([name cost] ...)] ...)
  (apply append (list (make-libm-impls/binary32* (itype ... otype) [name cost] ...) ...)))

; Define binary64 implementations with libm's fl

(define-syntax (make-libm-impl/binary64 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype cost attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f64" (syntax->datum #'op)))])
       #'(make-libm-impl op (op impl itype ...) otype cost attrib ...))]))

(define-syntax-rule (make-libm-impls/binary64* (itype ... otype) [name cost] ...)
  (list (make-libm-impl/binary64 name (itype ...) otype cost) ...))

(define-syntax-rule (make-libm-impls/binary64 [(itype ... otype) ([name cost] ...)] ...)
  (apply append (list (make-libm-impls/binary64* (itype ... otype) [name cost] ...) ...)))
