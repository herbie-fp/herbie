#lang racket

;; Double-precision profile (accelerators only)

(require ffi/unsafe)
(require "../../plugin.rkt" "binary64.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-operator stx)
  (syntax-case stx (real)
    [(_ (op real ...) [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.f64)])
       #`(begin
          (define fl-proc
            (get-ffi-obj 'op #f (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double)
                          (λ () #f)))
           (when fl-proc
            (define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'binary64))) binary64
              [fl fl-proc] [key value] ...))))]))

(define-syntax-rule (define-1ary-libm-operator op)
  (define-libm-operator (op real)))

(define-syntax-rule (define-2ary-libm-operator op)
  (define-libm-operator (op real real)))

(define-syntax-rule (define-1ary-libm-operators op ...)
  (begin (define-1ary-libm-operator op) ...))

(define-syntax-rule (define-2ary-libm-operators op ...)
  (begin (define-2ary-libm-operator op) ...))

(define-1ary-libm-operators expm1 log1p)
(define-2ary-libm-operators atan2 hypot)
(define-libm-operator (fma real real real))
