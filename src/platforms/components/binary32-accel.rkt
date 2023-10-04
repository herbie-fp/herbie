#lang racket

;; Single-precision platform (math only)

(require ffi/unsafe)
(require "../../plugin.rkt" "binary32.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-operator stx)
  (syntax-case stx (real)
    [(_ (op real ...) [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.f32)]
            [cname (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) 'f)])
       #`(begin
           (define fl-proc
            (get-ffi-obj '#,cname #f (_fun #,@(build-list num-args (λ (_) #'_float)) -> _float)
                         (λ () #f)))
           (when fl-proc
            (define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'binary32))) binary32
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
