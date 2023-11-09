#lang racket

;; libm runtime support
;; The builtin plugins prefer to use the underlying libm
;; rather than Racket's implementations

(require ffi/unsafe (for-syntax racket/match))
(require "utils.rkt" (for-syntax "utils.rkt"))

(provide define/libm
         define-binary64-impl/libm
         define-binary32-impl/libm
         define-binary64-impls/libm
         define-binary32-impls/libm)

;; Binds `id` to a libm floating-point operator
(define-syntax (define/libm stx)
  (syntax-case stx ()
    [(_ id name type argc)
     (begin
       (unless (identifier? #'id)
         (raise-syntax-error 'define/libm "expected identifier" stx #'id))
       (define-values (cname ctype) 
         (match (syntax->datum #'type)
           ['double (values (syntax->datum #'name) '_double)]
           ['float  (values (sym-append (syntax->datum #'name) 'f) '_float)]
           [_ (raise-syntax-error 'define/libm
                                  "expected either 'double or 'single"
                                  stx #'type)]))
       (with-syntax ([(args ...) (build-list (syntax->datum #'argc) (λ (_) ctype))]
                     [cname cname]
                     [ctype ctype])
         #`(define id (get-ffi-obj 'cname #f (_fun args ... -> ctype) (λ () #f)))))]))

(define-syntaxes (define-binary64-impl/libm define-binary32-impl/libm)
  (let ([make-definer
        (lambda (repr-name)
          (lambda (stx)
            (syntax-case stx (real)
              [(_ (op real ...) [key value] ...)
               (begin
                 (define name (syntax->datum #'op))
                 (define argc (length (cdr (syntax->list (cadr (syntax-e stx))))))
                 (define-values (suffix ctype)
                   (match repr-name
                     ['binary64 (values '.f64 'double)]
                     ['binary32 (values '.f32 'float)]
                     [_         (error 'go "unknown representation ~a" repr-name)]))
                 (define impl-name (sym-append name suffix))
                 (with-syntax ([(args ...) (build-list argc (λ (_) repr-name))])
                   #`(begin
                       (define/libm fl-proc op #,ctype #,argc)
                       (when fl-proc
                         (define-operator-impl (op #,impl-name args ...) #,repr-name
                           [fl fl-proc] [key value] ...)))))])))])
      (values (make-definer 'binary64)
              (make-definer 'binary32))))

(define-syntaxes (define-binary64-impls/libm define-binary32-impls/libm)
  (let ([make-multi-definer
         (lambda (definer)
           (lambda (stx)
             (syntax-case stx ()
               [(_ [ops ...] ...)
                (let loop ([ops (syntax->list #'((ops ...) ...))] [exprs '()] [arity 1])
                  (cond
                    [(null? ops)
                     (datum->syntax stx (cons #'begin exprs))]
                    [else
                     (define ops-at-arity (car ops))
                     (define args (build-list arity (λ (_) #'real)))
                     (loop (cdr ops)
                           (for/fold ([exprs exprs]) ([op (syntax->list ops-at-arity)])
                             (cons
                               (datum->syntax ops-at-arity
                                 (list definer (cons op args)))
                               exprs))
                           (+ arity 1))]))])))])

    (values (make-multi-definer 'define-binary64-impl/libm)
            (make-multi-definer 'define-binary32-impl/libm))))
