#lang racket

;; Integration for the generic-flonum package
;; Runtime for "all" IEEE 754 floating-point formats
;; Since there are far too many formats to enumerate, they are loaded dynamically

(require generic-flonum)
(require "runtime/utils.rkt")

(define (sym-append . args)
  (define strs (map ~s args))
  (string->symbol (apply string-append strs)))

; evaluation under a rounding context
(define ((generate-context es nbits) fn)
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    (lambda args (apply fn args))))

; generator for floating-point formats
(define (register-floating-point-impl! es nbits)
  (define name (list 'float es nbits))
  (define with-context (generate-context es nbits))

  (define (register-op! name argc impl-fn #:otype [otype repr])
    (register-operator-impl! name
                             (sym-append name '.fl es '- nbits)
                             (make-list argc repr)
                             otype
                             (list (cons 'fl (with-context impl-fn)))))

  (define-syntax register-ops!
    (syntax-rules ()
      [(_ argc) (void)]
      [(_ argc [name impl-fn] rest ...)
       (begin
         (register-op! 'name argc impl-fn)
         (register-ops! argc rest ...))]
      [(_ argc [name impl-fn otype] rest ...)
       (begin
         (register-op! 'name argc impl-fn #:otype (get-representation 'otype))
         (register-ops! argc rest ...))]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (register-representation! name
                            'real
                            gfl?
                            (with-context bigfloat->gfl)
                            gfl->bigfloat
                            (with-context ordinal->gfl)
                            (with-context gfl->ordinal)
                            nbits
                            (disjoin gflinfinite? gflnan?))

  (define repr (get-representation name))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (register-ops! 0
                 [PI (const pi.gfl)]
                 [E (const (gflexp 1.gfl))]
                 [INFINITY (const +inf.gfl)]
                 [NAN (const +nan.gfl)])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (register-ops! 1
                 [neg gfl-]
                 [sqrt gflsqrt]
                 [cbrt gflcbrt]
                 [fabs gflabs]
                 [log gfllog]
                 [log2 gfllog2]
                 [log10 gfllog10]
                 [log1p gfllog1p]
                 [exp gflexp]
                 [exp2 gflexp2]
                 [expm1 gflexpm1]
                 [sin gflsin]
                 [cos gflcos]
                 [tan gfltan]
                 [asin gflatan]
                 [acos gflacos]
                 [atan gflatan]
                 [sinh gflsinh]
                 [cosh gflcosh]
                 [tanh gfltanh])

  (register-ops! 2
                 [+ gfl+]
                 [- gfl-]
                 [* gfl*]
                 [/ gfl/]
                 [pow gflexpt]
                 [atan2 gflatan2]
                 [hypot gflhypot]
                 [remainder gflremainder])

  (register-ops! 3 [fma gflfma])

  (register-ops! 2
                 [== gfl= bool]
                 [!= (negate gfl=) bool]
                 [< gfl< bool]
                 [> gfl> bool]
                 [<= gfl<= bool]
                 [>= gfl>= bool])

  (void))

(define (generate-float-format name)
  (match name
    ; we have hardware acceleration for some formats
    [(list 'float 11 64)
     (define repr (get-representation 'binary64))
     (register-representation-alias! name repr)
     repr]
    [(list 'float 8 32)
     (define repr (get-representation 'binary32))
     (register-representation-alias! name repr)
     repr]
    ; otherwise, we use software
    [(list 'float es nbits)
     (register-floating-point-impl! es nbits)
     (get-representation name)]
    ; aliases
    ['binary256
     (register-floating-point-impl! 19 256)
     (define repr (get-representation '(float 19 256)))
     (register-representation-alias! name repr)
     repr]
    ['binary128
     (register-floating-point-impl! 15 128)
     (define repr (get-representation '(float 15 128)))
     (register-representation-alias! name repr)
     repr]
    ['binary80
     (register-floating-point-impl! 15 80)
     (define repr (get-representation '(float 15 80)))
     (register-representation-alias! name repr)
     repr]
    ['pxr24
     (register-floating-point-impl! 8 24)
     (define repr (get-representation '(float 8 24)))
     (register-representation-alias! name repr)
     repr]
    ['fp24
     (register-floating-point-impl! 7 24)
     (define repr (get-representation '(float 7 24)))
     (register-representation-alias! name repr)
     repr]
    ['tf32
     (register-floating-point-impl! 8 19)
     (define repr (get-representation '(float 8 19)))
     (register-representation-alias! name repr)
     repr]
    ['bfloat16
     (register-floating-point-impl! 8 16)
     (define repr (get-representation '(float 8 16)))
     (register-representation-alias! name repr)
     repr]
    ['binary16
     (register-floating-point-impl! 5 16)
     (define repr (get-representation '(float 5 16)))
     (register-representation-alias! name repr)
     repr]
    ; otherwise we don't know
    [_ #f]))

(register-generator! generate-float-format)
