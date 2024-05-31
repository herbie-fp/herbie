#lang racket

;; Builtin double-precision plugin (:precision binary64)

(require math/flonum
         math/bigfloat
         ffi/unsafe
         fpbench)

(require "runtime/utils.rkt"
         "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (binary64 real flonum?)
  bigfloat->flonum
  bf
  (shift 63 ordinal->flonum)
  (unshift 63 flonum->ordinal)
  64
  (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary64
  [PI PI.f64 pi]
  [E E.f64 (exp 1.0)]
  [INFINITY INFINITY.f64 +inf.0]
  [NAN NAN.f64 +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-impl/binary64 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype [key value] ...)
     (with-syntax ([impl (string->symbol (format "~a.f64" (syntax->datum #'op)))])
       #'(define-libm-impl op (op impl itype ...) otype [key value] ...))]))

(define-syntax-rule (define-libm-impls/binary64* (itype ... otype) name ...)
  (begin (define-libm-impl/binary64 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary64 [(itype ... otype) (name ...)] ...)
  (begin (define-libm-impls/binary64* (itype ... otype) name ...) ...))

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-libm-impls/binary64
  [(binary64 binary64)
   (acos acosh asin asinh atan atanh cbrt ceil cos cosh erf exp exp2
    fabs floor lgamma log log10 log2 logb rint round sin sinh sqrt
    tan tanh tgamma trunc)]
  [(binary64 binary64 binary64)
   (atan2 copysign fdim fmax fmin fmod pow remainder)])

(define-comparator-impls binary64
  [== ==.f64 =]
  [!= !=.f64 (negate =)]
  [< <.f64 <]
  [> >.f64 >]
  [<= <=.f64 <=]
  [>= >=.f64 >=])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; accelerators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libm expm1.f64 (expm1 double double))
(when expm1.f64
  (define-accelerator-impl expm1 expm1.f64 (binary64) binary64 expm1.f64))

(define-libm log1p.f64 (log1p double double))
(when log1p.f64
  (define-accelerator-impl log1p log1p.f64 (binary64) binary64 log1p.f64))

(define-libm hypot.f64 (hypot double double double))
(when hypot.f64
  (define-accelerator-impl hypot hypot.f64 (binary64 binary64) binary64 hypot.f64))

(define-libm fma.f64 (fma double double double double))
(when fma.f64
  (define-accelerator-impl fma fma.f64 (binary64 binary64 binary64) binary64 fma.f64))

(define-libm erfc.f64 (erfc double double))
(when erfc.f64
  (define-accelerator-impl erfc erfc.f64 (binary64) binary64 erfc.f64))

(define-accelerator (recip real) real (λ (x) (/ 1 x)))
(define-accelerator (rsqrt real) real (λ (x) (/ 1 (sqrt x))))
      
;; (define-accelerator (fmsub real real real) real (lambda (x y z) (- (* x y) z)))
;; (define-accelerator (fnmadd real real real) real (lambda (x y z) (+ (neg (* x y)) z)))
;; (define-accelerator (fnmsub real real real) real (lambda (x y z) (- (neg (* x y)) z)))

;; (define-accelerator-impl fmsub fmsub.f64 (binary64 binary64 binary64) binary64)
;; (define-accelerator-impl fnmadd fnmadd.f64 (binary64 binary64 binary64) binary64)
;; (define-accelerator-impl fnmsub fnmsub.f64 (binary64 binary64 binary64) binary64)

;; (define-accelerator (vdt-exp real) real (lambda (x) (exp x)))

;; (define-operator-impl (recip recip.f64 binary64) binary64
  ;; [fl (λ (x)
        ;; (parameterize ([bf-precision 12])
          ;; (bigfloat->flonum (bf/ 1.bf (bf x)))))])
;; 
;; (define-operator-impl (rsqrt rsqrt.f64 binary64) binary64
  ;; [fl (λ (x)
        ;; (parameterize ([bf-precision 12])
          ;; (bigfloat->flonum (bf/ 1.bf (bfsqrt (bf x))))))])

(define-ruleset* reciprocal (arithmetic simplify)
  #:type ([a real])
  [add-recip     (/ 1 a)        (recip a)]
  [remove-recip  (recip a)      (/ 1 a)]
  [add-rsqrt     (/ 1 (sqrt a)) (rsqrt a)]
  [remove-rsqrt  (rsqrt a)      (/ 1 (sqrt a))])

;; (define-accelerator (fast-exp real) real (lambda (x) (exp x)))
;; (define-accelerator (fast-sin real) real (lambda (x) (sin x)))
;; (define-accelerator (fast-cos real) real (lambda (x) (cos x)))
;; (define-accelerator (fast-tan real) real (lambda (x) (tan x)))
;; (define-accelerator (fast-tanh real) real (lambda (x) (tanh x)))
;; (define-accelerator (fast-log real) real (lambda (x) (log x)))
;; (define-accelerator (fast-asin real) real (lambda (x) (asin x)))
;; (define-accelerator (fast-acos real) real (lambda (x) (acos x)))
;; (define-accelerator (fast-atan real) real (lambda (x) (atan x)))
;; (define-accelerator (fast-isqrt real) real (lambda (x) (/ 1 (sqrt x))))


;; (define libvdt "libvdt")

;; (set-c-header!
;;  (lambda (previous)
;;    (string-join (previous) "#include <vdtMath.h>\n")))

;; (set-unknown->c!
;;  (lambda (previous)
;;    (lambda (context operation arguments)
;;      (previous
;;       context
;;       (match operation
;;         ['fast-exp 'exp]
;;         ['fast-sin 'sin]
;;         ['fast-cos 'cos]
;;         ['fast-tan 'tan]
;;         ['fast-tanh 'tanh]
;;         ['fast-log 'log]
;;         ['fast-asin 'asin]
;;         ['fast-acos 'acos]
;;         ['fast-atan 'atan])
;;       arguments))))

;; (define-accelerator-impl fast-exp fast-exp.f64 (binary64) binary64 (get-ffi-obj "exp" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-sin fast-sin.f64 (binary64) binary64 (get-ffi-obj "sin" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-cos fast-cos.f64 (binary64) binary64 (get-ffi-obj "cos" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-tan fast-tan.f64 (binary64) binary64 (get-ffi-obj "tan" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-tanh fast-tanh.f64 (binary64) binary64 (get-ffi-obj "tanh" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-log fast-log.f64 (binary64) binary64 (get-ffi-obj "log" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-asin fast-asin.f64 (binary64) binary64 (get-ffi-obj "asin" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-acos fast-acos.f64 (binary64) binary64 (get-ffi-obj "acos" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-atan fast-atan.f64 (binary64) binary64 (get-ffi-obj "atan" libvdt (_fun _double -> _double)))

;; (define-accelerator-impl fast-exp fast-exp.f32 (binary32) binary32 (get-ffi-obj "expf" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-sin fast-sin.f32 (binary32) binary32 (get-ffi-obj "sinf" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-cos fast-cos.f32 (binary32) binary32 (get-ffi-obj "cosf" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-tan fast-tan.f32 (binary32) binary32 (get-ffi-obj "tanf" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-tanh fast-tanh.f32 (binary32) binary32 (get-ffi-obj "tanhf" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-log fast-log.f32 (binary32) binary32 (get-ffi-obj "logf" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-asin fast-asin.f32 (binary32) binary32 (get-ffi-obj "asinf" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-acos fast-acos.f32 (binary32) binary32 (get-ffi-obj "acosf" libvdt (_fun _double -> _double)))
;; (define-accelerator-impl fast-atan fast-atan.f32 (binary32) binary32 (get-ffi-obj "atanf" libvdt (_fun _double -> _double)))
