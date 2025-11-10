#lang racket

;; Provide operator type information expected by src/syntax/syntax.rkt
;; rival-functions: hash mapping op symbol -> (cons otype (list itypes))

(provide rival-functions)

(define (arity . ts) ts)

;; Types are symbols: 'real for real-valued, 'bool for boolean
;; This table mirrors Rival ops used by Herbie platforms.
(define rival-functions
  (make-immutable-hash
   (list
    ;; boolean constants and ops
    (cons 'TRUE (cons 'bool (arity)))
    (cons 'FALSE (cons 'bool (arity)))
    (cons 'and (cons 'bool (arity 'bool 'bool)))
    (cons 'or  (cons 'bool (arity 'bool 'bool)))
    (cons 'not (cons 'bool (arity 'bool)))

    ;; comparisons (real -> bool)
    (cons '== (cons 'bool (arity 'real 'real)))
    (cons '!= (cons 'bool (arity 'real 'real)))
    (cons '<  (cons 'bool (arity 'real 'real)))
    (cons '>  (cons 'bool (arity 'real 'real)))
    (cons '<= (cons 'bool (arity 'real 'real)))
    (cons '>= (cons 'bool (arity 'real 'real)))

    ;; conditionals
    (cons 'if (cons 'real (arity 'bool 'real 'real)))

    ;; constants
    (cons 'PI (cons 'real (arity)))
    (cons 'E  (cons 'real (arity)))
    (cons 'INFINITY (cons 'real (arity)))
    (cons 'NAN (cons 'real (arity)))

    ;; basic arithmetic
    (cons '+ (cons 'real (arity 'real 'real)))
    (cons '- (cons 'real (arity 'real 'real)))
    (cons '* (cons 'real (arity 'real 'real)))
    (cons '/ (cons 'real (arity 'real 'real)))
    (cons 'neg (cons 'real (arity 'real)))

    ;; unary math
    (cons 'fabs   (cons 'real (arity 'real)))
    (cons 'sin    (cons 'real (arity 'real)))
    (cons 'cos    (cons 'real (arity 'real)))
    (cons 'tan    (cons 'real (arity 'real)))
    (cons 'asin   (cons 'real (arity 'real)))
    (cons 'acos   (cons 'real (arity 'real)))
    (cons 'atan   (cons 'real (arity 'real)))
    (cons 'sinh   (cons 'real (arity 'real)))
    (cons 'cosh   (cons 'real (arity 'real)))
    (cons 'tanh   (cons 'real (arity 'real)))
    (cons 'asinh  (cons 'real (arity 'real)))
    (cons 'acosh  (cons 'real (arity 'real)))
    (cons 'atanh  (cons 'real (arity 'real)))
    (cons 'lgamma (cons 'real (arity 'real)))
    (cons 'tgamma (cons 'real (arity 'real)))
    (cons 'sqrt   (cons 'real (arity 'real)))
    (cons 'cbrt   (cons 'real (arity 'real)))
    (cons 'exp    (cons 'real (arity 'real)))
    (cons 'exp2   (cons 'real (arity 'real)))
    (cons 'expm1  (cons 'real (arity 'real)))
    (cons 'log    (cons 'real (arity 'real)))
    (cons 'log2   (cons 'real (arity 'real)))
    (cons 'log10  (cons 'real (arity 'real)))
    (cons 'log1p  (cons 'real (arity 'real)))
    (cons 'erf    (cons 'real (arity 'real)))
    (cons 'erfc   (cons 'real (arity 'real)))
    (cons 'logb   (cons 'real (arity 'real)))
    (cons 'rint   (cons 'real (arity 'real)))
    (cons 'round  (cons 'real (arity 'real)))
    (cons 'floor  (cons 'real (arity 'real)))
    (cons 'ceil   (cons 'real (arity 'real)))
    (cons 'trunc  (cons 'real (arity 'real)))

    ;; binary math
    (cons 'pow       (cons 'real (arity 'real 'real)))
    (cons 'hypot     (cons 'real (arity 'real 'real)))
    (cons 'atan2     (cons 'real (arity 'real 'real)))
    (cons 'copysign  (cons 'real (arity 'real 'real)))
    (cons 'fdim      (cons 'real (arity 'real 'real)))
    (cons 'fmax      (cons 'real (arity 'real 'real)))
    (cons 'fmin      (cons 'real (arity 'real 'real)))
    (cons 'fmod      (cons 'real (arity 'real 'real)))
    (cons 'remainder (cons 'real (arity 'real 'real)))
    (cons 'fma       (cons 'real (arity 'real 'real 'real)))
    )))
