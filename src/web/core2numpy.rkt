#lang racket

(require fpbench/src/imperative)

(provide core->numpy)

(define numpy-reserved   ; Language-specific reserved names (avoid name collision)
  '(False None True and as assert break class continue def del
    elif else except finally for from global if import in is
    lambda nonlocal not or pass raise return try while with yield))

(define (operator->numpy op args ctx)
  (match (cons op args)
   ;[(list 'sum3 a b c) (format "fsum([~a, ~a, ~a])" a b c)]
   ;[(list 'sum4 a b c d) (format "fsum([~a, ~a, ~a, ~a])" a b c d)]

   [(list 'asin a) (format "numpy.arcsin(~a)" a)]
   [(list 'acos a) (format "numpy.arccos(~a)" a )]
   [(list 'atan a) (format "numpy.arctan(~a)" a )]
   [(list 'asinh a) (format "numpy.arcsinh(~a)" a )]
   [(list 'acosh a) (format "numpy.arccosh(~a)" a )]
   [(list 'atanh a) (format "numpy.arctanh(~a)" a )]
   [(list 'atan2 a b) (format "numpy.arctan2(~a, ~b)" a b)]
   [(list '+ a b) (format "numpy.add(~a, ~b)" a b)]
   [(list 'recip a) (format "numpy.reciprocal(~a)" a )]
   [(list 'neg a) (format "numpy.negative(~a)" a )]
   [(list '* a b) (format "numpy.multiply(~a, ~b)" a b)]
   [(list '/ a b) (format "numpy.divide(~a, ~b)" a b)]
   [(list 'pow a b) (format "numpy.power(~a, ~b)" a b)]
   [(list '- a b) (format "numpy.subtract(~a, ~b)" a b)]
   ;[(list 'tgamma a) (format "math.gamma(~a)" a)]
   [_ (format "numpy.~a(~a)" op (string-join args ", "))]))

(define (constant->numpy x ctx)
  (match x
   ['TRUE "True"]
   ['FALSE "False"]
   ['INFINITY "math.inf"]
   ['NAN "math.nan"]
   ['PI "math.pi"]
   ['E "math.e"]
   [(? hex?) (~a (real->double-flonum (hex->racket x)))]
   [(? number?) (~a (real->double-flonum x))]
   [(? symbol?) (~a x)]))

(define declaration->numpy
  (case-lambda
   [(var ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (format "~a = ~a" var (match prec ['binary64 "0"] ['boolean "True"]))]
   [(var val ctx)
    (format "~a = ~a" var val)]))

(define (assignment->numpy var val ctx)
  (format "~a = ~a" var val))

(define (program->numpy name args arg-ctxs body ret ctx used-vars)
  (format "def ~a(~a):\n~a\treturn ~a\n" name
          (string-join args ", ") body ret))

(define  core->numpy
  (make-imperative-compiler "numpy"
    #:operator operator->numpy
    #:constant constant->numpy
    #:declare declaration->numpy
    #:infix-ops null
    #:assign assignment->numpy
    #:program program->numpy
    #:flags '(colon-instead-of-brace
              no-parens-around-condition
              use-elif
              boolean-ops-use-name)
    #:reserved numpy-reserved))
