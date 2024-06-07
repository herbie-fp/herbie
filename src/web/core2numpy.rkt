#lang racket

(require fpbench/src/imperative)

(provide core->numpy)

(define numpy-reserved   ; Language-specific reserved names (avoid name collision)
  '(False None True and as assert break class continue def del
    elif else except finally for from global if import in is
    lambda nonlocal not or pass raise return try while with yield))

(define (operator->numpy op args ctx)
  (match (cons op args)
   [(list 'deg2rad a) (format "numpy.deg2rad(~a)" a)]
   [(list 'rad2deg a) (format "numpy.rad2deg(~a)" a)]
   [(list 'logaddexp a b) (format "numpy.logaddexp(~a, ~a)" a b)]
   [(list 'logaddexp2 a b) (format "numpy.logaddexp2(~a, ~a)" a b)]
   ;[(list 'square a) (format "numpy.square(~a)" a)]
   [(list 'asin a) (format "numpy.arcsin(~a)" a)]
   [(list 'acos a) (format "numpy.arccos(~a)" a)]
   [(list 'atan a) (format "numpy.arctan(~a)" a)]
   [(list 'asinh a) (format "numpy.arcsinh(~a)" a)]
   [(list 'acosh a) (format "numpy.arccosh(~a)" a)]
   [(list 'atanh a) (format "numpy.arctanh(~a)" a)]
   [(list 'atan2 a b) (format "numpy.arctan2(~a, ~a)" a b)]
   [(list '+ a b) (format "numpy.add(~a, ~a)" a b)]
   [(list 'recip a) (format "numpy.reciprocal(~a)" a)]
   [(list 'neg a) (format "numpy.negative(~a)" a)]
   [(list '* a b) (format "numpy.multiply(~a, ~a)" a b)]
   [(list '/ a b) (format "numpy.divide(~a, ~a)" a b)]
   [(list 'pow a b) (format "numpy.float_power(~a, ~a)" a b)]
   [(list '- a) (format "numpy.negative(~a)" a)]
   [(list '- a b) (format "numpy.subtract(~a, ~a)" a b)]
   [(list 'rint a) (format "numpy.rint(~a)" a )] 
   [(list '> a b) (format "numpy.greater(~a, ~a)" a b)]
   [(list '>= a b) (format "numpy.greater_equal(~a, ~a)" a b)]
   [(list '< a b) (format "numpy.less(~a, ~a)" a b)]
   [(list '<= a b) (format "numpy.less_equal(~a, ~a)" a b)]
   [(list '== a b) (format "numpy.equal(~a, ~a)" a b)]
   [(list '!= a b) (format "numpy.not_equal(~a, ~a)" a b)]
   ;;TODO add variary comparison operators
   [(list 'not a) (format "numpy.logical_not(~a)" a)]
   [(list 'and a ...) (format (operator-nary->binary "and" args))]
   [(list 'or a ...) (format (operator-nary->binary "or" args))]
   [_ (format "numpy.~a(~a)" op (string-join args ", "))]))

  (define (operator-nary->binary name arguments)
  (for/fold ([l (car arguments)]) ([r (cdr arguments)])
    (format "numpy.logical_~a(~a, ~a)" name l r)))

(define (constant->numpy x ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (match x
   ['TRUE (format "numpy.full(length, True)")]
   ['FALSE (format "numpy.full(length, False)")]
   ['INFINITY (format "numpy.full(length, numpy.inf, dtype=~a)"(type->numpy prec))]
   ['NAN (format "numpy.full(length, numpy.nan, dtype=~a)" (type->numpy prec))]
   ['PI (format "numpy.full(length, numpy.pi, dtype=~a)" (type->numpy prec))]
   ['E (format "numpy.full(length, numpy.e, dtype=~a)" (type->numpy prec))]
   [(? hex?) (format "numpy.full(length, ~a, dtype=~a)" (real->double-flonum (hex->racket x)) (type->numpy prec))]
   [(? number?) (format "numpy.full(length, ~a, dtype=~a)" (real->double-flonum x) (type->numpy prec))]
   [(? symbol?) (format "numpy.full(length, ~a, dtype=~a)" x (type->numpy prec))]))

(define declaration->numpy
  (case-lambda
   [(var ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (format "~a = ~a" var (match prec ['binary64 "numpy.zeros(length)"] ['binary32 "numpy.zeros(length)"] ['boolean "True"]))]
   [(var val ctx)
    (format "~a = ~a" var val)]))

(define (assignment->numpy var val ctx)
  (format "~a = ~a" var val))

(define (program->numpy name args arg-ctxs body ret ctx used-vars)
  (format "def ~a(~a):\n\tlength = ~a\n~a\treturn ~a\n" name
          (string-join args ", ") (if (> (length args) 0) (format "~a.shape" (first args)) "10000") body ret))

(define (visit-if/numpy vtor cond ift iff #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define prec (ctx-lookup-prop ctx ':precision))
  (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
  (define-values (iff* iff-ctx) (visit/ctx vtor iff ift-ctx))
  (define-values (cond* cond-ctx) (visit/ctx vtor cond ctx))
  (define-values (ctx* name) (ctx-random-name iff-ctx prec))

  (printf "~a~a = numpy.where(~a, ~a, ~a)\n" indent name cond* ift* iff*)
  (values name ctx*))

(define (visit-op_/numpy vtor op args #:ctx ctx)
  (match (cons op args)
    [(list '- x)
     ;; TODO: Any better way to do this?
     (visit-op_/numpy vtor '- (list 0 x) #:ctx ctx)]
    [else
     (define prec (ctx-lookup-prop ctx ':precision))
     (define indent (ctx-lookup-extra ctx 'indent))
     (define-values (name-ctx name) (ctx-random-name ctx prec))
     (define args*
       (for/list ([arg args])
         (define-values (arg* arg-ctx) (visit/ctx vtor arg ctx))
         arg*))
     (define ctx*
       (if (set-member? bool-ops op)
           (ctx-update-props name-ctx (list ':precision 'boolean))
           name-ctx))
     (printf "~a~a = ~a;\n" indent name (operator->numpy op args* ctx))
     (values name ctx*)]))


(define (visit-number/numpy vtor x #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (name-ctx name) (ctx-random-name ctx prec))
  (printf "~a~a = ~a\n" indent name (constant->numpy x ctx))
  (values name name-ctx))

(define (type->numpy type)
  (match type
    ['binary64 "numpy.float64"]
    ['binary32 "numpy.float32"]))

(define-expr-visitor imperative-visitor numpy-visitor
  [visit-if visit-if/numpy]
  [visit-number visit-number/numpy]
  [visit-op visit-op_/numpy]
)

(define core->numpy
  (make-imperative-compiler "numpy"
    #:operator operator->numpy
    #:infix-ops null
    #:constant constant->numpy
    #:declare declaration->numpy
    #:assign assignment->numpy
    #:program program->numpy
    #:visitor numpy-visitor
    #:reserved numpy-reserved))
