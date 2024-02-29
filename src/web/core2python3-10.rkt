#lang racket

(require fpbench/src/imperative)

(provide core->python)

(define python-reserved   ; Language-specific reserved names (avoid name collision)
  '(False None True and as assert break class continue def del
    elif else except finally for from global if import in is
    lambda nonlocal not or pass raise return try while with yield))

(define (operator->python op args ctx)
  (match (cons op args)
   [(list 'sum3 a b c) (format "fsum([~a, ~a, ~a])" a b c)]
   [(list 'sum4 a b c d) (format "fsum([~a, ~a, ~a, ~a])" a b c d)]
   [(list 'fmax a b) (format "max(~a, ~a)" a b)]
   [(list 'fmin a b) (format "min(~a, ~a)" a b)]
   [(list 'tgamma a) (format "math.gamma(~a)" a)]
   [_ (format "math.~a(~a)" op (string-join args ", "))]))

(define (constant->python x ctx)
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

(define declaration->python
  (case-lambda
   [(var ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (format "~a = ~a" var (match prec ['binary64 "0"] ['boolean "True"]))]
   [(var val ctx)
    (format "~a = ~a" var val)]))

(define (assignment->python var val ctx)
  (format "~a = ~a" var val))

(define (program->python name args arg-ctxs body ret ctx used-vars)
  (format "def ~a(~a):\n~a\treturn ~a\n" name
          (string-join args ", ") body ret))

(define core->python
  (make-imperative-compiler "python"
    #:operator operator->python
    #:constant constant->python
    #:declare declaration->python
    #:assign assignment->python
    #:program program->python
    #:flags '(colon-instead-of-brace
              no-parens-around-condition
              use-elif
              boolean-ops-use-name)
    #:reserved python-reserved))
