#lang racket

(require fpbench/src/imperative)

(provide core->Racket)

(define true-reserved   ; Language-specific reserved names (avoid name collision)
  '(False None True and as assert break class continue def del
    elif else except finally for from global if import in is
    lambda nonlocal not or pass raise return try while with yield))


(define (constant->true x ctx)
  (match x
   ['TRUE "true"]
   ['FALSE "false"]
   ['INFINITY "Inf"]
   ['NAN "NaN"]
   ['PI "π"]
   ['E "ℯ"]
   [(? hex?) (~a (real->double-flonum (hex->racket x)))]
   [(? number?) (~a (real->double-flonum x))]
   [(? symbol?) (~a x)]))

(define declaration->true
  (case-lambda
   [(var ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (format "~a = ~a" var (match prec ['binary64 "0"] ['boolean "True"]))]
   [(var val ctx)
    (format "~a = ~a" var val)]))

(define (assignment->true var val ctx)
  (format "~a = ~a" var val))

(define (program->true name args arg-ctxs body ret ctx used-vars)
  (format "function ~a(~a):\n~a\treturn ~a\n \nend" name
          (string-join args ", ") body ret))

(define core->true
  (make-imperative-compiler "true"
    #:operator operator->true
    #:constant constant->true
    #:declare declaration->true
    #:assign assignment->true
    #:program program->true
    #:flags '(colon-instead-of-brace
              no-parens-around-condition
              use-elif
              boolean-ops-use-name)
    #:reserved true-reserved))
