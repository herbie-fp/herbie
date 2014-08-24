#lang racket
(require casio/common)
(require casio/syntax)

(provide texify-expression)

(define-table texify-constants
  [l "\\ell"]
  [pi "\\pi"]
  [alpha "\\alpha"]
  [beta "\\beta"]
  [gamma "\\gamma"]
  [lambda "\\lambda"])

(define (apply-converter conv args)
  (cond
   [(string? conv) (apply format conv args)]
   [(list? conv) (apply format (list-ref conv (length args)) args)]
   [(procedure? conv) (apply conv args)]
   [else (error "Unknown syntax entry" conv)]))

(define-table texify-operators
  [+        "~a + ~a" #t]
  [-        '(#f "-~a" "~a - ~a") #t]
  [*        "~a \\cdot ~a" #t]
  [/        '(#f "\\frac1{~a}" "\\frac{~a}{~a}") #f]
  [abs      "|~a|" #f]
  [sqrt     "\\sqrt{~a}" #f]
  [sqr      "{~a}^2" #t]
  [exp      "e^{~a}" #f]
  [expt     "{~a}^{~a}" #t]
  [log      "\\log ~a " #t]
  [sin      "\\sin ~a" #t]
  [cos      "\\cos ~a" #t]
  [tan      "\\tan ~a" #t]
  [cotan    "\\cot ~a" #t]
  [asin     "\\sin^{-1} ~a" #t]
  [acos     "\\cos^{-1} ~a" #t]
  [atan     "\\tan^{-1} ~a" #t]
  [sinh     "\\sinh ~a" #t]
  [cosh     "\\cosh ~a" #t]
  [tanh     "\\tanh ~a" #t]
  [atan2    "\\tan^{-1}_* \frac{~a}{~a}" #f]
  [if       "~a ? ~a : ~a" #t]
  [>        "~a > ~a" #t]
  [<        "~a < ~a" #t]
  [<=       "~a \\le ~a" #t]
  [>=       "~a \\ge ~a" #t]
  [and      "~a \\wedge ~a" #t]
  [or       "~a \\vee ~a" #t]
  [mod      "~a \\modb ~a" #t])

(define (texify-expression expr [parens #f])
  (match expr
    [(? real?) (number->string expr)]
    [(? symbol?) (hash-ref texify-constants expr (symbol->string expr))]
    [`(,f ,args ...)
     (let* ([template (list-ref (hash-ref texify-operators f) 0)]
            [parens* (list-ref (hash-ref texify-operators f) 1)]
            [args* (for/list ([arg args])
                     (texify-expression arg parens*))]
            [result (apply-converter template args*)])
       (if parens
           (format "\\left(~a\\right)" result)
           result))]))
