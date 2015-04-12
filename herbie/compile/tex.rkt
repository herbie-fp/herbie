#lang racket
(require "../common.rkt")
(require "../syntax.rkt")

(provide texify-expression)

(define-table texify-constants
  [l "\\ell"]
  [pi "\\pi"]
  [eps "\\varepsilon"]
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
  [+        '(#f "+~a" "~a + ~a") '+ '+]
  [-        '(#f "-~a" "~a - ~a") '+ '+]
  [*        "~a \\cdot ~a" '* '*]
  [/        '(#f "\\frac1{~a}" "\\frac{~a}{~a}") #f #t]
  [abs      "\\left|~a\\right|" #f #t]
  [sqrt     "\\sqrt{~a}" #f #t]
  [sqr      "{~a}^2" #f #f]
  [exp      "e^{~a}" #f #t]
  [expt     "{~a}^{~a}" #f #f]
  [log      "\\log ~a" 'fn #f]
  [sin      "\\sin ~a" 'fn #f]
  [cos      "\\cos ~a" 'fn #f]
  [tan      "\\tan ~a" 'fn #f]
  [cotan    "\\cot ~a" 'fn #f]
  [asin     "\\sin^{-1} ~a" 'fn #f]
  [acos     "\\cos^{-1} ~a" 'fn #f]
  [atan     "\\tan^{-1} ~a" 'fn #f]
  [sinh     "\\sinh ~a" 'fn #f]
  [cosh     "\\cosh ~a" 'fn #f]
  [tanh     "\\tanh ~a" 'fn #f]
  [atan2    "\\tan^{-1}_2 \\frac{~a}{~a}" 'fn #t]
  [if       "~a ? ~a : ~a" #t #t]
  [=        "~a == ~a" #f #t]
  [>        "~a > ~a" #f #t]
  [<        "~a < ~a" #f #t]
  [<=       "~a \\le ~a" #f #t]
  [>=       "~a \\ge ~a" #f #t]
  [and      "~a \\wedge ~a" '* '*]
  [or       "~a \\vee ~a" '+ '+]
  [mod      "~a \\modb ~a" #t #f])

(define parens-precedence '(#t + * fn #f))

(define (parens-< a b)
  (let loop ([l parens-precedence])
    (cond
     [(and (eq? (car l) a) (eq? (car l) b)) #f]
     [(eq? (car l) a) #t]
     [(eq? (car l) b) #f]
     [else (loop (cdr l))])))

(define (texify-expression expr [parens #t])
  "Compile an expression to TeX code.
   The TeX is intended to be used in math mode.

   `parens` is one of #f, '+, '*, 'fn, or #t"
  (match expr
    [(? real?) (number->string expr)]
    [(? symbol?) (car (hash-ref texify-constants expr (list (symbol->string expr))))]
    [`(,f ,args ...)
     (let* ([template (list-ref (hash-ref texify-operators f) 0)]
            [self-paren-level (list-ref (hash-ref texify-operators f) 1)]
            [arg-paren-level (list-ref (hash-ref texify-operators f) 2)]
            [args* (for/list ([arg args])
                     (texify-expression arg arg-paren-level))]
            [result (apply-converter template args*)])
       (if (parens-< parens self-paren-level)
           result
           (format "\\left(~a\\right)" result)))]))
