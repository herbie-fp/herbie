#lang racket
(require unstable/sequence)
(require "../common.rkt")
(require "../syntax.rkt")

(provide texify-expression)

(define-table texify-constants
  [l "\\ell"]
  [pi "\\pi"]
  [eps "\\varepsilon"]
  [epsilon "\\epsilon"]
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
  [atan2    "\\tan^{-1}_* \\frac{~a}{~a}" 'fn #t]
  [if       "~a ? ~a : ~a" #t #t]
  [=        "~a = ~a" #f #t]
  [>        "~a \\gt ~a" #f #t]
  [<        "~a \\lt ~a" #f #t]
  [<=       "~a \\le ~a" #f #t]
  [>=       "~a \\ge ~a" #f #t]
  [and      "~a \\land ~a" '* '*]
  [or       "~a \\lor ~a" '+ '+]
  [mod      "~a \\modb ~a" #t #f])

(define parens-precedence '(#t + * fn #f))

(define (parens-< a b)
  (let loop ([l parens-precedence])
    (cond
     [(and (eq? (car l) a) (eq? (car l) b)) #f]
     [(eq? (car l) a) #t]
     [(eq? (car l) b) #f]
     [else (loop (cdr l))])))

(define (collect-branches expr loc)
  (match expr
    [`(if ,cond ,ift ,iff)
     (cons (list cond ift loc) (collect-branches iff (cons 3 loc)))]
    [else
     (list (list #t expr loc))]))

(define (texify-expression expr #:loc [color-loc #f] #:color [color "red"])
  "Compile an expression to TeX code.
   The TeX is intended to be used in math mode.

   `parens` is one of #f, '+, '*, 'fn, or #t"

  (define color-loc* (if color-loc (reverse color-loc) #f))
  
  (let texify ([expr expr] [parens #t] [loc '(2)])
    (define result
      (match expr
        [(and (? integer?) (? exact?)) (number->string expr)]
        [(and (? rational?) (? exact?))
         (format "\\frac{~a}{~a}" (numerator expr) (denominator expr))]
        [(? real?)
         (match (string-split (number->string expr) "e")
           [(list num) num]
           [(list significand exp)
            (if (equal? significand "1")
                (format "10^{~a}" exp)
                (format "~a \\cdot 10^{~a}" significand exp))])]
        [(? symbol?) (car (hash-ref texify-constants expr (list (symbol->string expr))))]
        [`(if ,cond ,ift ,iff)
         (define branches (collect-branches expr loc))
         (format "\\begin{cases} ~a \\end{cases}"
                 (string-join
                  (for/list ([rec branches])
                    (match rec
                      [(list #t expr loc)
                       (format "~a & \\text{otherwise}"
                               (texify expr #t (cons 2 loc)))]
                      [(list condition expr loc)
                       (format "~a & \\text{when } ~a"
                               (texify expr #t (cons 2 loc))
                               (texify condition #t (cons 1 loc)))]))
                  " \\\\ "))]
        [`(,f ,args ...)
         (let* ([template (list-ref (hash-ref texify-operators f) 0)]
                [self-paren-level (list-ref (hash-ref texify-operators f) 1)]
                [arg-paren-level (list-ref (hash-ref texify-operators f) 2)]
                [args* (for/list ([arg args] [id (in-naturals 1)])
                         (texify arg arg-paren-level (cons id loc)))]
                [result (apply-converter template args*)]
                [paren-result
                 (if (parens-< parens self-paren-level)
                     result
                     (format "\\left(~a\\right)" result))])
           paren-result)]))

    (if (equal? color-loc* loc)
        (format "\\color{~a}{~a}" color result)
        result)))
