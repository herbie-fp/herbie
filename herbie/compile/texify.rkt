#lang racket
(require "../common.rkt")
(require "../syntax.rkt")
(require "../programs.rkt")

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
  (define (render-func func)
    (match func
      [`(,f ,args ...)
       (match (hash-ref texify-operators f)
         [`(,template ,self-paren-level ,arg-paren-level)
          (apply-converter template args)])]))
  (define (render-assigns vars vals)
     (for/fold ([texified-result ""])
         ([var vars] [val vals])
       (string-append texified-result
                      (format "~a \\gets ~a\\\\" (symbol->string var) val))))
  (expression-induct
   expr
   #:constant (λ (const)
                (if (number? const)
                    (number->string const)
                    (car (hash-ref texify-constants expr))))
   #:variable symbol->string
   #:primitive render-func 
   #:predicate render-func 
   #:let (λ (lt)
           (match lt
             [`(let ([,vars ,vals] ...) ,body)
              (string-append (render-assigns vars vals) body)]))
   #:fold (λ (fld)
            (match fld
              [`(for/fold ([,accs ,inits] ...)
                    ([,items ,lst-exprs] ...)
                  ,body)
               (let ([lst-names (for/list ([lst-expr lst-exprs])
                                      (gensym "lst"))])
                 (string-append
                  (render-assigns accs inits)
                  (apply render-assigns
                         (flip-lists (for/list ([lst-name lst-names] [lst-expr lst-exprs])
                                       (list lst-name lst-expr))))
                  "for "
                  (for/fold ([texified-result ""])
                      ([item items] [lst lst-names])
                    (string-append texified-result
                                   (symbol->string item)
                                   " in "
                                   (symbol->string lst)
                                   ","))
                  ":\\\\"
                  (if (= 1 (length accs))
                      (string-append
                       (symbol->string (car accs)) " \\gets "
                       body)
                      (match body
                        [`(values . ,exprs)
                         (apply
                          string-append
                          (for/list ([acc accs] [expr exprs])
                            (format "~a \\gets ~a\\\\" acc expr)))]
                        [_ (error "uh oh, you don't return values, but you have multiple accs")]))))]))))
