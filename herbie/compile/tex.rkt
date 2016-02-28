#lang racket
(require "../common.rkt")
(require "../syntax.rkt")

(provide texify-expression mathjax-url texify-operators texify-constants apply-converter)

(define mathjax-url "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")

(define-table texify-constants
  [l       "\\ell"]
  [pi      "\\pi"]
  [eps     "\\varepsilon"]
  [epsilon "\\varepsilon"]
  [alpha   "\\alpha"]
  [beta    "\\beta"]
  [gamma   "\\gamma"]
  [phi     "\\phi"]
  [phi1    "\\phi_1"]
  [phi2    "\\phi_2"]
  [lambda  "\\lambda"]
  [lambda1 "\\lambda_1"])

(define (apply-converter conv args [idx #f])
  (cond
   [(string? conv) (apply format conv args)]
   [(list? conv) (apply-converter (list-ref conv (length args)) args idx)]
   [(procedure? conv) (apply conv (if idx (cons idx args) args))]
   [else (error "Unknown syntax entry" conv)]))

(define (tag str idx)
  (format "\\class{location}{\\cssId{~a}{\\color{red}{\\enclose{circle}{~a}}}}" idx str))
(define (untag str)
  (format "\\color{black}{~a}" str))

(define (tag-inner-untag str idx . args)
  (tag (apply format str (map untag args)) idx))
(define (tag-infix op idx arg1 arg2)
  (format "~a ~a ~a" arg1 (tag op idx) arg2))

(define-table texify-operators
  [+        '(#f "+~a" "~a + ~a")
            `(#f ,(λ (idx a) (format "~a~a" (tag "+" idx) a))
                 ,(curry tag-infix "+"))
            '+ '+]
  [-        '(#f "-~a" "~a - ~a")
            `(#f ,(λ (idx a) (format "~a~a" (tag "-" idx) a))
                 ,(curry tag-infix "-"))
            '+ '+]
  [*        "~a \\cdot ~a"
            (curry tag-infix "\\cdot")
            '* '*]
  [/        '(#f "\\frac1{~a}" "\\frac{~a}{~a}")
            `(#f ,(curry tag-inner-untag "\\frac1{~a}")
                 ,(curry tag-inner-untag "\\frac{~a}{~a}"))
            #f #t]
  [abs      "\\left|~a\\right|"
            (curry tag-inner-untag "\\left|~a\\right|")
            #f #t]
  [sqrt     "\\sqrt{~a}"
            (curry tag-inner-untag "\\sqrt{~a}")
            #f #t]
  [cbrt     "\\sqrt[\\leftroot{-1}\\uproot{2}\\scriptstyle 3]{~a}"
            (curry tag-inner-untag "\\sqrt[\\leftroot{-1}\\uproot{2}\\scriptstyle 3]{~a}")
            #f #t]
  [hypot    "\\sqrt[\\leftroot{-1}\\uproot{2}\\scriptstyle *]{~a^2 + ~a^2}^*"
            (curry tag-inner-untag "\\sqrt[\\leftroot{-1}\\uproot{2}\\scriptstyle *]{~a^2 + ~a^2}^*")
            #f #t]
  [sqr      "{~a}^2"
            (lambda (idx a) (format "{~a}^{~a}" a (tag "2" idx)))
            #f #f]
  [cube     "{~a}^3"
            (lambda (idx a) (format "{~a}^{~a}" a (tag "3" idx)))
            #f #f]
  [exp      "e^{~a}"
            (curry tag-inner-untag "e^{~a}")
            #f #t]
  [expm1    "\\exp_* (~a - 1)"
            (curry tag-inner-untag "\\exp_* (~a - 1)")
            'fn #t]
  [expt     "{~a}^{~a}"
            (curry tag-inner-untag "{~a}^{~a}")
            #f #f]
  [log      "\\log ~a"
            (curry tag-inner-untag "\\log (~a)")
            'fn #f]
  [log1p    "\\log_* (1 + ~a)"
            (curry tag-inner-untag "\\log_* (1 + ~a)")
            '+ #f]
  [sin      "\\sin ~a"
            (curry tag-inner-untag "\\sin ~a")
            'fn #f]
  [cos      "\\cos ~a"
            (curry tag-inner-untag "\\cos ~a")
            'fn #f]
  [tan      "\\tan ~a"
            (curry tag-inner-untag "\\tan ~a")
            'fn #f]
  [cotan    "\\cot ~a"
            (curry tag-inner-untag "\\cot ~a")
            'fn #f]
  [asin     "\\sin^{-1} ~a"
            (curry tag-inner-untag "\\sin^{-1} ~a")
            'fn #f]
  [acos     "\\cos^{-1} ~a"
            (curry tag-inner-untag "\\cos^{-1} ~a")
            'fn #f]
  [atan     "\\tan^{-1} ~a"
            (curry tag-inner-untag "\\tan^{-1} ~a")
            'fn #f]
  [sinh     "\\sinh ~a"
            (curry tag-inner-untag "\\sinh ~a")
            'fn #f]
  [cosh     "\\cosh ~a"
            (curry tag-inner-untag "\\cosh ~a")
            'fn #f]
  [tanh     "\\tanh ~a"
            (curry tag-inner-untag "\\tanh ~a")
            'fn #f]
  [atan2    "\\tan^{-1}_* \\frac{~a}{~a}"
            (curry tag-inner-untag "\\tan^{-1}_* \\frac{~a}{~a}")
            'fn #t]
  [if       "~a ? ~a : ~a"
            (λ (idx a b c)
              (format "~a ~a ~a : ~a" a (tag "?" idx) b c))
            #t #t]
  [=        "~a = ~a"
            (curry tag-infix "=")
            #f #t]
  [>        "~a \\gt ~a"
            (curry tag-infix "\\gt")
            #f #t]
  [<        "~a \\lt ~a"
            (curry tag-infix "\\lt")
            #f #t]
  [<=       "~a \\le ~a"
            (curry tag-infix "\\le")
            #f #t]
  [>=       "~a \\ge ~a"
            (curry tag-infix "\\ge")
            #f #t]
  [and      "~a \\land ~a"
            (curry tag-infix "\\land")
            '* '*]
  [or       "~a \\lor ~a"
            (curry tag-infix "\\lor")
            '+ '+]
  [mod      "~a \\modb ~a"
            (curry tag-infix "\\modb")
            #t #f]
  [fma      "\\mathsr{fma}_*(~a, ~a, ~a)"
            (curry tag-infix "\\mathsr{fma}_*")
            'fn #f])

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

;; The highlight ops are an alist of locations to indexes that marks
;; those locations as highlighted with the given location
;; index. highlight-ops and loc/colors are not meant to be used
;; simultaniously.
(define (texify-expression expr #:loc [color-loc #f] #:color [color "red"] #:highlight-ops [highlight-locs '()])
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
                [highlight-template
                 (list-ref (hash-ref texify-operators f) 1)]
                [self-paren-level (list-ref (hash-ref texify-operators f) 2)]
                [arg-paren-level (list-ref (hash-ref texify-operators f) 3)]
                [args* (for/list ([arg args] [id (in-naturals 1)])
                         (texify arg arg-paren-level (cons id loc)))]
                [loc? (assoc (reverse loc) highlight-locs)]
                [result (if loc?
                            (apply-converter highlight-template args* (cdr loc?))
                            (apply-converter template args*))]
                [paren-result
                 (if (parens-< parens self-paren-level)
                     result
                     (format "\\left(~a\\right)" result))])
           paren-result)]))

    (if (equal? color-loc* loc)
        (format "\\color{~a}{~a}" color result)
        result)))
