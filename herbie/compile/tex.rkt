#lang racket
(require "../common.rkt")
(require "../syntax.rkt")
(require "../programs.rkt")

(provide mathjax-url texify-expr texify-prog)

(define mathjax-url
  "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")

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
  [lambda1 "\\lambda_1"]
  [lambda2 "\\lambda_2"])

; Note that normal string converters ignore idx and
; procedure converters take idx as first arg.
(define (apply-converter conv args [idx #f])
  (cond
    [(string? conv)     (apply format conv args)]
    [(list? conv)       (apply-converter (list-ref conv (length args)) args idx)]
    [(procedure? conv)  (apply conv (if idx (cons idx args) args))]
    [else               (error "Unknown syntax entry" conv)]))

(define parens-precedence '(#t + * fn #f))

(define (parens-< a b)
  (< (index-of parens-precedence a)
     (index-of parens-precedence b)))

; "enclose" is a MathJax extension which may
; not work with standard TeX processors.
(define (tag str idx)
  (let* ([enc (format "\\enclose{circle}{~a}" str)]
         [col (format "\\color{red}{~a}" enc)]
         [css (format "\\class{location}{\\cssId{~a}{~a}}" idx col)])
    css))

(define (untag str)
  (format "\\color{black}{~a}" str))

(define ((tag-inner-untag str) idx . args)
  (tag (apply format str (map untag args)) idx))

(define ((tag-infix op) idx arg1 arg2)
  (format "~a ~a ~a" arg1 (tag op idx) arg2))

; self-paren-level : #t --> paren me
;                    #f --> do not paren me
;
; args-paren-level : #t --> do not paren args
;                    #f --> paren args
(define-table texify-operators
  [+    '(#f "+~a" "~a + ~a")
        `(#f ,(tag-inner-untag "+~a")
             ,(tag-infix "+"))
        '+ '+]
  [-    '(#f "-~a" "~a - ~a")
        `(#f ,(tag-inner-untag "-~a")
             ,(tag-infix "-"))
        '+ '+]
  [*    "~a \\cdot ~a"
        (tag-infix "\\cdot")
        '* '*]
  [/    '(#f "\\frac1{~a}" "\\frac{~a}{~a}")
        `(#f ,(tag-inner-untag "\\frac1{~a}")
             ,(tag-inner-untag "\\frac{~a}{~a}"))
        #f #t]

  [sqr      "{~a}^2"
            (lambda (idx a) (format "{~a}^{~a}" a (tag "2" idx)))
            #f #f]
  [cube     "{~a}^3"
            (lambda (idx a) (format "{~a}^{~a}" a (tag "3" idx)))
            #f #f]
  [cotan    "\\cot ~a"
            (tag-inner-untag "\\cot ~a")
            'fn #f]

  [acos      "\\cos^{-1} ~a"
             (tag-inner-untag "\\cos^{-1} ~a")
             'fn #f]
  [acosh     "\\cosh^{-1} ~a"
             (tag-inner-untag "\\cosh^{-1} ~a")
             'fn #f]
  [asin      "\\sin^{-1} ~a"
             (tag-inner-untag "\\sin^{-1} ~a")
             'fn #f]
  [asinh     "\\sinh^{-1} ~a"
             (tag-inner-untag "\\sinh^{-1} ~a")
             'fn #f]
  [atan      "\\tan^{-1} ~a"
             (tag-inner-untag "\\tan^{-1} ~a")
             'fn #f]
  [atan2     "\\tan^{-1}_* \\frac{~a}{~a}"
             (tag-inner-untag "\\tan^{-1}_* \\frac{~a}{~a}")
             'fn #t]
  [atanh     "\\tanh^{-1} ~a"
             (tag-inner-untag "\\tanh^{-1} ~a")
             'fn #f]
  [cbrt      "\\sqrt[3]{~a}"
             (tag-inner-untag "\\sqrt[3]{~a}")
             #f #t]
  [ceil      "\\left\\lceil~a\\right\\rceil"
             (tag-inner-untag "\\left\\lceil~a\\right\\rceil")
             #f #t]
  [copysign  "\\mathsf{copysign}\\left(~a, ~a\\right)"
             (tag-inner-untag "\\mathsf{copysign}\\left(~a, ~a\\right)")
             #f #t]
  [cos       "\\cos ~a"
             (tag-inner-untag "\\cos ~a")
             'fn #f]
  [cosh      "\\cosh ~a"
             (tag-inner-untag "\\cosh ~a")
             'fn #f]
  [erf       "\\mathsf{erf} ~a"
             (tag-inner-untag "\\mathsf{erf} ~a")
             'fn #f]
  [erfc      "\\mathsf{erfc} ~a"
             (tag-inner-untag "\\mathsf{erfc} ~a")
             'fn #f]
  [exp       "e^{~a}"
             (tag-inner-untag "e^{~a}")
             #f #t]
  [exp2      "2^{~a}"
             (tag-inner-untag "2^{~a}")
             #f #t]
  [expm1     "(e^{~a} - 1)^*"
             (tag-inner-untag "(e^{~a} - 1)^*")
             #f #t]
  [abs       "\\left|~a\\right|"
             (tag-inner-untag "\\left|~a\\right|")
             #f #t]
  [fdim      "\\mathsf{fdim}\\left(~a, ~a\\right)"
             (tag-inner-untag "\\mathsf{fdim}\\left(~a, ~a\\right)")
             #f #t]
  [floor     "\\left\\lfloor~a\\right\\rfloor"
             (tag-inner-untag "\\left\\lfloor~a\\right\\rfloor")
             #f #t]
  [fma       "(~a * ~a + ~a)_*"
             (tag-inner-untag "(~a * ~a + ~a)_*")
             #f #f]
  [fmax      "\\mathsf{fmax}\\left(~a, ~a\\right)"
             (tag-inner-untag "\\mathsf{fmax}\\left(~a, ~a\\right)")
             #f #t]
  [fmin      "\\mathsf{fmin}\\left(~a, ~a\\right)"
             (tag-inner-untag "\\mathsf{fmin}\\left(~a, ~a\\right)")
             #f #t]
  [mod       "~a \\mathsf{mod} ~a"
             (tag-infix "\\mathsf{mod}")
             #t #f]
  [hypot     "\\sqrt{~a^2 + ~a^2}^*"
             (tag-inner-untag "\\sqrt{~a^2 + ~a^2}^*")
             #f #f]
  [j0        "\\mathsf{j0} ~a"
             (tag-inner-untag "\\mathsf{j0} ~a")
             'fn #f]
  [j1        "\\mathsf{j1} ~a"
             (tag-inner-untag "\\mathsf{j1} ~a")
             'fn #f]
  [log       "\\log ~a"
             (tag-inner-untag "\\log ~a")
             'fn #f]
  [log10     "\\log_{10} ~a"
             (tag-inner-untag "\\log_{10} ~a")
             'fn #f]
  [log1p     "\\log_* (1 + ~a)"
             (tag-inner-untag "\\log_* (1 + ~a)")
             #f '+]
  [log2      "\\log_{2} ~a"
             (tag-inner-untag "\\log_{2} ~a")
             'fn #f]
  [expt      "{~a}^{~a}"
             (tag-inner-untag "{~a}^{~a}")
             #f #f]
  [remainder "~a \\mathsf{rem} ~a"
             (tag-infix "\\mathsf{rem}")
             #t #f]
  [round     "\\mathsf{round} ~a"
             (tag-inner-untag "\\mathsf{round} ~a")
             'fn #f]
  [sin       "\\sin ~a"
             (tag-inner-untag "\\sin ~a")
             'fn #f]
  [sinh      "\\sinh ~a"
             (tag-inner-untag "\\sinh ~a")
             'fn #f]
  [sqrt      "\\sqrt{~a}"
             (tag-inner-untag "\\sqrt{~a}")
             #f #t]
  [tan       "\\tan ~a"
             (tag-inner-untag "\\tan ~a")
             'fn #f]
  [tanh      "\\tanh ~a"
             (tag-inner-untag "\\tanh ~a")
             'fn #f]
  [trunc     "\\mathsf{trunc} ~a"
             (tag-inner-untag "\\mathsf{trunc} ~a")
             'fn #f]
  [y0        "\\mathsf{y0} ~a"
             (tag-inner-untag "\\mathsf{y0} ~a")
             'fn #f]
  [y1        "\\mathsf{y1} ~a"
             (tag-inner-untag "\\mathsf{y1} ~a")
             'fn #f]

  [if     "~a ? ~a : ~a"
          (lambda (idx a b c)
            (format "~a ~a ~a : ~a" a (tag "?" idx) b c))
          #t #t]
  [=      "~a = ~a"
          (tag-infix "=")
          #f #t]
  [>      "~a \\gt ~a"
          (tag-infix "\\gt")
          #f #t]
  [<      "~a \\lt ~a"
          (tag-infix "\\lt")
          #f #t]
  [>=     "~a \\ge ~a"
          (tag-infix "\\ge")
          #f #t]
  [<=     "~a \\le ~a"
          (tag-infix "\\le")
          #f #t]
  [not    "\\neg ~a"
          (tag-inner-untag "\\neg ~a")
          'fn #f]
  [and    "~a \\land ~a"
          (tag-infix "\\land")
          '* '*]
  [or     "~a \\lor ~a"
          (tag-infix "\\lor")
          '+ '+])

(define (collect-branches expr loc)
  (match expr
    [`(if ,cond ,ift ,iff)
     (cons (list cond ift loc)
           (collect-branches iff (cons 3 loc)))]
    [else
     (list (list #t expr loc))]))

;; The highlight ops are an alist of locations to indexes that marks
;; those locations as highlighted with the given location
;; index. highlight-ops and loc/colors are not meant to be used
;; simultaniously.
(define (texify-prog prog
                     #:loc [color-loc #f]
                     #:color [color "red"]
                     #:highlight-ops [highlight-locs '()])
  "Compile the body of a program to math mode TeX."
  (let texify ([expr (program-body prog)] [parens #t] [loc '(2)])
    (format
      (if (and color-loc (equal? (reverse color-loc) loc))
        (format "\\color{~a}{~~a}" color)
        "~a")
      (match expr
        [(? exact-integer?)
         (number->string expr)]
        [(? exact-rational?)
         (format "\\frac{~a}{~a}" (numerator expr) (denominator expr))]
        [(? real?)
         (match (string-split (number->string expr) "e")
           [(list num) num]
           [(list significand exp)
            (if (equal? significand "1")
                (format "10^{~a}" exp)
                (format "~a \\cdot 10^{~a}" significand exp))])]
        [(? symbol?)
         (if (hash-has-key? texify-constants expr)
           (car (hash-ref texify-constants expr))
           (symbol->string expr))]
        [`(if ,cond ,ift ,iff)
          (let ([texed-branches
                  (for/list ([branch (collect-branches expr loc)])
                    (match branch
                           [(list #t bexpr bloc)
                            (format "~a & \\text{otherwise}"
                              (texify bexpr #t (cons 2 bloc)))]
                           [(list bcond bexpr bloc)
                            (format "~a & \\text{when } ~a"
                              (texify bexpr #t (cons 2 bloc))
                              (texify bcond #t (cons 1 bloc)))]))])
            (format "\\begin{cases} ~a \\end{cases}"
                 (string-join texed-branches " \\\\ ")))]
        [`(,f ,args ...)
         (match (hash-ref texify-operators f)
           [(list template highlight-template self-paren-level arg-paren-level)
            (let ([texed-args
                    (for/list ([arg args] [id (in-naturals 1)])
                      (texify arg arg-paren-level (cons id loc)))]
                  [hl-loc
                    (assoc (reverse loc) highlight-locs)])
              (format
                ; omit parens if parent contex has lower precedence
                (if (parens-< parens self-paren-level)
                  "~a"
                  "\\left(~a\\right)")
                (if hl-loc
                  (apply-converter highlight-template texed-args (cdr hl-loc))
                  (apply-converter template texed-args))))])]))))

; TODO probably a better way to write this wrapper using
;      make-keyword-procedure and keyword-apply
(define (texify-expr expr
                     #:loc [color-loc #f]
                     #:color [color "red"]
                     #:highlight-ops [highlight-locs '()])
  (texify-prog (expr->prog expr)
               #:loc color-loc
               #:color color
               #:highlight-ops highlight-locs))

(define (exact-rational? r)
  (and (rational? r) (exact? r)))
