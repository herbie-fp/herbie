#lang racket
(require "../common.rkt")
(require "../syntax/syntax.rkt")
(require "../programs.rkt")

(provide mathjax-url texify-expr texify-prog)

(define mathjax-url
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML")

(define/match (texify-variable var)
  [('l)       "\\ell"]
  [('eps)     "\\varepsilon"]
  [('epsilon) "\\varepsilon"]
  [('alpha)   "\\alpha"]
  [('beta)    "\\beta"]
  [('gamma)   "\\gamma"]
  [('phi)     "\\phi"]
  [('phi1)    "\\phi_1"]
  [('phi2)    "\\phi_2"]
  [('lambda)  "\\lambda"]
  [('lambda1) "\\lambda_1"]
  [('lambda2) "\\lambda_2"]
  [(_) (symbol->string var)])

; "enclose" is a MathJax extension which may
; not work with standard TeX processors.
(define (tag str idx)
  (let* ([enc (format "\\enclose{circle}{~a}" str)]
         [col (format "\\color{red}{~a}" enc)]
         [css (format "\\class{location}{\\cssId{~a}{~a}}" idx col)])
    css))

(define (untag str)
  (format "\\color{black}{~a}" str))

; self-paren-level : #t --> paren me
;                    #f --> do not paren me
;
; args-paren-level : #t --> do not paren args
;                    #f --> paren args
(define precedence-ordering '(#t + * fn #f))

(define (precedence< a b)
  (< (index-of precedence-ordering a)
     (index-of precedence-ordering b)))

(define (precedence-levels op)
  (match op
    [(or '+ '- 'or 'complex) (values '+ '+)]
    [(or '* 'and) (values '* '*)]
    ['/ (values #f #t)]
    [(or 'sqr 'cube 'fma 'hypot 'pow) (values #f #f)]
    ['atan2 (values 'fn #t)]
    ['log1p (values #f '+)]
    ['if (values #t #t)]
    [(or 'remainder 'fmod) (values #t #f)]
    [(or 'cbrt 'ceil 'copysign 'expm1 'exp2 'floor 'fmax 'exp 'sqrt 'fmin 'fabs 'fdim)
     (values #f #t)]
    [(or '== '< '> '<= '>= '!=)
     (values #f #t)]
    [_ (values 'fn #f)]))

(define ((highlight-template op) idx args)
  (define to-tag-infix
    #hash((+ . "+") (- . "-") (* . "\\cdot") (fmod . "\\bmod") (remainder . "\\mathsf{rem}")
          (< . "\\lt") (> . "\\gt") (== . "=") (!= . "\\ne") (<= . "\\le") (>= . "\\ge")
          (and . "\\land") (or . "\\lor")))
  (cond
   [(and (equal? (length args) 2) (hash-has-key? to-tag-infix op))
    (match-define (list a b) args)
    (format "~a ~a ~a" a (tag (hash-ref to-tag-infix op) idx) b)]
   [(equal? op 'if)
    (match-define (list a b c) args)
    (format "~a ~a ~a : ~a" a (tag "?" idx) b c)]
   [(equal? op 'sqr)
    (match-define (list a) args)
    (format "{~a}^{~a}" a (tag "2" idx))]
   [(equal? op 'cube)
    (match-define (list a) args)
    (format "{~a}^{~a}" a (tag "3" idx))]
   [else
    (tag (apply (operator-info op '->tex) (map untag args)) idx)]))

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
            (define num
              (if (equal? significand "1")
                  (format "10^{~a}" exp)
                  (format "~a \\cdot 10^{~a}" significand exp)))
            (if (precedence< parens #f) num (format "\\left( ~a \\right)" num))])]
        [(? constant?)
         (constant-info expr '->tex)]
        [(? symbol?) (texify-variable expr)]
        [`(if ,cond ,ift ,iff)
         (define NL "\\\\\n")
         (define IND "\\;\\;\\;\\;")
         (with-output-to-string
           (λ ()
             (printf "\\begin{array}{l}\n")
             (for ([branch (collect-branches expr loc)])
               (match branch
                 [(list #t bexpr bloc)
                  (printf "\\mathbf{else}:~a~a~a~a\n"
                          NL IND (texify bexpr #t (cons 2 bloc)) NL)]
                 [(list bcond bexpr bloc)
                  (printf "\\mathbf{if}\\;~a:~a~a~a~a\n"
                          (texify bcond #t (cons 1 bloc))
                          NL IND (texify bexpr #t (cons 2 bloc)) NL)]))
             (printf "\\end{array}")))]
        [`(,f ,args ...)
         (define-values (self-paren-level arg-paren-level) (precedence-levels f))
         (let ([texed-args
                (for/list ([arg args] [id (in-naturals 1)])
                  (texify arg arg-paren-level (cons id loc)))]
               [hl-loc
                (assoc (reverse loc) highlight-locs)])
           (format
                                        ; omit parens if parent contex has lower precedence
            (if (precedence< parens self-paren-level)
                "~a"
                "\\left(~a\\right)")
            (if hl-loc
                ((highlight-template f) (cdr hl-loc) texed-args)
                (apply (operator-info f '->tex) texed-args))))]))))

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

