#lang racket
(require math/bigfloat)
(provide js-tex-include texify-expr texify-prog)

(define js-tex-include
  '((link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css"]
           [integrity "sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH"]
           [crossorigin "anonymous"]))
    (script ([src "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js"]
             [integrity "sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm"]
             [crossorigin "anonymous"]))
    (script ([src "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/contrib/auto-render.min.js"]
             [integrity "sha384-aGfk5kvhIq5x1x5YdvCp4upKZYnA8ckafviDpmWEKp4afOZEqOli7gqSnh8I6enH"]
             [crossorigin "anonymous"]))))

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
    ['pow (values #f #f)]
    ['atan2 (values 'fn #t)]
    ['if (values #t #t)]
    [(or 'remainder 'fmod) (values #t #f)]
    [(or 'cbrt 'ceil 'copysign 'exp2 'floor 'fmax 'exp 'sqrt 'fmin 'fabs 'fdim  'expm1 'fma 'log1p 'hypot 'j0 'j1 'y0 'y1 'lgamma 'tgamma 'trunc)
     (values #f #t)]
    [(or '== '< '> '<= '>= '!=)
     (values #f #t)]
    [_ (values 'fn #f)]))

(define (collect-branches expr loc)
  (match expr
    [`(if ,cond ,ift ,iff)
     (cons (list cond ift loc)
           (collect-branches iff (cons 3 loc)))]
    [else
     (list (list #t expr loc))]))

(define (texify-prog expr #:loc [color-loc #f] #:color [color "red"])
  (texify-expr (program-body expr) #:loc color-loc #:color color))

(define (texify-expr expr #:loc [color-loc #f] #:color [color "red"])
  "Compile an expression to math mode TeX."
  (let texify ([expr expr] [parens #t] [loc '(2)])
    (format
      (if (and color-loc (equal? (reverse color-loc) loc))
        (format "\\color{~a}{~~a}" color)
        "~a")
      (match expr
        [(? exact-integer?)
         (number->string expr)]
        [(and (? rational?) (? exact?))
         (format "\\frac{~a}{~a}" (numerator expr) (denominator expr))]
        [(? (conjoin complex? (negate real?)))
         (format "~a ~a ~a i"
                 (texify (real-part expr) '+ loc)
                 (if (or (< (imag-part expr) 0) (equal? (imag-part expr) -0.0)) '- '+)
                 (texify (abs (imag-part expr)) '+ loc))]
        [(? value?)
         (define s (bigfloat->string ((representation-repr->bf (infer-representation expr)) expr)))
         (match (string-split s "e")
           [(list "-inf.bf") "-\\infty"]
           [(list "+inf.bf") "+\\infty"]
           [(list num) num]
           [(list significand exp)
            (define num
              (if (equal? significand "1")
                  (format "10^{~a}" exp)
                  (format "~a \\cdot 10^{~a}" significand exp)))
            (if (precedence< parens #f) num (format "\\left( ~a \\right)" num))])]
<<<<<<< HEAD
        [(? constant?)
=======
        [(? complex?)
         (format "~a ~a ~a i"
                 (texify (real-part expr) '+ loc)
                 (if (or (< (imag-part expr) 0) (equal? (imag-part expr) -0.0)) '- '+)
                 (texify (abs (imag-part expr)) '+ loc))]
        [(? constant-or-num?)
>>>>>>> Changed constant? to the updated constant-or-num
         (constant-info expr '->tex)]
        [(? symbol?) (texify-variable expr)]
        [`(if ,cond ,ift ,iff)
         (define NL "\\\\\n")
         (define IND "\\;\\;\\;\\;")
         (with-output-to-string
           (λ ()
             (printf "\\begin{array}{l}\n")
             (for ([branch (collect-branches expr loc)] [n (in-naturals)])
               (match branch
                 [(list #t bexpr bloc)
                  (printf "\\mathbf{else}:~a~a~a~a\n"
                          NL IND (texify bexpr #t (cons 2 bloc)) NL)]
                 [(list bcond bexpr bloc)
                  (printf "\\mathbf{~a}\\;~a:~a~a~a~a\n"
                          (if (= n 0) "if" "elif")
                          (texify bcond #t (cons 1 bloc))
                          NL IND (texify bexpr #t (cons 2 bloc)) NL)]))
             (printf "\\end{array}")))]
        [`(<= ,x -inf.0)
         (texify `(== ,x -inf.0) parens loc)]
        [`(,f ,args ...)
         (define-values (self-paren-level arg-paren-level) (precedence-levels f))
         (let ([texed-args
                (for/list ([arg args] [id (in-naturals 1)])
                  (texify arg arg-paren-level (cons id loc)))]
               [hl-loc
                #f])
           (format ; omit parens if parent contex has lower precedence
            (if (precedence< parens self-paren-level) "~a" "\\left(~a\\right)")
            (apply (operator-info f '->tex) texed-args)))]))))

