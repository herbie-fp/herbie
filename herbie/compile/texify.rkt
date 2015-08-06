#lang racket
(require "../common.rkt")
(require "../syntax.rkt")
(require "../programs.rkt")
(require "../main.rkt")

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
   [(list? conv) (apply format (list-ref conv (min 2 (length args))) args)]
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
  [mod      "~a \\modb ~a" #t #f]
  [length   "\\textsf{length}(~a)" #f #f]
  [cdr      "\\textsf{tail}(~a)" #f #f]
  [car      "\\textsf{head}(~a)" #f #f])

(define parens-precedence '(#t + * fn #f))

(define (parens-< a b)
  (let loop ([l parens-precedence])
    (cond
     [(and (eq? (car l) a) (eq? (car l) b)) #f]
     [(eq? (car l) a) #t]
     [(eq? (car l) b) #f]
     [else (loop (cdr l))])))

(define (list->string items)
  (string-append
   "["
   (if ((length items) . > . 1)
       (apply
	string-append
	(car items)
	(for/list ([item items])
	  (string-append ", " item)))
       "")
   "]"))
   

(define (texify-expression expr [parens #t])
  "Compile an expression to TeX code.
   The TeX is intended to be used in math mode.

   `parens` is one of #f, '+, '*, 'fn, or #t"
  (define (render-func func expr)
    (match func
      [`(,f ,args ...)
       (if (eq? f 'list) (list->string args)
	   (match (hash-ref texify-operators f)
	     [`(,template ,self-paren-level ,arg-paren-level)
              (apply-converter
	       template
	       (for/list ([arg args] [earg (cdr expr)])
		 (match earg
		   [`(,child-f ,child-args ...)
		    (if (eq? f 'list) arg
			(match-let ([`(,_ ,child-paren-level ,_)
				     (hash-ref texify-operators child-f)])
			  (if (parens-< child-paren-level arg-paren-level)
			      (format "\\left(~a\\right)" arg)
			      arg)))]
		   [_ arg])))]))]))
  (define (render-assigns vars vals [indent ""])
    (apply
     string-append
     (for/list ([var vars] [val vals])
       (string-append indent
                      (format "~a \\gets ~a\\\\"
                              (symbol->string var) val)))))
  (expression-induct*
   (unfold-lets-expr expr)
   #:constant (λ (const* const)
                (if (number? const)
                    (number->string const)
                    (car (hash-ref texify-constants const))))
   #:variable (λ (v* v) (symbol->string v*))
   #:primitive render-func
   #:predicate render-func
   #:let (λ (lt* lt)
           (error "there shouldn't be any lets anymore"))
   #:loop (λ (lp* lp)
            (match lp*
              [`(do ([,accs ,inits ,updates] ...)
                    ,while-expr
                  ,ret-expr)
               (string-append
                (render-assigns accs inits)
                "\\textsf{while $" while-expr "$}\\\\"
                (render-assigns accs updates "\\hspace{3em}")
                "\\textsf{return }" ret-expr)]
              [`(do-list ([,accs ,inits ,updates] ...)
                         ([,items ,lsts] ...)
                         ,ret-expr)
               (string-append
                (render-assigns accs inits)
                
                "\\textsf{for "
                (apply
                 string-append
                 (for/list ([item items] [lst lsts]
                            [idx (in-range (length items))])
                   (string-append "$" (symbol->string item) "$"
                                  " in "
                                  "$" lst "$"
                                  (if (< idx (sub1 (length items))) "," ""))))
                ":}\\\\"
                (render-assigns accs updates "\\hspace{3em}")
                "\\textsf{return }" ret-expr)]))))
