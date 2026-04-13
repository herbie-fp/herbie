#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt"
         "../syntax/platform.rkt"
         "../core/alternative.rkt"
         "../core/programs.rkt"
         "../utils/common.rkt"
         "../config.rkt")

(provide sollya-minimax-alts
         expr->sollya
         run-sollya-fpminimax)

(define sollya-path (find-executable-path "sollya"))

(define (expr->sollya expr var)
  (define (recur e)
    (match e
      [(== var) "x"]
      [(? symbol?) #f]
      [(? exact-integer?) (number->string e)]
      [(? rational?) (format "(~a/~a)" (numerator e) (denominator e))]
      [(? real?) (number->string (real->double-flonum e))]
      [(list 'PI) "pi"]
      [(list 'E) "(exp(1))"]
      [(list 'neg a)
       (define sa (recur a))
       (and sa (format "(-(~a))" sa))]
      [(list '+ a b) (binop "+" a b)]
      [(list '- a b) (binop "-" a b)]
      [(list '* a b) (binop "*" a b)]
      [(list '/ a b) (binop "/" a b)]
      [(list 'pow base (? exact-integer? n))
       (define sb (recur base))
       (and sb (format "(~a)^(~a)" sb n))]
      [(list 'pow base power) (binop "^" base power)]
      [(list (and op
                  (or 'sqrt
                      'exp
                      'log
                      'sin
                      'cos
                      'tan
                      'asin
                      'acos
                      'atan
                      'sinh
                      'cosh
                      'tanh
                      'erf
                      'erfc
                      'expm1
                      'log1p
                      'asinh
                      'acosh
                      'atanh))
             a)
       (define sa (recur a))
       (and sa (format "~a(~a)" op sa))]
      [(list 'cbrt a)
       (define sa (recur a))
       (and sa (format "(~a)^(1/3)" sa))]
    ;;;   [(list 'fabs a)
    ;;;    (define sa (recur a))
    ;;;    (and sa (format "abs(~a)" sa))]
      [(list 'log2 a)
       (define sa (recur a))
       (and sa (format "(log(~a)/log(2))" sa))]
      [(list 'log10 a)
       (define sa (recur a))
       (and sa (format "(log(~a)/log(10))" sa))]
      [(list 'hypot a b)
       (define sa (recur a))
       (define sb (recur b))
       (and sa sb (format "sqrt((~a)^2 + (~a)^2)" sa sb))]
      [(list 'fma a b c)
       (define sa (recur a))
       (define sb (recur b))
       (define sc (recur c))
       (and sa sb sc (format "((~a) * (~a) + (~a))" sa sb sc))]
      [_ #f]))
  (define (binop op a b)
    (define sa (recur a))
    (define sb (recur b))
    (and sa sb (format "(~a ~a ~a)" sa op sb)))
  (recur expr))

(define (parse-sollya-number s)
  (define n (string->number (string-trim s)))
  (and n (real->double-flonum n)))

(define (parse-sollya-poly output var)
  (define trimmed (string-trim output))
  (when (or (string=? trimmed "")
            (string=? trimmed "error")
            (regexp-match? #rx"(?i:nan|\\binf\\b|warning)" trimmed))
    (error 'parse-sollya-poly "invalid output"))
  (let parse ([s trimmed])
    (define m (regexp-match-positions #rx" \\+ x \\* " s))
    (cond
      [m
       (define sep-start (car (car m)))
       (define sep-end (cdr (car m)))
       (define coeff-str (substring s 0 sep-start))
       (define rest-raw (substring s sep-end))
       (define rest-str
         (if (and (string-prefix? rest-raw "(") (string-suffix? rest-raw ")"))
             (substring rest-raw 1 (sub1 (string-length rest-raw)))
             rest-raw))
       (define coeff (parse-sollya-number coeff-str))
       (unless coeff
         (error 'parse-sollya-poly "cannot parse coefficient: ~a" coeff-str))
       `(+ ,coeff (* ,var ,(parse rest-str)))]
      [else
       (define n (parse-sollya-number s))
       (unless n
         (error 'parse-sollya-poly "cannot parse constant: ~a" s))
       n])))

(define (run-sollya-fpminimax sollya-expr degree lo hi var)
  (define script
    (format
     "verbosity=0!;\ndisplay=decimal!;\np = fpminimax(~a, ~a, [|DD...|], [~a + 1e-50, ~a + 1e-50]);\nprint(p);\nquit;\n"
     sollya-expr
     degree
     (number->string (exact->inexact lo))
     (number->string (exact->inexact hi))))
  (printf "~a, ~a, ~a, ~a\n" sollya-expr degree lo hi)
  (define cust (make-custodian))
  (define output
    (with-handlers ([exn:fail? (λ (e) #f)])
      (define result
        (parameterize ([current-custodian cust])
          (define-values (proc out in err) (subprocess #f #f #f sollya-path "--warnonstderr"))
          (display script in)
          (close-output-port in)
          (thread (λ () (void (port->string err))))
          (define ch (make-channel))
          (thread (λ () (channel-put ch (port->string out))))
          (sync/timeout 2.0 ch)))
      (custodian-shutdown-all cust)
      result))
  (when (not output)
    (custodian-shutdown-all cust))
  (printf "~a\n" output)
  (and output
       (with-handlers ([exn:fail? (λ (e) #f)])
         (parse-sollya-poly output var))))

(define (polynomial-expr? expr var)
  (match expr
    [(== var) #t]
    [(? number?) #t]
    [(list (or '+ '- '*) a b) (and (polynomial-expr? a var) (polynomial-expr? b var))]
    [(list 'neg a) (polynomial-expr? a var)]
    [(list 'pow base (? exact-integer?)) (polynomial-expr? base var)]
    [_ #f]))

(define (sollya-minimax-alts altns global-batch regime-intervals)
  (cond
    [(or (not sollya-path) (hash-empty? regime-intervals)) '()]
    [else
     (define ctx (*context*))
     (define vars
       (for/list ([var (in-list (context-vars ctx))]
                  #:when (equal? (representation-type (context-lookup ctx var)) 'real))
         var))
     (define brfs (map alt-expr altns))
     (define reprs (map (batch-reprs global-batch ctx) brfs))
     (define spec-brfs (batch-to-spec! global-batch brfs))
     (define exprs-fn (batch-exprs global-batch))
     (define free-vars-fn (batch-free-vars global-batch))
     (define sollya-cache (make-hash))

     (reap [sow]
           (define max-width 128)
           (for ([var (in-list vars)])
             (define intervals (hash-ref regime-intervals var '()))
             (define seen-sollya (make-hash))
             (for ([interval (in-list intervals)])
               (match-define (cons lo hi) interval)
               (when (and (< lo hi) (<= (- hi lo) max-width))
                 (for ([spec-brf (in-list spec-brfs)]
                       [repr (in-list reprs)]
                       [altn (in-list altns)]
                       #:when (not (array-representation? repr))
                       #:do [(define fv (free-vars-fn spec-brf))]
                       #:when (and (= (set-count fv) 1) (set-member? fv var)) ; Univariate
                       #:do [(define spec-expr (exprs-fn spec-brf))]
                       #:when (pair? spec-expr)
                       #:when (not (polynomial-expr? spec-expr var)) ; Filter out polynomials
                       #:do [(define sollya-str (expr->sollya spec-expr var))]
                       #:when sollya-str ; Check if the expression can be lowered to Sollya
                       #:unless (hash-has-key? seen-sollya (cons sollya-str interval)))
                   (hash-set! seen-sollya (cons sollya-str interval) #t)
                   (eprintf "[minimax] expr=~a interval=[~a,~a]\n" spec-expr lo hi)
                   (for ([degree (in-range 1 (add1 (* 2 (*taylor-order-limit*))))])
                     (define key (list sollya-str degree lo hi))
                     (define poly-expr
                       (hash-ref! sollya-cache
                                  key
                                  (λ () (run-sollya-fpminimax sollya-str degree lo hi var))))
                     (when poly-expr
                       (define poly-brf (batch-add! global-batch poly-expr))
                       (define gen (approx spec-brf (hole (representation-name repr) poly-brf)))
                       (define brf (batch-add! global-batch gen))
                       (sow (alt brf `(taylor minimax ,var) (list altn))))))))))]))
