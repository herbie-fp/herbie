#lang racket

(require math/base
         "../config.rkt")

(provide reap
         drop-at
         find-duplicates
         partial-sums
         get-seed
         set-seed!
         quasisyntax
         sym-append
         format-time
         format-bits
         prop-dict/c
         props->dict
         fpcore->string
         (all-from-out "../config.rkt"))

(module+ test
  (require rackunit))

;; Various syntactic forms of convenience used in Herbie

(define-syntax-rule (reap [sows ...] body ...)
  (let* ([sows (let ([store '()])
                 (cons (λ () store)
                       (match-lambda*
                         [(list elt) (set! store (cons elt store))]
                         [(list) store]
                         [_ (error 'reap "invalid sow")])))] ...)
    (let ([sows (cdr sows)] ...)
      body ...)
    (values (reverse ((car sows))) ...)))

(define (drop-at ls index)
  (define-values (front back) (split-at ls index))
  (append front (rest back)))

(define (partial-sums vec)
  (define res (make-vector (vector-length vec)))
  (for/fold ([cur-psum 0]) ([(el idx) (in-indexed (in-vector vec))])
    (define new-psum (+ cur-psum el))
    (vector-set! res idx new-psum)
    new-psum)
  res)

(module+ test
  (check-equal? (partial-sums #(1 4 6 3 8)) #(1 5 11 14 22)))

(define (find-duplicates l)
  (map car (filter (compose pair? rest) (group-by identity l))))

;; Matching support for syntax objects.

;; Begin the match with a #`
;; Think of the #` as just like a ` match, same behavior
;; In fact, matching x with #`pat should do the same
;; as matching (syntax->datum x) with `pat
;; Inside the #`, you can use #, to bind not a value but a syntax object.

(define-match-expander quasisyntax
  (λ (stx)
    (syntax-case stx (unsyntax unquote)
      [(_ (unsyntax pat)) #'pat]
      [(_ (unquote pat)) #'(app syntax-e pat)]
      [(_ (pats ...))
       (let ([parts (for/list ([pat (syntax-e #'(pats ...))])
                      (syntax-case pat (unsyntax unquote ...)
                        [... pat]
                        [(unsyntax a) #'a]
                        [(unquote a) #'(app syntax-e a)]
                        [a #'(quasisyntax a)]))])
         #`(app syntax-e #,(datum->syntax stx (cons #'list parts))))]
      [(_ a) #'(app syntax-e 'a)])))

;; String formatting operations

(define (format-time ms)
  (cond
    [(< ms 1000) (format "~ams" (round ms))]
    [(< ms 60000) (format "~as" (/ (round (/ ms 100.0)) 10))]
    [(< ms 3600000) (format "~amin" (/ (round (/ ms 6000.0)) 10))]
    [else (format "~ahr" (/ (round (/ ms 360000.0)) 10))]))

(module+ test
  (check-equal? (format-time 60000) "1.0min")
  (check-equal? (format-time 3600000) "1.0hr")
  (check-equal? (format-time 500) "500ms")
  (check-equal? (format-time 2000) "2.0s")
  (check-equal? (format-time 0) "0ms")
  (check-equal? (format-time 1800000) "30.0min")
  (check-equal? (format-time 7200000) "2.0hr"))

(define (format-bits r #:unit [unit? #f])
  (define unit (if unit? "b" ""))
  (cond
    [(not r) ""]
    [else (format "~a~a" (/ (round (* r 10)) 10) unit)]))

;; Symbol generation

(define (sym-append . args)
  (string->symbol (apply string-append (map ~a args))))

;; FPCore properties

(define prop-dict/c (listof (cons/c symbol? any/c)))

;; Prop list to dict
(define (props->dict props)
  (let loop ([props props]
             [dict '()])
    (match props
      [(list key val rest ...) (loop rest (dict-set dict key val))]
      [(list key) (error 'props->dict "unmatched key" key)]
      [(list) dict])))

(define (fpcore->string core)
  (define-values (ident args props expr)
    (match core
      [(list 'FPCore name (list args ...) props ... expr) (values name args props expr)]
      [(list 'FPCore (list args ...) props ... expr) (values #f args props expr)]))
  (define props* ; make sure each property (name, value) gets put on the same line
    (for/list ([(prop name) (in-dict (props->dict props))])
      (format "~a ~a" prop (pretty-format name (- 69 (string-length (~a prop))) #:mode 'write))))
  (define top
    (if ident
        (format "FPCore ~a ~a" ident args)
        (format "FPCore ~a" args)))
  (format "(~a\n  ~a\n  ~a)" top (string-join props* "\n  ") (pretty-format expr 70 #:mode 'write)))
