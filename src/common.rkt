#lang racket

(require racket/runtime-path math/base)
(require "config.rkt" "debug.rkt")
(module+ test (require rackunit))

(provide reap ->float32
         call-with-output-files
         flip-lists find-duplicates partial-sums
         argmins argmaxs index-of set-disjoint?
         get-seed set-seed!
         quasisyntax dict sym-append comparator
         format-time format-bits web-resource
         debug ; from debug.rkt
         (all-from-out "config.rkt"))

;; Various syntactic forms of convenience used in Herbie

(define-syntax-rule (reap [sows ...] body ...)
  (let* ([sows (let ([store '()])
                 (cons
                  (λ () store)
                  (match-lambda*
                   [(list elt) (set! store (cons elt store))]
                   [(list) store]
                   [_ (error 'reap "invalid sow")])))] ...)
    (let ([sows (cdr sows)] ...)
      body ...)
    (values (reverse ((car sows))) ...)))

(define cast-single
  (let ([flsingle identity])
    (local-require racket/flonum)
    flsingle))

(define (->float32 x)
  (if (>= (string->number (substring (version) 0 1)) 8)
      (cast-single (exact->inexact x))
      (real->single-flonum x)))

;; Utility list functions

(define (argmins f lst)
  (let loop ([lst lst] [best-score #f] [best-elts '()])
    (if (null? lst)
        (reverse best-elts)
        (let* ([elt (car lst)] [lst* (cdr lst)] [score (f elt)])
          (cond
           [(not best-score)
            (loop lst* score (list elt))]
           [(< score best-score)
            (loop lst* score (list elt))]
           [(> score best-score)
            (loop lst* best-score best-elts)]
           [(= score best-score)
            (loop lst* best-score (cons elt best-elts))])))))

(module+ test
  (check-equal? (argmins string-length '("a" "bb" "f" "ccc" "dd" "eee" "g"))
                '("a" "f" "g")))

(define (argmaxs f lst)
  (argmins (λ (x) (- (f x))) lst))

(module+ test
  (check-equal? (argmaxs string-length '("a" "bb" "f" "ccc" "dd" "eee" "g"))
                '("ccc" "eee")))

(define (flip-lists list-list)
  "Flip a list of rows into a list of columns"
  (apply map list list-list))

(module+ test
  (check-equal? (flip-lists '((1 2 3) (4 5 6) (7 8 9)))
                '((1 4 7) (2 5 8) (3 6 9))))

(define (partial-sums vec)
  (define res (make-vector (vector-length vec)))
  (for/fold ([cur-psum 0]) ([(el idx) (in-indexed (in-vector vec))])
    (let ([new-psum (+ cur-psum el)])
      (vector-set! res idx new-psum)
      new-psum))
  res)

(module+ test
  (check-equal? (partial-sums #(1 4 6 3 8)) #(1 5 11 14 22)))

(define (find-duplicates l)
  (define found (mutable-set))
  (define duplicates '())
  (for ([x l])
    (when (set-member? found x)
      (set! duplicates (cons x duplicates)))
    (set-add! found x))
  (reverse duplicates))

(define (index-of lst elt)
  (for/first ([e lst] [i (in-naturals)]
             #:when (equal? e elt))
             i))

(module+ test
  (check-equal? (index-of '(a b c d e) 'd) 3)
  (check-equal? (index-of '(a b c d e) 'foo) #f))

(define (set-disjoint? s1 s2)
  (set-empty? (set-intersect s2 s1)))

(module+ test
  (check-true (set-disjoint? '(a b c) '(e f g)))
  (check-true (set-disjoint? '() '()))
  (check-false (set-disjoint? '(a b c) '(a))))

;; Miscellaneous helper

(define the-seed #f)

(define (get-seed)
  (or the-seed (error "Seed is not set yet!")))

(define (set-seed! seed)
  "Reset the random number generator to a new seed"
  (set! the-seed seed)
  (if (vector? seed)
      (current-pseudo-random-generator
       (vector->pseudo-random-generator seed))
      (random-seed seed)))

;; Matching support for syntax objects.

;; Begin the match with a #`
;; Think of the #` as just like a ` match, same behavior
;; In fact, matching x with #`pat should do the same
;; as matching (syntax->datum x) with `pat
;; Inside the #`, you can use #, to bind not a value but a syntax object.

(define-match-expander quasisyntax
  (λ (stx)
    (syntax-case stx (unsyntax unquote)
      [(_ (unsyntax pat))
       #'pat]
      [(_ (unquote pat))
       #'(app syntax-e pat)]
      [(_ (pats ...))
       (let ([parts
              (for/list ([pat (syntax-e #'(pats ...))])
                (syntax-case pat (unsyntax unquote ...)
                  [... pat]
                  [(unsyntax a) #'a]
                  [(unquote a) #'(app syntax-e a)]
                  [a #'(quasisyntax a)]))])
         #`(app syntax-e #,(datum->syntax stx (cons #'list parts))))]
      [(_ a)
       #'(app syntax-e 'a)])))

(define-match-expander dict
  (λ (stx)
    (syntax-case stx (quote)
      [(_)
       #'(? dict?)]
      [(dict 'x y rest ...)
       #'(and (dict rest ...) (? (curryr dict-has-key? 'x)) (app (curryr dict-ref 'x) y))])))

;; String formatting operations

(define (format-time ms #:min [min-unit 0])
  (cond
   [(< (max ms min-unit) 1000) (format "~ams" (round ms))]
   [(< (max ms min-unit) 60000) (format "~as" (/ (round (/ ms 100.0)) 10))]
   [(< (max ms min-unit) 3600000) (format "~amin" (/ (round (/ ms 6000.0)) 10))]
   [else (format "~ahr" (/ (round (/ ms 360000.0)) 10))]))

(define (format-bits r #:sign [sign #f] #:unit [unit? #f])
  (define unit (if unit? "b" ""))
  (cond
   [(not r) ""]
   [(and (> r 0) sign) (format "+~a~a" (/ (round (* r 10)) 10) unit)]
   [else (format "~a~a" (/ (round (* r 10)) 10) unit)]))

(define (call-with-output-files names k)
  (let loop ([names names] [ps '()])
    (if (null? names)
        (apply k (reverse ps))
        (if (car names)
            (call-with-output-file
                (car names) #:exists 'replace
                (λ (p) (loop (cdr names) (cons p ps))))
            (loop (cdr names) (cons #f ps))))))

(define-runtime-path web-resource-path "web/resources/")

(define (web-resource [name #f])
  (if name
      (build-path web-resource-path name)
      web-resource-path))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~a args))))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))
