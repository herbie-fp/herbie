#lang racket

(require racket/runtime-path math/base)
(require "config.rkt" "debug.rkt")
(module+ test (require rackunit))

(provide reap define-table table-ref table-set! table-remove!
         call-with-output-files
         take-up-to flip-lists list/true find-duplicates
         argmins argmaxs index-of set-disjoint? comparator
         parse-flag get-seed set-seed!
         quasisyntax syntax-e* dict sym-append
         format-time format-bits in-sorted-dict web-resource
         (all-from-out "config.rkt") (all-from-out "debug.rkt"))

;; Various syntactic forms of convenience used in Herbie

(define-syntax-rule (reap [sows ...] body ...)
  (let* ([sows (let ([store '()])
                 (cons
                  (λ () store)
                  (λ (elt) (set! store (cons elt store)))))] ...)
    (let ([sows (cdr sows)] ...)
      body ...)
    (values (reverse ((car sows))) ...)))

;; The new, contracts-using version of the above

(define-syntax-rule (define-table name [field type] ...)
  (define name (cons (list (cons 'field type) ...) (make-hash))))

(define (table-ref tbl key field)
  (match-let ([(cons header rows) tbl])
    (for/first ([(field-name type) (in-dict header)]
                [value (in-list (hash-ref rows key))]
                #:when (equal? field-name field))
      value)))

(define (table-set! tbl key fields)
  (match-let ([(cons header rows) tbl])
    (define row (for/list ([(hkey htype) (in-dict header)]) (dict-ref fields hkey)))
    (hash-set! rows key row)))

(define (table-remove! tbl key)
  (hash-remove! (cdr tbl) key))

;; Utility list functions

(define (take-up-to l k)
  (for/list ([x l] [i (in-range k)])
    x))

(module+ test
  (check-equal? (take-up-to '(a b c d e f) 3) '(a b c))
  (check-equal? (take-up-to '(a b) 3) '(a b)))

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

(define (list/true . args)
  (filter identity args))

(module+ test
  (check-equal? (flip-lists '((1 2 3) (4 5 6) (7 8 9)))
                '((1 4 7) (2 5 8) (3 6 9))))

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

(define (parse-flag s)
  (match (string-split s ":")
    [(list (app string->symbol category) (app string->symbol flag))
     (and
      (dict-has-key? all-flags category)
      (set-member? (dict-ref all-flags category) flag)
      (list category flag))]
    [_ #f]))

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

(define (in-sorted-dict d #:key [key identity])
  (in-dict (sort (dict->list d) > #:key key)))

(define-runtime-path web-resource-path "web/")

(define (web-resource [name #f])
  (if name
      (build-path web-resource-path name)
      web-resource-path))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (syntax-e* stx)
  (match (syntax-e stx)
    [(list elems ...) (map syntax-e* elems)]
    [stx* stx*]))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~a args))))
