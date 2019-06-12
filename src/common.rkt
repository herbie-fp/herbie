#lang racket

(require math/flonum math/bigfloat racket/runtime-path)
(require "config.rkt" "errors.rkt" "debug.rkt" "syntax/softposit.rkt"
         "interface.rkt")
(module+ test (require rackunit))

(provide *start-prog* *all-alts*
         reap define-table table-ref table-set! table-remove!
         assert for/append string-prefix call-with-output-files
         take-up-to flip-lists list/true find-duplicates all-partitions
         argmins argmaxs setfindf index-of set-disjoint? comparator
         write-file write-string
         random-exp parse-flag get-seed set-seed!
         common-eval quasisyntax
         format-time format-bits when-dict in-sorted-dict web-resource
         (all-from-out "config.rkt") (all-from-out "debug.rkt"))

;; A useful parameter for many of Herbie's subsystems, though
;; ultimately one that should be located somewhere else or perhaps
;; exorcised

(define *start-prog* (make-parameter '()))
(define *all-alts* (make-parameter '()))

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

;; More various helpful values

(define-syntax assert
  (syntax-rules ()
    [(assert pred #:loc location)
     (when (not pred)
       (error location "~a returned false!" 'pred))]
    [(assert pred)
     (when (not pred)
       (error 'assert "~a returned false!" 'pred))]))

(define-syntax-rule (for/append (defs ...)
                                bodies ...)
  (apply append
         (for/list (defs ...)
           bodies ...)))

(module+ test
  (check-equal? (for/append ([v (in-range 5)]) (list v v v))
                '(0 0 0 1 1 1 2 2 2 3 3 3 4 4 4)))

;; Utility list functions

(define (take-up-to l k)
  (for/list ([x l] [i (in-range k)])
    x))

(define (string-prefix s length)
  (if (<= (string-length s) length)
      s
      (substring s 0 length)))

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

(define (setfindf f s)
  (for/first ([elt (in-set s)] #:when (f elt))
    elt))

(module+ test
  (check-equal? (setfindf positive? (set -3 6 0)) 6))

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

;; Utility output functions

(define-syntax-rule (write-file filename . rest)
   (with-output-to-file filename (lambda () . rest) #:exists 'replace))

(define-syntax-rule (write-string . rest)
  (with-output-to-string (lambda () . rest)))

;; Miscellaneous helper

(define (random-exp k)
  "Like (random (expt 2 k)), but k is allowed to be arbitrarily large"
  (if (< k 31) ; Racket generates random numbers in the range [0, 2^32-2]; I think it's a bug
      (random (expt 2 k))
      (let ([head (* (expt 2 31) (random-exp (- k 31)))])
        (+ head (random (expt 2 31))))))

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

;; Common namespace for evaluation

(define-namespace-anchor common-eval-ns-anchor)
(define common-eval-ns (namespace-anchor->namespace common-eval-ns-anchor))
(define (common-eval expr) (eval expr common-eval-ns))

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

;; String formatting operations

(define (format-time ms)
  (cond
   [(< ms 1000) (format "~ams" (round ms))]
   [(< ms 60000) (format "~as" (/ (round (/ ms 100.0)) 10))]
   [(< ms 3600000) (format "~am" (/ (round (/ ms 6000.0)) 10))]
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

(define-syntax-rule (when-dict d (arg ...) body ...)
  (if (and (dict-has-key? d 'arg) ...)
      (let ([arg (dict-ref d 'arg)] ...)
        body ...)
      '()))

(define (in-sorted-dict d #:key [key identity])
  (in-dict (sort (dict->list d) > #:key (compose key cdr))))

(define-runtime-path web-resource-path "web/")

(define (web-resource [name #f])
  (if name
      (build-path web-resource-path name)
      web-resource-path))

(define (all-partitions n #:from [k 1])
  (cond
   [(= n 0) '(())]
   [(< n k) '()]
   [else
    (append (map (curry cons k) (all-partitions (- n k) #:from k))
            (all-partitions n #:from (+ k 1)))]))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))
