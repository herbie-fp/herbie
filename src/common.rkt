#lang racket

(require math/flonum)
(require math/bigfloat)
(require "config.rkt" "errors.rkt" "debug.rkt")

(module+ test
  (require rackunit))

(provide *start-prog* *all-alts*
         reap define-table table-ref table-set! table-remove!
         assert for/append
         ordinary-value? =-or-nan? </total
         take-up-to flip-lists argmins argmaxs setfindf index-of set-disjoint? all-equal?
         write-file write-string
         binary-search-floats binary-search-ints binary-search
         random-exp parse-flag get-seed set-seed!
         common-eval-ns common-eval quasisyntax
         format-time format-bits
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
  (define/contract name
    (cons/c (listof (cons/c symbol? contract?)) (hash/c symbol? (list/c type ...)))
    (cons (list (cons 'field type) ...) (make-hash))))

(define/contract (table-ref tbl key field)
  (->i ([tbl (cons/c (listof (cons/c symbol? contract?)) (hash/c symbol? (listof any/c)))]
        [key symbol?]
        [field symbol?])
       [_ (tbl field) (dict-ref (car tbl) field)])
  (match-let ([(cons header rows) tbl])
    (for/first ([(field-name type) (in-dict header)]
                [value (in-list (dict-ref rows key))]
                #:when (equal? field-name field))
      value)))

(define/contract (table-set! tbl key fields)
  (->i ([tbl (cons/c (listof (cons/c symbol? contract?)) (hash/c symbol? (listof any/c)))]
        [key symbol?]
        [fields (tbl)
                ;; Don't check value types because the contract gets pretty rough :(
                (and/c dict? (λ (d) (andmap (curry dict-has-key? d) (dict-keys (car tbl)))))])
       any)
  (match-let ([(cons header rows) tbl])
    (define row (for/list ([(hkey htype) (in-dict header)]) (dict-ref fields hkey)))
    (dict-set! rows key row)))

(define/contract (table-remove! tbl key)
  ((cons/c (listof (cons/c symbol? contract?)) (hash/c symbol? (listof any/c))) symbol? . -> . void?)
  (match-let ([(cons header rows) tbl])
    (dict-remove! rows key)))

;; More various helpful values

(define-syntax assert
  (syntax-rules ()
    [(assert pred #:loc location)
     (when (not pred)
       (error location "~a returned false!" 'pred))]
    [(assert pred #:extra-info func)
     (when (not pred)
       (error 'assert (format "~a returned false! Extra info: ~a"
			       'pred (func))))]
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

;; Simple floating-point functions

(define (ordinary-value? x)
  (match x
    [(? real?)
     (not (or (infinite? x) (nan? x)))]
    [(? complex?)
     (and (ordinary-value? (real-part x)) (ordinary-value? (imag-part x)))]
    [(? boolean?)
     true]))

(module+ test
  (check-true (ordinary-value? 2.5))
  (check-false (ordinary-value? +nan.0))
  (check-false (ordinary-value? -inf.f)))

(define (=-or-nan? x1 x2)
  (or (= x1 x2)
      (and (nan? x1) (nan? x2))))

(module+ test
  (check-true (=-or-nan? 2.3 2.3))
  (check-false (=-or-nan? 2.3 7.8))
  (check-true (=-or-nan? +nan.0 -nan.f))
  (check-false (=-or-nan? 2.3 +nan.f)))

(define (</total x1 x2)
  (cond
   [(nan? x1) #f]
   [(nan? x2) #t]
   [else (< x1 x2)]))

;; Utility list functions

(define (take-up-to l k)
  ; This is unnecessarily slow. It is O(l), not O(k).
  ; To be honest, it just isn't that big a deal for now.
  (take l (min k (length l))))

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

(module+ test
  (check-equal? (flip-lists '((1 2 3) (4 5 6) (7 8 9)))
                '((1 4 7) (2 5 8) (3 6 9))))

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

(define (all-equal? l)
  (if (null? l)
      true
      (andmap (curry equal? (car l)) (cdr l))))

;; Utility output functions

(define-syntax-rule (write-file filename . rest)
   (with-output-to-file filename (lambda () . rest) #:exists 'replace))

(define-syntax-rule (write-string . rest)
  (with-output-to-string (lambda () . rest)))

;; Binary search implementation

;; Given two points, the first of which is pred, and the second is not,
;; finds the point where pred becomes false, by calling split to binary
;; search the space until (split a b) returns a, b, or #f.
(define (binary-search split pred p1 p2)
  ;; Get the midpoint using our given split function
  (let ([midpoint (split p1 p2)])
    ;; If the split function returned false, we're done.
    (cond [(not midpoint) p1]
	  ;; If our midpoint is one of our points, we're done.
	  [(or (equal? p1 midpoint) (equal? p2 midpoint)) midpoint]
	  ;; If our predicate is still true of our midpoint, search the
	  ;; space between our midpoint and p2.
	  [(pred midpoint) (binary-search split pred midpoint p2)]
	  ;; Otherwise, search the space between our midpoint and p1.
	  [#t (binary-search split pred p1 midpoint)])))

;; Given two floating point numbers, the first of which is pred,
;; and the second is not, find where pred becomes false (within epsilon).
(define (binary-search-floats pred p1 p2 close-enough)
  (binary-search (lambda (a b) (if (close-enough a b) #f
				   (/ (+ a b) 2)))
		 pred p1 p2))

;; Implemented here for example.
(define binary-search-ints (curry binary-search (compose floor (compose (curryr / 2) +))))

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
