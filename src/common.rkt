#lang racket

(require math/flonum)
(require math/bigfloat)
(require "config.rkt" "errors.rkt" "debug.rkt")

(module+ test
  (require rackunit))

(provide reap define-table println ordinary-float? =-or-nan?
         take-up-to argmins
         common-eval-ns common-eval
         write-file write-string
         *start-prog* html-escape-unsafe
         flip-lists argmaxs
         binary-search-floats binary-search-ints binary-search
         random-exp assert setfindf first-value log2 for/append
         (all-from-out "config.rkt") (all-from-out "debug.rkt")
         get-seed set-seed! index-of
         parse-flag)

(define *start-prog* (make-parameter '()))

(define (println #:port [p (current-error-port)] #:end [end "\n"] . args)
  (for ([val args])
    (if (string? val)
        (display val p)
        (write val p)))
  (when end (display end p))
  (let ([possible-returns (filter (negate string?) args)])
    (when (not (null? possible-returns))
      (last possible-returns))))

(define-syntax-rule (reap [sows ...] body ...)
  (let* ([sows (let ([store '()])
		 (λ (elt) (if elt
			      (begin (set! store (cons elt store))
				     elt)
			      store)))] ...)
    body ...
    (values (reverse (sows #f)) ...)))

(define-syntax-rule (define-table name [key values ...] ...)
  (define name
    (let ([hash (make-hasheq)])
      (for ([rec (list (list 'key values ...) ...)])
        (hash-set! hash (car rec) (cdr rec)))
      hash)))

(define-syntax-rule (first-value expr)
  (call-with-values
      (λ () expr)
    (compose car list)))

(module+ test
  (check-equal? (first-value (values 1 2 3)) 1))

(define (ordinary-float? x)
  (and (real? x) (not (or (infinite? x) (nan? x)))))

(module+ test
  (check-true (ordinary-float? 2.5))
  (check-false (ordinary-float? +nan.0))
  (check-false (ordinary-float? -inf.f)))

(define (=-or-nan? x1 x2)
  (or (= x1 x2)
      (and (nan? x1) (nan? x2))))

(module+ test
  (check-true (=-or-nan? 2.3 2.3))
  (check-false (=-or-nan? 2.3 7.8))
  (check-true (=-or-nan? +nan.0 -nan.f))
  (check-false (=-or-nan? 2.3 +nan.f)))

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
        best-elts
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
                '("g" "f" "a"))) ; should this be in reverse order?

(define (argmaxs f lst)
  (argmins (λ (x) (- (f x))) lst))

(module+ test
  (check-equal? (argmaxs string-length '("a" "bb" "f" "ccc" "dd" "eee" "g"))
                '("eee" "ccc")))

(define-syntax-rule (write-file filename . rest)
   (with-output-to-file filename (lambda () . rest) #:exists 'replace))

(define-syntax-rule (write-string . rest)
  (with-output-to-string (lambda () . rest)))

;; Basically matrix flipping, but for lists. So, if you pass it '((1 2 3) (4 5 6) (7 8 9)),
;; it returns '((1 4 7) (2 5 8) (3 6 9)).
(define (flip-lists list-list)
  (apply map list list-list))

(module+ test
  (check-equal? (flip-lists '((1 2 3) (4 5 6) (7 8 9)))
                '((1 4 7) (2 5 8) (3 6 9))))

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
(define (binary-search-floats pred p1 p2 epsilon)
  (define (close-enough a b) (> epsilon (abs (- a b))))
  (binary-search (lambda (a b) (if (close-enough a b) #f
				   (/ (+ a b) 2)))
		 pred p1 p2))

;; Implemented here for example.
(define binary-search-ints (curry binary-search (compose floor (compose (curryr / 2) +))))

(define (random-exp k)
  "Like (random (expt 2 k)), but k is allowed to be arbitrarily large"
  (if (< k 31) ; Racket generates random numbers in the range [0, 2^32-2]; I think it's a bug
      (random (expt 2 k))
      (let ([head (* (expt 2 31) (random-exp (- k 31)))])
        (+ head (random (expt 2 31))))))

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

(define (setfindf f s)
  (for/first ([elt (in-set s)] #:when (f elt))
    elt))

(module+ test
  (check-equal? (setfindf positive? (set -3 6 0)) 6))

(define (log2 x)
  (/ (log x) (log 2)))

(define (html-escape-unsafe err)
  (string-replace (string-replace (string-replace err "&" "&amp;") "<" "&lt;") ">" "&gt;"))

(module+ test
  (check-equal? (html-escape-unsafe "foo&bar") "foo&amp;bar")
  (check-equal? (html-escape-unsafe "foo<bar") "foo&lt;bar")
  (check-equal? (html-escape-unsafe "foo>bar") "foo&gt;bar")
  (check-equal? (html-escape-unsafe "&foo<bar>") "&amp;foo&lt;bar&gt;"))

(define-syntax-rule (for/append (defs ...)
                                bodies ...)
  (apply append
         (for/list (defs ...)
           bodies ...)))

(module+ test
  (check-equal? (for/append ([v (in-range 5)]) (list v v v))
                '(0 0 0 1 1 1 2 2 2 3 3 3 4 4 4)))

(define (get-seed)
  (pseudo-random-generator->vector
   (current-pseudo-random-generator)))

(define (set-seed! seed)
  "Reset the random number generator to a new seed"
  (current-pseudo-random-generator
   (vector->pseudo-random-generator seed)))

(define (index-of lst elt)
  (for/first ([e lst] [i (in-naturals)]
             #:when (equal? e elt))
             i))

(module+ test
  (check-equal? (index-of '(a b c d e) 'd) 3)
  (check-equal? (index-of '(a b c d e) 'foo) #f))

(define-namespace-anchor common-eval-ns-anchor)
(define common-eval-ns (namespace-anchor->namespace common-eval-ns-anchor))
(define (common-eval expr) (eval expr common-eval-ns))

(define (parse-flag s)
  (match (string-split s ":")
    [(list (app string->symbol category) (app string->symbol flag))
     (and
      (dict-has-key? all-flags category)
      (set-member? (dict-ref all-flags category) flag)
      (list category flag))]
    [_ #f]))
