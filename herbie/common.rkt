#lang racket

(require math/flonum)
(require math/bigfloat)
(require "config.rkt")
(require "debug.rkt")

(provide reap define-table println ordinary-float? =-or-nan?
         take-up-to argmins list-product alist-append list-join
         pipe ulp-difference *bit-width* ulps->bits bit-difference
	 write-file write-string has-duplicates?
	 symbol<? *start-prog* html-escape-unsafe
	 flip-lists argmaxs multipartition
	 binary-search-floats binary-search-ints binary-search
         random-exp assert setfindf first-value log2 for/append
         (all-from-out "config.rkt") (all-from-out "debug.rkt")
         get-seed set-seed! in-pairs)

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

(define (ordinary-float? x)
  (and (real? x) (not (or (infinite? x) (nan? x)))))

(define (=-or-nan? x1 x2)
  (or (= x1 x2)
      (and (nan? x1) (nan? x2))))

(define (take-up-to l k)
  ; This is unnecessarily slow. It is O(l), not O(k).
  ; To be honest, it just isn't that big a deal for now.
  (take l (min k (length l))))

;; Pipes an initial values through a list of funcs.
(define (pipe initial funcs)
  ((apply compose (reverse funcs)) initial))

;; A more informative andmap. If any of your results are false, this returns
;; false. Otherwise, it acts as a normal map.
(define (info-andmap f l)
  (let loop ([rest l] [acc '()])
    (if (null? rest)
	(reverse acc)
	(let ([result (f l)])
	  (and result (loop (cdr rest) (cons result acc)))))))

(define (list-product . subs)
  (if (null? subs)
      '(())
      (for*/list ([fst (car subs)]
                  [rst (apply list-product (cdr subs))])
         (cons fst rst))))

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

(define (argmaxs f lst)
  (argmins (λ (x) (- (f x))) lst))

(define (alist-append . args)
  (define (a-append joe bob)
    (if (null? joe)
	bob
	(a-append (cdr joe) (cons
                             (cons (caar joe)
                                   (let ([match (assoc (caar joe) bob)])
                                     (if match
                                         (append (cdr match) (cdar joe))
                                         (cdar joe))))
                             bob))))
  (if (< 2 (length args))
      (car args)
      (foldr (lambda (x y) (a-append x y)) '() args)))

(define-syntax-rule (write-file filename . rest)
   (with-output-to-file filename (lambda () . rest) #:exists 'replace))

(define-syntax-rule (write-string . rest)
  (with-output-to-string (lambda () . rest)))

(define (has-duplicates? lst)
  (cond [(null? lst) #f]
	[(member (car lst) (cdr lst)) #t]
	[#t (has-duplicates? (cdr lst))]))

;; Provide sorting for symbols so that we can canonically order variables and other atoms
(define (symbol<? sym1 sym2)
  (string<? (symbol->string sym1) (symbol->string sym2)))

;; Basically matrix flipping, but for lists. So, if you pass it '((1 2 3) (4 5 6) (7 8 9)),
;; it returns '((1 4 7) (2 5 8) (3 6 9)).
(define (flip-lists list-list)
  (apply map list list-list))

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

;; Takes a list of items, and returns a list of lists of items, where
;; the items are grouped by the value produced when key-func is evaluated
;; on them.
(define (multipartition items key-func)
  (let loop ([rest-items items] [acc '()])
    (if (null? rest-items) (reverse (map (compose reverse cdr) acc))
	(let* ([key (key-func (car rest-items))]
	       [lookup (assoc key acc)])
	  (loop (cdr rest-items)
		(if lookup
		    (cons (cons (car lookup) (cons (car rest-items) (cdr lookup)))
			  (remove lookup acc))
		    (cons (cons key (list (car rest-items)))
			  acc)))))))

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
  (let/ec return
    (set-for-each s (λ (el)
		      (when (f el)
			(return el))))
    #f))

(define (single-flonum->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f))

(define (single-flonum->ordinal x)
  (cond [(x . < . 0.0f0) (- (single-flonum->bit-field (- 0.0f0 x)))]
	[else            (single-flonum->bit-field (abs x))]))

(define (single-flonums-between x y)
  (- (single-flonum->ordinal y) (single-flonum->ordinal x)))

(define (ulp-difference x y)
  (((flag 'precision 'double) flonums-between single-flonums-between) x y))

(define (*bit-width*) ((flag 'precision 'double) 64 32))

(define (ulps->bits x)
  (cond
   [(nan? x) +nan.0]
   [(infinite? x) (*bit-width*)]
   [else (log2 x)]))

(define (bit-difference x y)
  (ulps->bits (+ 1 (abs (ulp-difference x y)))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (list-join l1 l2)
  (match l1
    ['() '()]
    [(list but-last1 ... last1)
     (append (append-map (curryr cons l2) but-last1) (list last1))]))

(define (html-escape-unsafe err)
  (string-replace (string-replace (string-replace err "&" "&amp;") "<" "&lt;") ">" "&gt;"))

(define-syntax-rule (for/append (defs ...)
                                bodies ...)
  (apply append
         (for/list (defs ...)
           bodies ...)))

(define (get-seed)
  (pseudo-random-generator->vector
   (current-pseudo-random-generator)))

(define (set-seed! seed)
  "Reset the random number generator to a new seed"
  (current-pseudo-random-generator
   (vector->pseudo-random-generator seed)))

(define (in-pairs seq)
  "Given a sequence of pairs, returns a sequence of their cars and cdrs."
  ;; This code is pretty confusing; the sequence APIs are not good
  (define-values (more? next) (sequence-generate seq))
  (define stop? #f)
  (in-producer
   (λ () (if stop? (values #f #f) (let ([x (next)]) (values (car x) (cdr x)))))
   (λ _ (begin0 stop? (set! stop? (not (more?)))))))
