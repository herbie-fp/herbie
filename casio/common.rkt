#lang racket

(require math/bigfloat)
(require data/order)

(provide reap println ->flonum *precision* cotan ordinary-float?
         list= list< enumerate take-up-to *debug* debug debug-reset pipe 1+
	 flip-args idx-map list-product set-debug-level! match-loc-fst
	 match-loc)

; Precision for approximate evaluation
(define *precision* (make-parameter real->double-flonum))

(define (println #:port [p (current-output-port)] #:end [end "\n"] . args)
  (for ([val args])
    (if (string? val)
        (display val p)
        (write val p)))
  (when end (display end p))
  (let ([possible-returns (filter (negate string?) args)])
    (when (not (null? possible-returns))
      (last possible-returns))))

(define *debug* (make-parameter #f))
(define *log* '())

(define *tags*
  #hasheq([enter . "> "]
          [exit . "< "]
          [info . ";; "]))

;; To set a particular #:from max-depth, pass it in here.
;; To turn on all messages for a particular #:from, pass in a depth of #t.
;; To set the default, pass in a max depth with the #:from #t.
(define (set-debug-level! from depth)
  (let ([existing (cond [(not (*debug*)) '((#t . 0))]
			[(eq? #t (*debug*)) '((#t . #t))]
			[#t (*debug*)])])
    (*debug* (cons (cons from depth) existing))))

(define (debug #:from from #:tag (tag #f) #:depth (depth 1) . args)
  (when (or (eq? (*debug*) #t) ;; If debug is true, print no matter what
	    (and (*debug*) ;; If debug is false, never print
		 (let ([max-depth (if (and from (dict-has-key? (*debug*) from))
				      ;; If we were given a #:from, and we have it in the dictionary,
				      ;; look up it's max depth
				      (dict-ref (*debug*) from)
				      ;; Otherwise, just use whatevers default.
				      (dict-ref (*debug*) #t))])
		   ;; If the max depth is true, turn everything on.
		   ;; If the max depth isn't positve, turn everything off.
		   ;; Otherwise, if our dept is less than the max-depth,
		   ;; return true.
		   (or (eq? max-depth #t)
		       (and (>= max-depth depth)
			    (> max-depth 0))))))
    (set! *log* (cons (list* from tag args) *log*))
    (display (hash-ref *tags* tag "; "))
    (write from)
    (display ": ")
    (for/list ([arg args])
      (display " ")
      ((if (string? arg) display write) arg))
    (newline)))

(define (debug-reset)
  (set! *log* '()))

(define-syntax (reap stx)
  "A reap/sow abstraction for filters and maps."
  (syntax-case stx ()
    [(_ [sow] body ...)
     #'(let* ([store '()]
              [sow (λ (elt) (set! store (cons elt store)) elt)])
         body ...
         (reverse store))]))

(define (->flonum x)
  (cond
   [(real? x) ((*precision*) x)]
   [(bigfloat? x) ((*precision*) (bigfloat->flonum x))]
   [(complex? x)
    (if (= (imag-part x) 0)
        (->flonum (real-part x))
        +nan.0)]
   [else (error "Invalid number" x)]))

; Functions used by our benchmarks
(define (cotan x)
  (/ 1 (tan x)))

(define (ordinary-float? x)
  (not (or (infinite? x) (nan? x))))

(define (=-or-nan? x1 x2)
  (or (= x1 x2)
      (and (nan? x1) (nan? x2))))

(define (1+ x)
  (+ 1 x))

(define (list= l1 l2)
  (and l1 l2 (andmap =-or-nan? l1 l2)))

(define (list< list1 list2)
  "Compares lists lexicographically."
  ; Who picked this terrible API design of returning '< or '> ?
  (eq? (datum-order list1 list2) '<))

(define (idx-map fun  lst #:from [start 0])
  (let loop ([idx start] [lst lst])
    (if (null? lst)
        '()
        (cons (fun (car lst) idx) (loop (+ 1 idx) (cdr lst))))))

(define (enumerate . l)
  (apply map list (range (length (car l))) l))

(define (take-up-to l k)
  ; This is unnecessarily slow. It is O(l), not O(k).
  ; To be honest, it just isn't that big a deal for now.
  (take l (min k (length l))))

;; Pipes an initial values through a list of funcs.
(define (pipe initial funcs)
  ((apply compose (reverse funcs)) initial))

;; Flips the argument order of a two argument function.
(define (flip-args f) (lambda (x y) (f y x)))

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
;; Simple location match utility function. If 'a' is a continutation of 'b',
;; such as in a='(2 1) b='(2), returns the tail of
;; 'a' after 'b', '(1). Visa-versa for 'b' as a continuation of 'a'. If
;; 'a' and 'b' diverge at some point before the end, returns false.
(define (match-loc a b)
  (cond [(null? a) b]
	[(null? b) a]
	[(= (car a) (car b)) (match-loc (cdr a) (cdr b))]
	[#t #f]))

(define (match-loc-fst inside outside)
  (cond [(null? outside) inside]
	[(null? inside) #f]
	[(= (car outside) (car inside))
	 (match-loc-fst (cdr inside) (cdr outside))]
	[#t #f]))

