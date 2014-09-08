#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/config)
(require casio/debug)

(provide reap define-table println ordinary-float? =-or-nan?
         enumerate take-up-to argmins list-product alist-append for/accumulate
         *debug* *debug-port* debug pipe
	 write-file write-string has-duplicates?
	 with-item symbol<?
	 flip-lists argmaxs multipartition
	 safe-eval write-file write-string has-duplicates?
	 with-item symbol<? common-eval-ns
	 flip-lists argmaxs multipartition
	 binary-search-floats binary-search-ints
         random-exp
         (all-from-out casio/config) (all-from-out casio/debug))

(define (println #:port [p (current-error-port)] #:end [end "\n"] . args)
  (for ([val args])
    (if (string? val)
        (display val p)
        (write val p)))
  (when end (display end p))
  (let ([possible-returns (filter (negate string?) args)])
    (when (not (null? possible-returns))
      (last possible-returns))))

(define *debug* (make-parameter #f))

(define *debug-port* (make-parameter (current-output-port)))

(define *tags*
  #hasheq([misc  . "[misc]"]
          [enter . "[enter]"]
          [exit  . "[exit]"]
          [info  . "[info]"]
          [error . "[ERROR]"]))

;; To set a particular #:from max-depth, pass it in here.
;; To turn on all messages for a particular #:from, pass in a depth of #t.
;; To set the default, pass in a max depth with the #:from #t.
(define (set-debug-level! from depth)
  (let ([existing (cond [(not (*debug*)) '((#t . 0))]
			[(eq? #t (*debug*)) '((#t . #t))]
			[#t (*debug*)])])
    (*debug* (cons (cons from depth) existing))))

(define (should-print-debug? from depth)
  (or (eq? (*debug*) #t) ;; If debug is true, print no matter what
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
		      (> max-depth 0)))))))

(define (debug #:from [from 'casio] #:tag [tag 'misc] #:depth [depth 1] . args)
  (when (should-print-debug? from depth)
    (debug-print from tag args (*debug-port*))))

(define (debug-print from tag args port)
  (display (hash-ref *tags* tag "; ") port)
  (write from port)
  (display ": " port)
  (for/list ([arg args])
    (display " " port)
    ((if (string? arg) display write) arg port))
  (newline port))

(define-syntax-rule (reap [sow] body ...)
  (let* ([store '()]
         [sow (λ (elt) (set! store (cons elt store)) elt)])
    body ...
    (reverse store)))

(define-syntax-rule (for/accumulate [reg start] ([vars seqs] ...) body ...)
  (pipe start (map (λ (vars ...) (λ (reg) body ...)) seqs ...)))

(define-syntax-rule (define-table name [key values ...] ...)
  (define name
    (let ([hash (make-hasheq)])
      (for ([rec (list (list 'key values ...) ...)])
        (hash-set! hash (car rec) (cdr rec)))
      hash)))

(define (ordinary-float? x)
  (not (or (infinite? x) (nan? x))))

(define (=-or-nan? x1 x2)
  (or (= x1 x2)
      (and (nan? x1) (nan? x2))))

(define (enumerate #:from [start 0] fun . lsts)
  (let loop ([idx start] [lsts (apply map list lsts)])
    (if (null? lsts)
        '()
        (cons (apply fun idx (car lsts)) (loop (+ 1 idx) (cdr lsts))))))

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

(define (with-item idx item lst)
  (map (λ (lst-item lst-idx)
	 (if (= lst-idx idx) item lst-item))
       lst
       (range (length lst))))

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
	  [(or (= p1 midpoint) (= p2 midpoint)) midpoint]
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

;; Flag Stuff

(define *flags*
  (make-parameter
   #hash([generate . (simplify rm)]
         #;[evaluate . (random-points)]
         [reduce   . (regimes zach)]
         [regimes  . (recurse prefilter)]
         [simplify . ()]
         [setup    . (simplify periodicity)])))

(define (toggle-flag! category flag)
  (*flags*
   (hash-update (*flags*) category
		(λ (flag-list)
		  (if (member flag flag-list)
		      (remove flag flag-list)
		      (cons flag flag-list))))))

(define ((flag type f) a b)
  (if (member f (hash-ref (*flags*) type
                          (λ () (error "Invalid flag type" type))))
      a
      b))
