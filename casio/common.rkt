#lang racket

(require math/bigfloat)
(require data/order)

(provide reap println ->flonum cotan ordinary-float? =-or-nan?
         enumerate take-up-to *debug* debug debug-reset pipe
	 list-product set-debug-level! alist-append
	 safe-eval write-file write-string has-duplicates?
	 with-item)

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

(define (debug #:from from #:tag [tag #f] #:depth [depth 1] . args)
  (when (should-print-debug? from depth)
    (debug-print from tag args (current-output-port)))
  (set! *log* (cons (list* from tag args) *log*))

(define (debug-print from tag args port)
  (display (hash-ref *tags* tag "; ") port)
  (write from port)
  (display ": " port)
  (for/list ([arg args])
    (display " " port)
    ((if (string? arg) display write) arg port))
  (newline port))

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
   [(real? x) (real->double-flonum x)]
   [(bigfloat? x) (real->double-flonum (bigfloat->flonum x))]
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

(define safe-eval
  (let ([ns (make-base-namespace)])
    (λ (expr) (eval expr ns))))

(define-syntax (write-file stx)
  (syntax-case stx ()
    [(_ filename . rest)
     #'(with-output-to-file filename (lambda () . rest) #:exists 'replace)]))

(define-syntax (write-string stx)
  (syntax-case stx ()
    [(_ . rest)
     #'(with-output-to-string (lambda () . rest))]))

(define (has-duplicates? lst)
  (cond [(null? lst) #f]
	[(member (car lst) (cdr lst)) #t]
	[#t (has-duplicates? (cdr lst))]))

(define (with-item idx item lst)
  (map (λ (lst-item lst-idx)
	 (if (= lst-idx idx) item lst-item))
       lst
       (range (length lst))))
