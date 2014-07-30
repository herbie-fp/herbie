#lang racket

(require math/flonum)
(require math/bigfloat)

(provide reap define-table println ->flonum ->bf cotan bfmod flmod e ordinary-float? =-or-nan?
         enumerate take-up-to argmins list-product alist-append
         *debug* debug pipe
	 safe-eval write-file write-string has-duplicates?
	 with-item symbol<? common-eval-ns
	 flip-lists argmaxs
	 binary-search-floats)

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

(define *tags*
  #hasheq([misc . ";  "] [enter . ">  "] [exit . "<  "] [info . ";; "]
          [error . "!! "]))

(define (debug #:from [from 'casio] #:tag [tag 'misc] #:depth [depth 0] . args)
  (when (port? *debug*)
    (println #:port *debug* #:end "\t"
             (hash-ref *tags* tag ";  ")
             from (if (> 0 depth) (format ":~a" depth) ""))
    (for/list ([arg args])
      (display " " *debug*)
      ((if (string? arg) display write) arg *debug*))
    (newline *debug*)))


(define-syntax-rule (reap [sow] body ...)
  (let* ([store '()]
         [sow (λ (elt) (set! store (cons elt store)) elt)])
    body ...
    (reverse store)))

(define (->flonum x)
  (cond
   [(real? x) (real->double-flonum x)]
   [(bigfloat? x) (real->double-flonum (bigfloat->flonum x))]
   [(complex? x)
    (if (= (imag-part x) 0)
        (->flonum (real-part x))
        +nan.0)]
   [(eq? x 'pi) pi]
   [(eq? x 'e) (exp 1)]
   [else (error "Invalid number" x)]))

(define (->bf x)
  (cond
   [(real? x) (bf x)]
   [(bigfloat? x) x]
   [(complex? x)
    (if (= (imag-part x) 0) (->bf (real-part x)) +nan.bf)]
   [(eq? x 'pi) pi.bf]
   [(eq? x 'e) (bfexp 1.bf)]
   [else (error "Invalid number" x)]))

; Functions and constants used in our language
(define (cotan x)
  (/ 1 (tan x)))

(define (bfmod x mod)
  (bf- x (bf* mod (bffloor (bf/ x mod)))))

(define (flmod x mod)
  (fl- x (fl* mod (flfloor (fl/ x mod)))))

(define e
  (exp 1))

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

(define-namespace-anchor common-eval-ns-anchor)
(define common-eval-ns (namespace-anchor->namespace common-eval-ns-anchor))

(define (safe-eval expr) (eval expr common-eval-ns))

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
