#lang racket

(require math/bigfloat)
(require math/flonum)
(require "common.rkt")
(require "syntax.rkt")

(provide (all-from-out "syntax.rkt")
         location-induct program-induct expression-induct location-hash
         location-do location-get
         eval-prog
	 compile expression-cost program-cost
         free-variables replace-expression)

(define (location-induct
	 prog
	 #:toplevel [toplevel (λ (expr location) expr)] #:constant [constant (λ (c location) c)]
	 #:variable [variable (λ (x location) x)] #:primitive [primitive (λ (list location) list)]
	 #:symbol [symbol-table (λ (sym location) sym)] #:predicate [predicate (λ (pred loc) pred)])

  (define (inductor prog location)
    (cond
     [(constant? prog)
      (constant prog (reverse location))]
     [(variable? prog)
      (variable prog (reverse location))]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog) (cons 2 location))])
	(toplevel `(λ ,(program-variables prog) ,body*) (reverse location)))]
     [(and (list? prog) (memq (car prog) predicates))
      (predicate (cons (symbol-table (car prog) (reverse (cons 0 location)))
		       (enumerate #:from 1
				  (λ (idx prog) (inductor prog (cons idx location)))
				  (cdr prog)))
		 (reverse location))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog) (reverse (cons 0 location)))
		       (enumerate #:from 1
                                  (λ (idx prog) (inductor prog (cons idx location)))
                                  (cdr prog)))
		 (reverse location))]))
  (inductor prog '()))

(define (location-hash prog)
  (define expr->locs (make-hash))

  (define (save expr loc)
    (hash-update! expr->locs expr (curry cons loc) '())
    expr)

  (location-induct prog #:constant save #:variable save #:primitive save)
  expr->locs)

(define (expression-induct
	 expr vars
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity] #:predicate [predicate identity])
  (program-body (program-induct
		 `(λ ,vars ,expr)
		 #:toplevel toplevel #:constant constant
		 #:variable variable #:primitive primitive
		 #:symbol symbol-table #:predicate predicate)))

(define (program-induct
         prog
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity] #:predicate [predicate identity])

  ; Inlined for speed
  (define (inductor prog)
    (cond
     [(constant? prog) (constant prog)]
     [(variable? prog) (variable prog)]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog))])
	(toplevel `(λ ,(program-variables prog) ,body*)))]
     [(and (list? prog) (memq (car prog) predicates))
      (predicate (cons (symbol-table (car prog))
		       (map inductor (cdr prog))))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog))
		       (map inductor (cdr prog))))]))

  (inductor prog))

(define (free-variables prog [bound constants])
  (filter (λ (v) (not (member v bound)))
          (match prog
            [(? constant?) '()]
            [(? variable?) (list prog)]
            [`(λ ,vars ,body)
             (free-variables body (append vars constants))]
            [`(lambda ,vars ,body)
             (free-variables body (append vars constants))]
            [`(,f ,args ...)
             (remove-duplicates (append-map (curryr free-variables bound) args))])))

(define (replace-expression program from to)
  (cond
   [(equal? program from)
    to]
   [(list? program)
    (for/list ([subexpr program])
      (replace-expression subexpr from to))]
   [else
    program]))

(define (location-do loc prog f)
  (cond
   [(null? loc)
    (f prog)]
   [(not (pair? prog))
    (error "Bad location: cannot enter " prog "any further.")]
   [#t
    ; Inlined loop for speed
    (let loop ([idx (car loc)] [lst prog])
      (if (= idx 0)
          (cons (location-do (cdr loc) (car lst) f) (cdr lst))
          (cons (car lst) (loop (- idx 1) (cdr lst)))))]))

(define (location-get loc prog)
  ; Clever continuation usage to early-return
  (let/ec return
    (location-do loc prog return)))

(define (eval-prog prog mode)
  (let* ([real->precision (if (equal? mode mode:bf) ->bf ->flonum)]
         [op->precision (λ (op) (list-ref (hash-ref (*operations*) op) mode))]
         [prog* (program-induct prog #:constant real->precision #:symbol op->precision)]
         [prog-opt `(λ ,(program-variables prog*) ,(compile (program-body prog*)))]
         [fn (eval prog-opt common-eval-ns)])
    (lambda (pts)
      (->flonum (apply fn (map real->precision pts))))))

;; To compute the cost of a program, we could use the tree as a
;; whole, but this is inaccurate if the program has many common
;; subexpressions.  So, we compile the program to a register machine
;; and use that to estimate the cost.

(define (compile expr)
  (define assignments '())
  (define compilations (make-hash))

  ;; TODO : use one of Racket's memoization libraries
  (define (compile-one expr)
    (if (list? expr)
        (let ([memo (hash-ref compilations expr #f)])
          (or memo
              (let* ([fn (car expr)] [children (cdr expr)]
                     [newexpr (cons fn (map compile-one children))]
                     [register (gensym "r")])
                (hash-set! compilations expr register)
                (set! assignments (cons (list register newexpr) assignments))
                register)))
        (let ([memo (hash-ref compilations expr #f)])
          (or memo
              (let* ([register (gensym "r")])
                (hash-set! compilations expr register)
                (set! assignments (cons (list register expr) assignments))
                register)))))

  (let ([reg (compile-one expr)])
    `(let* ,(reverse assignments) ,reg)))

(define (program-cost prog)
  (expression-cost (program-body prog)))

(define (expression-cost expr)
  (for/sum ([step (second (compile expr))])
    (if (list? (second step))
        (let ([fn (caadr step)])
          (list-ref (hash-ref (*operations*) fn) 2))
        1)))
