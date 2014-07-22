#lang racket
(require math/bigfloat)
(require math/flonum)
(require casio/common)
(require casio/syntax)

(provide program-body program-variables program-cost
         location-induct program-induct
	 location-do location-get eval-prog operations
	 mode:bf mode:fl compile expression-cost
         (all-from-out casio/syntax))

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)
(define preds '(or and < > <= >= =))

(define (location-induct
	 prog
	 #:toplevel [toplevel (λ (expr location) expr)] #:constant [constant (λ (c location) c)]
	 #:variable [variable (λ (x location) x)] #:primitive [primitive (λ (list location) list)]
	 #:symbol [symbol-table (λ (sym location) sym)] #:predicate [predicate (λ (pred loc) pred)])
  (define (inductor prog location)
    (cond
     [(real? prog) (constant prog (reverse location))]
     [(symbol? prog) (variable prog (reverse location))]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog) (cons 2 location))])
	(toplevel `(λ ,(program-variables prog) ,body*) (reverse location)))]
     [(and (list? prog) (memq (car prog) preds))
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

(define (program-induct
         prog
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity] #:predicate [predicate identity])

  ; Inlined for speed
  (define (inductor prog)
    (cond
     [(real? prog) (constant prog)]
     [(symbol? prog) (variable prog)]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog))])
	(toplevel `(λ ,(program-variables prog) ,body*)))]
     [(and (list? prog) (memq (car prog) preds))
      (predicate (cons (symbol-table (car prog))
		       (map inductor (cdr prog))))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog))
		       (map inductor (cdr prog))))]))

  (inductor prog))

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
  (let* ([real->precision (list-ref (hash-ref constants #f) mode)]
         [op->precision
          (λ (op) (list-ref (hash-ref operations op) mode))]
         [variable->precision
          (λ (var)
             (cond
              [(member var (program-variables prog)) var]
              [(hash-has-key? constants var)
               ((list-ref (hash-ref constants var) mode))]
              [else var]))]
         [prog*
          (program-induct prog #:constant real->precision #:symbol op->precision
                          #:variable variable->precision)]
         [prog-opt `(λ ,(program-variables prog*) ,(compile (program-body prog*)))]
         [fn (eval prog-opt common-eval-ns)])
    (lambda (pts)
      (->flonum (apply fn (map real->precision pts))))))

;; We want to weigh our heuristic search by the program cost.
;; Simplest would be to simply compute the size of the tree as a
;; whole.  but this is inaccurate if the program has many common
;; subexpressions.  So, we compile the program to a register machine
;; and use that to estimate the cost.

(define (program-cost prog)
  (expression-cost (program-body prog)))

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
        expr))

  (let ([reg (compile-one expr)])
    `(let* ,(reverse assignments) ,reg)))

(define (expression-cost expr)
  (for/sum ([step (second (compile expr))])
    (let ([fn (caadr step)])
      (list-ref (hash-ref operations fn) 2))))
