#lang racket
(require math/bigfloat)
(require math/flonum)
(require casio/common)

(provide program-body program-variables program-cost
         location-induct program-induct
	 location-do location-get eval-prog operations
	 mode:bf mode:fl compile expression-cost)

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

(define (location-induct
	 prog
	 #:toplevel [toplevel (λ (expr location) expr)] #:constant [constant (λ (c location) c)]
	 #:variable [variable (λ (x location) x)] #:primitive [primitive (λ (list location) list)]
	 #:symbol [symbol-table (λ (sym location) sym)])
  (define (inductor prog location)
    (cond
     [(real? prog) (constant prog (reverse location))]
     [(symbol? prog) (variable prog (reverse location))]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog) (cons 2 location))])
	(toplevel `(λ ,(program-variables prog) ,body*) (reverse location)))]
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
         #:symbol [symbol-table identity])

  ; Inlined for speed
  (define (inductor prog)
    (cond
     [(real? prog) (constant prog)]
     [(symbol? prog) (variable prog)]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog))])
	(toplevel `(λ ,(program-variables prog) ,body*)))]
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

(define-namespace-anchor eval-prog-ns-anchor)
(define eval-prog-ns (namespace-anchor->namespace eval-prog-ns-anchor))

(define (eval-prog prog mode)
  (let* ([real->precision (list-ref (hash-ref operations #f) mode)]
         [op->precision (lambda (op) (list-ref (hash-ref operations op) mode))]
         [prog* (program-induct prog #:constant real->precision #:symbol op->precision)]
         [prog-opt `(λ ,(program-variables prog*)
                       ,(compile (program-body prog*)))]
         [fn (eval prog-opt eval-prog-ns)])
    (lambda (pts)
      (->flonum (apply fn (map real->precision pts))))))

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

; Table defining costs and translations to bigfloat and regular float
; See "costs.c" for details of how these costs were determined
(define operations
  (let ([table
         ;  op       bf       fl      cost
         `([+       ,bf+     ,fl+     1]
           [-       ,bf-     ,-       1]
           [*       ,bf*     ,fl*     1]
           [/       ,bf/     ,/       1]
           [abs     ,bfabs   ,flabs   1]
           [sqrt    ,bfsqrt  ,flsqrt  1]
           [sqr     ,bfsqr   ,sqr     1]
           [exp     ,bfexp   ,flexp   270]
           [expt    ,bfexpt  ,flexpt  640]
           [log     ,bflog   ,fllog   300]
           [sin     ,bfsin   ,flsin   145]
           [cos     ,bfcos   ,flcos   185]
           [tan     ,bftan   ,fltan   160]
           [cotan   ,bfcot   ,cotan   160]
           [asin    ,bfasin  ,flasin  140]
           [acos    ,bfacos  ,flacos  155]
           [atan    ,bfatan  ,flatan  130]
           [sinh    ,bfsinh  ,flsinh  300]
           [cosh    ,bfcosh  ,flcosh  300]
           [tanh    ,bftanh  ,fltanh  300]
           [if      ,if-fn   ,if-fn   1]
           [>       ,bf>     ,fl>     1]
           [<       ,bf<     ,fl<     1]
           [<=      ,bf<=    ,fl<=    1]
           [>=      ,bf>=    ,fl>=    1]
           [and     ,and-fn  ,and-fn  1]
           [or      ,or-fn   ,or-fn   1]
           [atan2   ,bfatan2 ,atan    230]
	   [mod     ,(λ (bigx bigmod)
		       (bf- bigx (bf* bigmod (bffloor (bf/ bigx bigmod)))))
		             ,(λ (fx fmod)
				(fl- fx (fl* fmod (flfloor (fl/ fx fmod)))))
			              1]

           ; For compiling variables
           [#f   ,bf      ,real->double-flonum 0])])

    ; Munge the table above into a hash table.
    (let ([hash (make-hasheq)])
      (for ([rec table])
        (hash-set! hash (car rec) (cdr rec)))
      hash)))

(define mode:bf 0)
(define mode:fl 1)

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
