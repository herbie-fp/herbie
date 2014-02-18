#lang racket

(require math/bigfloat)
(require casio/common)

(provide program-body program-variables program-cost
         location-induct program-induct location-do location-get
         eval-prog operations mode:bf mode:fl)

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

(define (location-map fun list)
  (letrec ([loc-map (λ (list fun acc location)
		       (if (null? list)
			   acc
			   (loc-map (cdr list) fun 
				    (cons (fun (car list) location) acc)
				    (cons 'cdr location))))])
    (reverse (loc-map list fun '() '()))))


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
      (let ([body* (inductor (program-body prog) (list* 'car 'cdr 'cdr location))])
	(toplevel `(λ ,(program-variables prog) ,body*) (reverse location)))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog) (reverse (cons 'car location)))
		       (location-map (λ (prog loc)
					(inductor prog (append (cons 'car (cons 'cdr loc)) location)))
				     (cdr prog)))
		 (reverse location))]))
  (inductor prog '()))

(define (program-induct
         prog
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity])

  (define ((ignore f) x y) (f x))

  (location-induct prog
    #:toplevel (ignore toplevel) #:constant (ignore constant) #:variable (ignore variable)
    #:primitive (ignore primitive) #:symbol (ignore symbol-table)))

(define (location-do loc prog f)
  (cond
   [(null? loc)
    (f prog)]
   [(eq? (car loc) 'car)
    (cons (location-do (cdr loc) (car prog) f) (cdr prog))]
   [(eq? (car loc) 'cdr)
    (cons (car prog) (location-do (cdr loc) (cdr prog) f))]
   [#t
    (error "Unknown location" loc)]))

(define (location-get loc prog)
  ; Clever continuation usage to early-return
  (let/ec return
    (location-do loc prog return)))

(define-namespace-anchor eval-prog-ns-anchor)
(define eval-prog-ns (namespace-anchor->namespace eval-prog-ns-anchor))

(define (eval-prog prog mode)
  (let* ([real->precision (list-ref (hash-ref operations '*var*) mode)]
         [op->precision (lambda (op) (list-ref (hash-ref operations op) mode))]
         [prog* (program-induct prog #:constant real->precision #:symbol op->precision)]
         [fn (eval prog* eval-prog-ns)])
    (lambda (pts)
      (->flonum (apply fn (map real->precision pts))))))

; Table defining costs and translations to bigfloat and regular float
; See "costs.c" for details of how these costs were determined
(define operations
  (let ([table
         ;  op       bf       fl      cost
         `([+       ,bf+     ,+       1]
           [-       ,bf-     ,-       1]
           [*       ,bf*     ,*       1]
           [/       ,bf/     ,/       1]
           [abs     ,bfabs   ,abs     1]
           [sqrt    ,bfsqrt  ,sqrt    1]
           [square  ,bfsqr   ,square  1]
           [exp     ,bfexp   ,exp     270]
           [log     ,bflog   ,log     300]
           [sin     ,bfsin   ,sin     145]
           [cos     ,bfcos   ,cos     185]
           [tan     ,bftan   ,tan     160]
           [cotan   ,bfcot   ,cotan   160]
           [asin    ,bfasin  ,asin    140]
           [acos    ,bfacos  ,acos    155]
           [atan    ,bfatan  ,atan    130]

           ; For compiling variables
           [*var*   ,bf      ,(*precision*) 0])])

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
  (define assignments '())
  (define compilations (make-hash))

  ;; TODO : use one of Racket's memoization libraries
  (define (compile expr)
    (if (list? expr)
        (let ([memo (hash-ref compilations expr #f)])
          (or memo
              (let* ([fn (car expr)] [children (cdr expr)]
                     [newexpr (cons fn (map compile children))]
                     [register (gensym "r")])
                (hash-set! compilations expr register)
                (set! assignments (cons (cons register newexpr) assignments))
                register)))
        expr))

  (compile (program-body prog))

  (for/sum ([step assignments])
    (let ([fn (cadr step)])
      (list-ref (hash-ref operations fn) 2))))
