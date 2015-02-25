#lang racket

(require math/bigfloat)
(require math/flonum)
(require "common.rkt")
(require "syntax.rkt")

(provide (all-from-out "syntax.rkt")
         location-induct program-induct expression-induct location-hash
         location-do location-get location-parent location-sibling
         eval-prog replace-subexpr
	 compile expression-cost program-cost
         free-variables replace-expression
         loop-parts make-loop)

(define (location-induct
	 prog
	 #:toplevel [toplevel (λ (expr location) expr)] #:constant [constant (λ (c location) c)]
	 #:variable [variable (λ (x location) x)] #:primitive [primitive (λ (list location) list)]
	 #:symbol [symbol-table (λ (sym location) sym)] #:predicate [predicate (λ (pred loc) pred)]
	 #:let [let-handler (λ (expr location) expr)] #:fold [fold-handler (λ (expr location) expr)])

  (define (inductor prog location)
    (match prog
      [(? constant?) (constant prog (reverse location))]
      [(? variable?) (variable prog (reverse location))]
      [`(,(or 'λ 'lambda) ,vars ,body)
       (toplevel `(λ ,vars
		    ,(inductor body (cons 2 location)))
		 (reverse location))]
      ;; Note: for the purposes of locations, the subexpressions of a
      ;; let expression are first it's binding expressions, then its
      ;; body. That is, for an expression like:
      ;; 
      ;; (let ([x (+ 3 5)]
      ;;       [y 2])
      ;;   (+ x y))
      ;;   
      ;; Expr at '(1) is (+ 3 5), expr at  '(1 2) is 5, expr at '(2)
      ;; is 2, and expr at '(3) is (+ x y).
      [`(let ([,vars ,vals] ...) ,body)
       (let-handler
	`(let ,(for/list ([var vars] [val vals] [loc-t (in-naturals 1)])
		 (list var (inductor val (cons loc-t location))))
	   ,(inductor body (cons (add1 (length vars)) location)))
	(reverse location))]
      ;; Note: for/fold's do a similar thing to let's (see above), but
      ;; with both sets of bindings, so that the first lst-expr is at
      ;; (add1 (length accs)), and the body is at (+ (length accs)
      ;; (length inits) 1).
      [`(for/fold ([,accs ,inits] ...) ([,items ,lst-exprs] ...) (values . ,body-exprs))
       (fold-handler
        `(for/fold ,(for/list ([acc accs] [init inits] [loc-t (in-naturals 1)])
                      (list acc (inductor init (cons loc-t location))))
             ,(for/list ([item items] [lst-expr lst-exprs] [loc-t (in-naturals 1)])
                (list item (inductor lst-expr (cons loc-t location))))
           (values . ,(for/list ([body-expr body-exprs]
                                 [loc (sequence-map (curryr cons location)
                                                    (in-naturals
                                                     (+ (length accs) (length inits))))])
                        (inductor body-expr loc))))
        (reverse location))]
      [`(for/fold ([,accs ,inits] ...) ([,items ,lst-exprs] ...) ,body)
       (fold-handler
	`(for/fold ,(for/list ([acc accs] [init inits] [loc-t (in-naturals 1)])
		      (list acc (inductor init (cons loc-t location))))
	     ,(for/list ([item items] [lst-expr lst-exprs] [loc-t (in-naturals 1)])
		(list item (inductor lst-expr (cons loc-t location))))
	   ,(inductor body (cons (+ (length accs) (length inits) 1) location)))
        (reverse location))]
      [`(,fn ,args ...)
       (let ([expr* (cons (symbol-table fn (reverse (cons 0 location)))
			  (enumerate #:from 1
				     (λ (idx prog) (inductor prog (cons idx location)))
				     (cdr prog)))])
	 (if (memq fn predicates)
	     (predicate expr* (reverse location))
	     (primitive expr* (reverse location))))]
      [_ (error "malformed expression:" prog)]))

  (inductor prog '()))

(define (location-hash prog)
  (define expr->locs (make-hash))

  (define (save expr loc)
    (hash-update! expr->locs expr (curry cons loc) '())
    expr)

  (location-induct prog #:constant save #:variable save #:primitive save)
  expr->locs)

(define (expression-induct
	 expr
         #:constant [constant identity] #:variable [variable identity]
	 #:primitive [primitive identity] #:symbol [symbol-table identity]
	 #:predicate [predicate identity]
	 #:let [let-handler identity] #:fold [fold-handler identity])

  (define (inductor expr)
    (match expr
      [(? constant?) (constant expr)]
      [(? variable?) (variable expr)]
      [`(let ([,vars ,vals] ...) ,body)
       (let-handler
	`(let ,(map list vars (map inductor vals))
	   ,(inductor body)))]
      [`(for/fold ([,accs ,inits] ...) ([,items ,lst-exprs] ...)
      	  (values . ,bodies))
       (fold-handler
      	`(for/fold ,(map list accs (map inductor inits))
      	     ,(map list items (map inductor lst-exprs))
      	   (values . ,(map inductor bodies))))]
      [`(for/fold ([,accs ,inits] ...) ([,items ,lst-exprs] ...) ,body)
       (fold-handler
	`(for/fold ,(map list accs (map inductor inits))
	     ,(map list items (map inductor lst-exprs))
	   ,(inductor body)))]
      [`(,fn ,args ...)
       (let ([expr* (cons (symbol-table fn) (map inductor args))])
	 (if (memq fn predicates)
	     (predicate expr*)
	     (primitive expr*)))]
      [_ (error "malformed expression:" expr)]))

  (inductor expr))

(define (program-induct
         prog
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity] #:predicate [predicate identity]
	 #:let [let-handler identity] #:fold [fold-handler identity])

  (toplevel `(λ ,(program-variables prog)
	       ,(expression-induct (program-body prog)
				   #:constant constant
				   #:variable variable
				   #:primitive primitive
				   #:symbol symbol-table
				   #:predicate predicate
				   #:let let-handler
				   #:fold fold-handler))))

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

(define (location-parent loc)
  (reverse (cdr (reverse loc))))

(define (location-sibling loc)
  (if (<= (length loc) 1)
      #f
      (let ([loc* (reverse loc)])
        (cond
         [(= (car loc*) 1)
          (reverse (cons 2 (cdr loc*)))]
         [(= (car loc*) 2)
          (reverse (cons 1 (cdr loc*)))]
         [else
          #f]))))

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

(define (compile expr [initial-compilations (make-hash)])
  (define assignments '())
  (define compilations initial-compilations)

  (define (add-assignment! register expr)
    (set! assignments (cons (list register expr) assignments))
    register)

  ;; TODO : use one of Racket's memoization libraries
  (define (compile-one expr)
    (hash-ref!
     compilations expr
     (λ ()
       (match expr
	 ;; For variables or constants, don't replace with anything
	 ;; but itself.
	 [(? (negate list?)) expr]
	 ;; For let bindings, compile the right hand sides as
	 ;; expressions, and then bind the variables to the registers
	 ;; that coorespond to their expressions. Finally, recurse on
	 ;; body of let.
	 [`(let ([,vars ,vals] ...) ,body)
	  (let ([val-regs (map compile-one vals)])
	    (for ([var vars] [val-reg val-regs])
	      (hash-set! compilations var val-reg))
	    (compile-one body))]
	 ;; For folds, register compile the intitial expressions for
	 ;; the accs and sequences for iterating over, and bind them
	 ;; properly in the new for/fold, and then recurse on body. We
	 ;; have to do some special handling for multiple-values.
	 [`(for/fold ([,accs ,inits] ...)
	       ([,items ,lsts] ...)
	     (values . ,ret-vals))
	  (let ([init-regs (map compile-one inits)]
		[lst-regs (map compile-one lsts)])
	    `(for-wrapper
	      (for/fold ,(map list accs init-regs)
		  ,(map list items lst-regs)
		,(let ([naive-compiled (compile `(values . ,ret-vals) compilations)])
		   (match naive-compiled
		     [`(let* ([,regs ,exprs] ...)
			 ,ret-reg)
		      (match (car (take-right exprs 1))
			[`(values . ,ret-regs)
			 `(let* ,(map list (drop-right regs 1) (drop-right exprs 1))
			    (values . ,ret-regs))])])))))]
	 [`(for/fold ([,accs ,inits] ...)
	       ([,items ,lsts] ...)
	     ,body)
	  (let ([init-regs (map compile-one inits)]
		[lst-regs (map compile-one lsts)])
	    `(for-wrapper
	      (for/fold ,(map list accs init-regs)
		  ,(map list items lst-regs)
		,(compile body compilations))))]
	 ;; For anything else, compile subexpressions, and then bind
	 ;; new register to function on old registers. 
	 [`(,fn ,args ...)
	  (let ([arg-regs (map compile-one args)]
		[register (gensym "r")])
	    (add-assignment! register (cons fn arg-regs)))]
	 ;; If we get here, we had to have had a list with a single
	 ;; element, I think, which is malformed.
	 [_ (error "malformed expr:" expr)]))))

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

(define (replace-subexpr prog needle needle*)
  `(λ ,(program-variables prog)
     ,(replace-expr-subexpr (program-body prog) needle needle*)))

(define (replace-expr-subexpr haystack needle needle*)
  (cond [(equal? haystack needle) needle*]
	[(list? haystack)
	 (cons (car haystack) (map (curryr replace-expr-subexpr needle* needle)
				   (cdr haystack)))]
	[#t haystack]))

(define (loop-accs loop-expr)
  (match loop-expr
    [`(for/fold ([,accs _]...)
          ([_ _]...)
        _)
     accs]
    [_ (error "not a loop expression")]))

;; Returns a list containing:
;; * the accumulators
;; * the inits
;; * the item names
;; * the lists
;; * the update expressions
(define (loop-parts loop-expr)
  (match-let ([`(for/fold ([,accs ,inits] ...)
                    ([,items ,lsts] ...)
                  ,body)
               loop-expr])
    (list accs inits items lsts
          (match body
            [`(values . ,update-exprs) update-exprs]
            [update-expr (list update-expr)]))))

(define (make-loop accs inits items lsts update-exprs)
  `(for/fold ,(map list accs inits)
     ,(map list items lsts)
     ,(if (= 1 (length accs))
          (car update-exprs)
          `(values . ,update-exprs))))
