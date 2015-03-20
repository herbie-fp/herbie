#lang racket

(require math/bigfloat)
(require math/flonum)
(require "common.rkt")
(require "syntax.rkt")

(provide (all-from-out "syntax.rkt")
         location-induct program-induct expression-induct location-hash
         expression-induct*
         location-do location-get location-parent location-sibling
         eval-prog replace-subexpr expr-size exact-eval
	 compile expression-cost program-cost
         free-variables replace-expression
         do-parts do-list-parts loop-common-parts
         make-do-list make-do make-loop loop-subexpr:locs)

(define (location-induct
	 prog
	 #:toplevel [toplevel (λ (expr location) expr)] #:constant [constant (λ (c location) c)]
	 #:variable [variable (λ (x location) x)] #:primitive [primitive (λ (list location) list)]
	 #:symbol [symbol-table (λ (sym location) sym)] #:predicate [predicate (λ (pred loc) pred)]
	 #:let [let-handler (λ (expr location) expr)] #:loop [loop-handler (λ (expr location) expr)])

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
      ;; Do does pretty much the same thing as let, where inits,
      ;; update expressions, loop conditionals, and return expressions
      ;; all fall under expressions. So, in a loop like:
      ;;
      ;; (do ([sum 0.0 (+ sum i)]
      ;;      [i 1.0 (+ i 1)])
      ;;     (< i 10)
      ;;   (* 2 sum))
      ;;
      ;; The location of the 0.0 is '(1), the location of (+ sum i) is
      ;; '(2), the location of 1.0 is '(3), the location of (+ i 1) is
      ;; '(4), the location of (< i 10) is '(5), and the location of
      ;; (* 2 sum) is '(6).
      [`(do ([,accs ,init-exprs ,update-exprs] ...)
            ,while-expr
          ,ret-expr)
       (loop-handler
        `(do ,(for/list ([acc accs] [init init-exprs] [update-expr update-exprs]
                         [loc-t (in-naturals)])
                (list acc (inductor init (cons (add1 (* loc-t 2)) location))
                      (inductor update-expr (cons (+ 2 (* loc-t 2)) location))))
             ,(inductor while-expr (cons (add1 (* 2 (length accs))) location))
           ,(inductor ret-expr (cons (+ 2 (* 2 (length accs))) location)))
        (reverse location))]
      [`(do-list ([,accs ,init-exprs ,update-exprs] ...)
                 ([,items ,lsts] ...)
                 ,ret-expr)
       (loop-handler
        `(do-list ,(for/list ([acc accs] [init init-exprs] [update-expr update-exprs]
                              [loc-t (in-naturals)])
                     (list acc (inductor init (cons (add1 (* loc-t 2)) location))
                           (inductor update-expr (cons (+ 2 (* loc-t 2)) location))))
                  ,(for/list ([item items] [lst lsts]
                              [loc (sequence-map (curryr cons location) (in-naturals 1))])
                     (list item (inductor lst loc)))
                  ,(inductor ret-expr (cons (+ 1 (* 2 (length accs)) (length items)) location)))
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
	 #:let [let-handler identity] #:loop [loop-handler identity])

  (expression-induct*
   expr
   #:constant (λ (e* e) (constant e*))
   #:variable (λ (e* e) (variable e*))
   #:primitive (λ (e* e) (primitive e*))
   #:symbol (λ (e* e) (symbol-table e*))
   #:predicate (λ (e* e) (predicate e*))
   #:let (λ (e* e) (let-handler e*))
   #:loop (λ (e* e) (loop-handler e*))))

(define (first x y) x)

(define (expression-induct*
	 expr
         #:constant [constant first] #:variable [variable first]
	 #:primitive [primitive first] #:symbol [symbol-table first]
	 #:predicate [predicate first]
	 #:let [let-handler first] #:loop [loop-handler first])

  (define (inductor expr)
    (match expr
      [(? constant?) (constant expr expr)]
      [(? variable?) (variable expr expr)]
      [`(let ([,vars ,vals] ...) ,body)
       (let-handler
	`(let ,(map list vars (map inductor vals))
	   ,(inductor body))
        expr)]
      [`(do ([,accs ,init-exprs ,update-exprs] ...)
            ,while-expr
          ,ret-expr)
       (loop-handler
        `(do ,(map list accs (map inductor init-exprs) (map inductor update-exprs))
             ,(inductor while-expr)
           ,(inductor ret-expr))
         expr)]
      [`(do-list ([,accs ,init-exprs ,update-exprs] ...)
                 ([,items ,lsts] ...)
                 ,ret-expr)
       (loop-handler
         `(do-list ,(map list accs (map inductor init-exprs) (map inductor update-exprs))
                   ,(map list items (map inductor lsts))
                   ,(inductor ret-expr))
         expr)]
      [`(,fn ,args ...)
       (let ([expr* (cons (symbol-table fn fn) (map inductor args))])
	 (if (memq fn predicates)
	     (predicate expr* expr)
	     (primitive expr* expr)))]
      [_ (error "malformed expression:" expr)]))

  (inductor expr))

(define (program-induct
         prog
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity] #:predicate [predicate identity]
	 #:let [let-handler identity] #:loop [loop-handler identity])

  (toplevel `(λ ,(program-variables prog)
	       ,(expression-induct (program-body prog)
				   #:constant constant
				   #:variable variable
				   #:primitive primitive
				   #:symbol symbol-table
				   #:predicate predicate
				   #:let let-handler
				   #:loop loop-handler))))

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
  (define (list-do lst idx)
    (let loop ([idx (car loc)] [lst prog])
      (if (= idx 0)
          (cons (location-do (cdr loc) (car lst) f) (cdr lst))
          (cons (car lst) (loop (- idx 1) (cdr lst))))))
  (if (null? loc) (f prog)
      (let ([next-step (car loc)])
        (match prog
          [`(let ([,vars ,vals]...) ,body)
           (if (next-step . < . (length vars))
               `(let ,(map list vars (list-do vals next-step))
                  ,body)
               `(let ,(map list vars vals)
                  ,(location-do (cdr loc) body f)))]
          [`(do ([,accs ,init-exprs ,update-exprs] ...)
                ,while-expr
              ,ret-expr)
           (cond [(next-step . <= . (* 2 (length accs)))
                  (if (= 1 (modulo next-step 2))
                      `(do ,(map list accs (list-do init-exprs (/ next-step 2)) update-exprs)
                           ,while-expr
                         ,ret-expr)
                      `(do ,(map list accs init-exprs (list-do update-exprs (sub1 (/ next-step 2))))
                           ,while-expr
                         ,ret-expr))]
                 [(next-step . = . (length accs))
                  `(do ,(map list accs init-exprs update-exprs)
                       ,(location-do (cdr loc) while-expr f)
                     ,ret-expr)]
                 [#t `(do ,(map list accs init-exprs update-exprs)
                          ,while-expr
                        ,(location-do (cdr loc) ret-expr f))])]
          [`(do-list ([,accs ,init-exprs ,update-exprs] ...)
                     ([,items ,lsts] ...)
                     ,ret-expr)
           (cond [(next-step . <= . (* 2 (length accs)))
                  (if (= 1 (modulo next-step 2))
                      `(do-list ,(map list accs (list-do init-exprs (/ next-step 2))
                                      update-exprs)
                           ,(map list items lsts)
                         ,ret-expr)
                      `(do-list ,(map list accs init-exprs
                                      (list-do update-exprs (sub1 (/ next-step 2))))
                           ,(map list items lsts)
                         ,ret-expr))]
                 [(next-step . <= . (+ (* 2 (length accs)) (length lsts)))
                  `(do-list ,(map list accs init-exprs update-exprs)
                       ,(map list items (list-do lsts (- next-step (* 2 (length accs))) update-exprs))
                     ,ret-expr)]
                 [#t `(do-list ,(map list accs init-exprs update-exprs)
                          ,(map list items lsts)
                        ,(location-do (cdr loc) ret-expr f))])]
          [`(,f . ,args)
           (list-do prog next-step)]))))

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
      ;; Programs can now fail
      ;; (with-handlers ([exn:fail? (λ _ +nan.0)])
      (->flonum (apply fn (map real->precision pts))))));)

(define (exact-eval expr vars)
  (let* ([expr* (expression-induct expr #:constant ->bf
                                #:symbol (λ (op) (list-ref (hash-ref (*operations*) op) mode:bf)))]
         [prog `(λ ,vars ,(compile expr*))]
         [fn (eval prog common-eval-ns)])
    (λ (pts)
      (apply fn (map ->bf pts)))))

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
         ;; For loops, don't try to do common subexpression
         ;; elimination, instead just desugar do and do-list into
         ;; racket do and for/fold respectively.
         [`(do ([,accs ,init-exprs ,update-exprs] ...)
               ,while-expr
             ,ret-expr)
          (let ([idx (gensym "i")])
            `(let-values ([,accs
                           (for/fold ,(map list accs (map compile-one init-exprs))
                               ([,idx (in-range ,(*max-loop-iters*))])
                             #:break (not ,while-expr)
                             (values . ,(for/list ([update-expr update-exprs])
                                          (compile update-expr (hash-copy compilations)))))])
               ,(compile ret-expr compilations)))]
         [`(do-list ([,accs ,init-exprs ,update-exprs] ...)
                    ([,items ,lsts] ...)
                    ,ret-expr)
          `(let-values ([,accs
                         (for/fold ,(map list accs (map compile-one init-exprs))
                             ,(map list items lsts)
                           (values . ,(for/list ([update-expr update-exprs])
                                        (compile update-expr (hash-copy compilations)))))])
             ,(compile ret-expr initial-compilations))]
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

;; Returns a list containing:
;; * the accumulators
;; * the inits
;; * the updaters
;; * the while-expression
;; * the return expression.
(define (do-parts do-expr)
  (match-let ([`(do ([,accs ,inits ,updates] ...)
                    ,while-expr
                  ,ret-expr)
               do-expr])
    (list accs inits updates while-expr ret-expr)))

;; Returns a list containing:
;; * the accumulators
;; * the inits
;; * the updaters
;; * the items names
;; * the lists
;; * the return expression
(define (do-list-parts do-list-expr)
  (match-let ([`(do-list ([,accs ,inits ,updates] ...)
                         ([,items ,lsts] ...)
                         ,ret-expr)
               do-list-expr])
    (list accs inits updates items lsts ret-expr)))

;; Returns, for any loop, a list containing:
;; * the accumulators
;; * the inits
;; * the updaters
;; * the return expression
(define (loop-common-parts loop-expr)
  (match loop-expr
    [`(do ([,accs ,inits ,updates] ...)
          ,_
        ,ret-expr)
     (list accs inits updates ret-expr)]
    [`(do-list ([,accs ,inits ,updates] ...)
               ([,_ ,_] ...)
               ,ret-expr)
     (list accs inits updates ret-expr)]))

(define (make-do-list accs inits update-exprs items lsts ret-expr)
  `(do-list ,(map list accs inits update-exprs)
            ,(map list items lsts)
            ,ret-expr))

(define (make-do accs inits update-exprs condition ret-expr)
  `(do ,(map list accs inits update-exprs)
       ,condition
     ,ret-expr))

(define (make-loop template accs inits update-exprs ret-expr)
  (match template
    [`(do ([,_ ,_ ,_] ...)
          ,cond
        ,_)
     `(do ,(map list accs inits update-exprs)
          ,cond
        ,ret-expr)]
    [`(do-list ([,_ ,_ ,_] ...)
               ([,items ,lsts] ...)
               ,_)
     `(do-list ,(map list accs inits update-exprs)
               ,(map list items lsts)
               ,ret-expr)]))

(define (loop-subexpr:locs lp-expr [base-loc '()])
  (match lp-expr
    [`(do ([,_ ,inits ,steps] ...)
          ,cond
        ,ret-expr)
     (append
      (for/list ([init inits] [i (in-naturals)])
        (list init (append base-loc (list (add1 (* 2 i)))))
      (for/list ([step steps] [i (in-naturals)])
        (list init (append base-loc (list (+ 2 (* 2 i))))))
      (list cond (append base-loc (list (add1 (* 2 (length inits))))))
      (list ret-expr (append base-loc (list (+ 2 (* 2 (length inits))))))))]
    [`(do-list ([,_ ,inits ,steps] ...)
               ([,_ ,lists] ...)
               ,ret-expr)
     (append
      (for/list ([init inits] [i (in-naturals)])
        (list init (append base-loc (list (add1 (* 2 i)))))
      (for/list ([step steps] [i (in-naturals)])
        (list init (append base-loc (list (+ 2 (* 2 i))))))
      (for/list ([lst lists] [i (in-naturals)])
        (list lst (append base-loc (list (+ i 1 (* 2 (length inits)))))))
      (list ret-expr (append base-loc (list (+ 1 (length lists) (* 2 (length inits))))))))]))

(define (expr-size expr)
  (define size 0)
  (expression-induct expr #:primitive (λ _ (set! size (add1 size))))
  size)
