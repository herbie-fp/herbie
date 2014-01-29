#lang racket

(require racket/match)
(require racket/flonum)
(require racket/pretty)
(require data/order)
(require math/bigfloat)
(require plot)

(define (println . args)
  (for ([val args])
    (if (string? val)
        (display val)
        (print val)))
  (newline))

;; Precision standards

; Precision for approximate evaluation
(define *precision* real->double-flonum)

; Functions used by our benchmarks
(define (cotan x)
  (/ 1 (tan x)))

(define (square x)
  (* x x))

(define (ordinary-float? x)
  (not (or (infinite? x) (nan? x))))

(define-syntax (reap stx)
  "A reap/sow abstraction for filters and maps."
  (syntax-case stx ()
    [(_ [sow] body ...)
     #'(let* ([store '()]
              [sow (λ (elt) (set! store (cons elt store)))])
         body ...
         (reverse store))]))

(define (=-or-nan? x1 x2)
  (or (= x1 x2)
      (and (nan? x1) (nan? x2))))

(define (list= l1 l2)
  (and l1 l2 (andmap =-or-nan? l1 l2)))

(define (list< list1 list2)
  "Compares lists lexicographically."
  ; Who picked this terrible API design of returning '< or '>
  (eq? (datum-order list1 list2) '<))

(define (enumerate . l)
  (apply map list (range (length (car l))) l))

;; We usually want to show the "Top N" alternatives.

(define (take-up-to l k)
  ; This is unnecessarily slow. It is O(l), not O(k).
  ; To be honest, it just isn't that big a deal for now.
  (take l (min k (length l))))

;; Different modes in which we evaluate expressions

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
           [*var*   ,bf      ,*precision* 0])])

    ; Munge the table above into a hash table.
    (let ([hash (make-hasheq)])
      (for ([rec table])
        (hash-set! hash (car rec) (cdr rec)))
      hash)))

(define mode:bf 0)
(define mode:fl 1)

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

;; We evaluate a program by comparing its results computed with floating point
;; to its results computed with arbitrary precision.

(define (relative-error approx exact)
  (if (and (real? approx) (real? exact))
      (abs (/ (- exact approx) (max (abs exact) (abs approx))))
      +nan.0))

(define (->flonum x)
  (cond
   [(real? x) (*precision* x)]
   [(bigfloat? x) (*precision* (bigfloat->flonum x))]))

(define (program-induct
         prog
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity])

  (define (inductor prog)
    (cond
     [(real? prog) (constant prog)]
     [(symbol? prog) (variable prog)]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog))])
        (toplevel `(lambda ,(program-variables prog) ,body*)))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog)) (map inductor (cdr prog))))]
     [#t
      (error "Invalid program expression" prog)]))

  (inductor prog))

(define (location-induct
	 prog
	 #:toplevel [toplevel (λ (expr location) expr)] #:constant [constant (λ (c location) c)]
	 #:variable [variable (λ (x location) x)] #:primitive [primitive (λ (list location) list)]
	 #:symbol [symbol-table (λ (sym location) sym)])
  (define (inductor prog location)
    (cond
     [(real? prog) (constant prog location)]
     [(symbol? prog) (variable prog location)]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog) location)])
	(toplevel `(λ ,(program-variables prog) ,body*) location))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog) (cons 'car location))
		       (location-map (λ (prog loc)
					(inductor prog (append (cons 'car (cons 'cdr loc)) location)))
				     (cdr prog)))
		 location)]))
  (inductor prog '()))

(define (location-map fun list)
  (letrec ([loc-map (λ (list fun acc location)
		       (if (null? list)
			   acc
			   (loc-map (cdr list) fun 
				    (cons (fun (car list) location) acc)
				    (cons 'cdr location))))])
    (reverse (loc-map list fun '() '()))))

(define-namespace-anchor eval-prog-ns-anchor)
(define eval-prog-ns (namespace-anchor->namespace eval-prog-ns-anchor))

(define (eval-prog prog mode)
  (let* ([real->precision (list-ref (hash-ref operations '*var*) mode)]
         [op->precision (lambda (op) (list-ref (hash-ref operations op) mode))]
         [prog* (program-induct prog #:constant real->precision #:symbol op->precision)]
         [fn (eval prog* eval-prog-ns)])
    (lambda (pts)
      (->flonum (apply fn (map real->precision pts))))))

; We evaluate  a program on random floating-point numbers.
; Right now we generate these input points by dividing the
; range of exponents into buckets, and generating a random
; point in each bucket.
;
; This is not quite uniform on the floating point numbers,
; but it is reasonably close, and it makes sure to test our
; points on a range of values.
;
; (prepare-points prog) generates not only the input values
; for a program but also the exact values for it.  These are
; computed with arbitrary-precision arithmetic.

(define (prepare-points prog)
  "Given a program, return two lists:
   a list of input points (each a list of flonums)
   and a list of exact values for those points (each a flonum)"

  (define (exp->pt bucket-number bucket-width)
    "Given an exponential bucket"
    (expt 2 (- (* bucket-width (+ bucket-number (random))) 126)))

  (define (list-cartesian-power lst repetitions)
    "Returns a list, each element of which is a list
     of `repetitions` elements of `lst`"

    (if (= repetitions 1)
        (map list lst)
        (let ([tails (list-cartesian-power lst (- repetitions 1))])
          (for*/list ([head lst] [tail tails])
            (cons head tail)))))

  ; The bucket width for a given number of dimensions
  (define bucket-width-per-dim '(: 1 6 15 25 35 45))

  (define (make-points dim)
    "Make a list of flonums.  The list spans a large range of values"

    (let* ([bucket-width (list-ref bucket-width-per-dim dim)]
           [num-buckets (floor (/ 253 bucket-width))]
           [bucket-indices (range 0 num-buckets)]
           [pts+ (map (curryr exp->pt bucket-width) bucket-indices)]
           [pts (append pts+ (map - pts+))])
      (list-cartesian-power pts dim)))

  (bf-precision 256)

  (define (make-exacts prog pts)
    "Given a list of arguments,
     produce a list of exact evaluations of a program-at those arguments
     using true arbitrary precision.  That is, we increase the bits
     available until the exact values converge.
     Not guaranteed to terminate."
    (let ([f (eval-prog prog mode:bf)])
      (let loop ([prec 64] [prev #f])
        (bf-precision prec)
        (let ([curr (map f pts)])
          (if (list= prev curr)
              curr
              (loop (+ prec 16) curr))))))

  (define (filter-points pts exacts)
    "Take only the points for which the exact value is normal"
    (reap (sow)
      (for ([pt pts] [exact exacts])
        (when (ordinary-float? exact)
          (sow pt)))))

  (define (filter-exacts pts exacts)
    "Take only the exacts for which the exact value is normal"
    (filter ordinary-float? exacts))

  ; These definitions in place, we finally generate the points.

  ; First, we generate points;
  (let* ([pts (make-points (length (program-variables prog)))]
         [exacts (make-exacts prog pts)]
         ; Then, we remove the points for which the answers
         ; are not representable
         [pts* (filter-points pts exacts)]
         [exacts* (filter-exacts pts exacts)])
    (values pts* exacts*)))

(define (errors prog points exacts)
  (let ([fn (eval-prog prog mode:fl)])
    (for/list ([point points] [exact exacts])
      (relative-error (fn point) exact))))

(define errors-compare-cache (make-hash))

(define (errors-compare errors1 errors2)
  (hash-ref!
   (hash-ref! errors-compare-cache errors1 make-hash)
   errors2
   (λ ()
      (for/list ([error1 errors1] [error2 errors2])
        (cond
         [(and (ordinary-float? error1) (ordinary-float? error2))
          (cond
           [(< error1 error2) '<]
           [(= error1 error2) '=]
           [(> error1 error2) '>]
           [#t (error "Cannot compare error1 and error2" error1 error2)])]
         [(or (and (ordinary-float? error1) (not (ordinary-float? error2))))
          '<]
         [(or (and (not (ordinary-float? error1)) (ordinary-float? error2)))
          '>]
         [(and (infinite? error1) (infinite? error2))
          '=]
         [(and (infinite? error1) (nan? error2))
          '<]
         [(and (nan? error1) (infinite? error2))
          '>]
         [(and (nan? error1) (nan? error2))
          '=]
         [#t (error "Failed to classify error1 and error2" error1 error2)])))
      ))

;; Now we define our rewrite rules.

(define-syntax (match-all stx)
  ; Like (match), but returns a list of all possible rewrites
  (syntax-case stx ()
    [(_ value [pattern expansion] ...)
     #'(append
        (match value
          [pattern (list expansion)]
          [_ '()])
        ...)]))

(define (rewrite-expression vars expr)
  ; (List symbol) → expr → (List expr)
  (match-all expr
    ; Associativity
    [`(+ ,a (+ ,b ,c)) `(+ (+ ,a ,b) ,c)]
    [`(+ (+ ,a ,b) ,c) `(+ ,a (+ ,b ,c))]
    [`(- (+ ,a ,b) ,c) `(+ ,a (- ,b ,c))]
    [`(+ ,a (- ,b ,c)) `(- (+ ,a ,b) ,c)]
    ; Distributivity
    [`(* ,a (+ ,b ,c)) `(+ (* ,a ,b) (* ,a ,c))]
    [`(+ (* ,a ,b) (* ,a ,c)) `(* ,a (+ ,b ,c))]
    ; Commutativity
    [`(+ ,a ,b) `(+ ,b ,a)]
    ; Identity
    [`(- ,a ,a) 0]
    [`(+ ,a 0) a]
    ; Square root
    [`(square (sqrt ,x)) x]
    [x `(square (sqrt ,x))]
    ; Exponentials
    [x `(exp (log ,x))]
    [x `(log (exp ,x))]
    [`(exp (log ,x)) x]
    [`(log (exp ,x)) x]
    ; Multiplying by x / x
    [`(+ ,a ,b)
     `(/ (- (square ,a) (square ,b)) (- ,a ,b))]
    [`(- ,a ,b)
     `(/ (- (square ,a) (square ,b)) (+ ,a ,b))]))

(define (rewrite-tree vars expr)
  "Return a list of expressions,
   each of which is `expr` but with one subexpression
   rewritten according to (rewrite-rules)."

  (define (recursively-apply->list f expr)
    ; (expr → List expr) → expr → (List expr)
    (program-induct expr
      #:constant
      (lambda (c) (cons c (f c)))
      #:variable
      (lambda (x) (cons x (f x)))
      #:toplevel
      (lambda (prog)
        (for/list ([alt (program-body prog)])
          `(lambda ,(program-variables prog) ,alt)))
      #:primitive
      (lambda (expr)
        (let ([oldexpr (cons (car expr) (map car (cdr expr)))])
          (cons oldexpr
                (append (f oldexpr)
                        (map (curry cons (car expr))
                             (list-join (cdr expr)))))))))

  (define (list-join x)
    ; This is basically completely trivial and needs no explanation.
    (apply append
           (let loop ([alts (car x)] [rest (cdr x)]
                      [prefix '()] [output '()])
             (let ([revprefix (reverse prefix)] [restcar (map car rest)])
               (let ([ans (for/list ([alt (cdr alts)])
                            (append revprefix (cons alt restcar)))])
                 (if (null? rest)
                     (cons ans output)
                     (loop (car rest) (cdr rest)
                           (cons (car alts) prefix)
                           (cons ans output))))))))

  ; Now we can recursively rewrite the expression

  (cdr (recursively-apply->list (curry rewrite-expression vars) expr)))

(define (rewrite-tree-changes vars expr)
  "Returns a list of expressions consed with change objects,
   each expression is `expr` but with one subexpression
   rewritten according to (rewrite-rules), and the change objects
   represent the rewrite made."

  (define (attach-changes original list location)
    "Loops across a given `list` of single-level alternatives to a
     given subexpression, and conses them with a change object.
     The change object assumes that the expression was originally
     `original`, and that it is located at `location`"
    (map (λ (item)
	    (cons item (change (reverse location) (cons original item) '() '() '() '())))
	 list))

  (define (recursively-apply->list f expr)
    "Takes an expression and a function which generates alternatives
     for subexpressions, and returns a list of all the possible
     [alternative expressions with one thing change, consed with a
     change object to represent the change]"
    (location-induct expr
		     #:constant
		     (λ (c l) (cons c (attach-changes c (f c) l)))
		     #:variable
		     (λ (x l) (cons x (attach-changes x (f x) l)))
		     #:toplevel
		     (λ (prog l)
			(for/list ([alt (program-body prog)])
			  `((λ ,(program-variables prog) ,(car alt)) . ,(cdr alt))))
		     #:primitive
		     (λ (expr l)
			(let ([oldexpr (cons (car expr) (map car (cdr expr)))])
			  (cons oldexpr
				(append (map (λ (item)
						(cons item (change (reverse l) (cons oldexpr item) '() '() '() '())))
					     (f oldexpr)) ;The alts involving changes on the whole expression
					(map (λ (e) (cons (cons (car expr) (car e)) (cdr e)))
					     (list-join (cdr expr))))))))) ;The alts involving changes on subexpressions
  (define (list-join x)
    "Takes a list of lists, and returns a list of alternatives.
     This assumes that each list is composed of first an original, 
     then a bunch of alternatives to the original. The alternatives
     generated are then each a list of one item from each of the
     original lists, where one of them is an alternative, and the
     rest are originals."
    (apply append
	   (let loop ([alts (car x)] [rest (cdr x)]
		      [prefix '()] [output '()])
	     (let ([revprefix (reverse prefix)] [restcar (map car rest)])
	       (let ([ans (for/list ([alt (cdr alts)])
			    (cons (append revprefix (cons (car alt) restcar)) (cdr alt)))])
		 (if (null? rest)
		     (cons ans output)
		     (loop (car rest) (cdr rest)
			   (cons (car alts) prefix)
			   (cons ans output))))))))

  ;;Now, recursively rewrite

  (cdr (recursively-apply->list (curry rewrite-expression vars) expr)))

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

;; To use this heuristic search mechanism, we'll need to implement a
;; few helper functions

(struct alternative (program errors cost changes) #:transparent)
(struct change (location rewrite preerrors precost posterrors postcost) #:transparent)

(define (alternative<>? alt1 alt2)
  "Compare two alternatives.
   Compares first by a lattice order on points, then by program cost."

  (let ([comparisons (errors-compare (alternative-errors alt1) (alternative-errors alt2))])
    (and (member '< comparisons) (member '> comparisons))))

(define (alternative<? alt1 alt2)
  "Compare two alternatives.
   Compares first by a lattice order on points, then by program cost."

  (let ([comparisons (errors-compare (alternative-errors alt1) (alternative-errors alt2))])
    (or (andmap (negate (curry eq? '>)) comparisons)
        (< (alternative-cost alt1) (alternative-cost alt2)))))

(define (alternative<-at? idx alt1 alt2)
  "Compare two alternatives.
   Compares first by a lattice order on points, then by program cost."
  ;(println "; Comparing " (alternative-program alt1) "\n;  and " (alternative-program alt2)
  ;         "\n;  errors " (list-ref (alternative-errors alt1) idx)
  ;         " and " (list-ref (alternative-errors alt2) idx))
  (eq? (list-ref (errors-compare (alternative-errors alt1) (alternative-errors alt2)) idx) '<))

;; Now that we've defined the intermediate representation, we can
;; searhc to find the best expression
;;
;; This is an A* search internally.

(define (brute-force-search prog iters points exacts)
  "Brute-force search for a better version of `prog`,
   giving up after `iters` iterations without progress"

  (define (make-alternative prog chng parent)
    (let ([parent-cost (alternative-cost parent)]
	  [parent-errors (alternative-errors parent)]
	  [child-cost (program-cost prog)]
	  [child-errors (errors prog points exacts)])
      (alternative prog child-errors child-cost (cons (change (change-location chng)
							      (change-rewrite chng)
							      parent-errors parent-cost
							      child-errors child-cost)
						      (alternative-changes parent)))))

  (define (init-alternative prog)
    "Create the initial alternative that doesn't have a parent"
    (alternative prog (errors prog points exacts) (program-cost prog) '()))

  (define (generate-alternatives alternative)
    (let ([body (program-body (alternative-program alternative))]
          [vars (program-variables (alternative-program alternative))])
      (map (λ (prog-change) (make-alternative (car prog-change) (cdr prog-change) alternative))
           (map (λ (body*-change) `((λ ,vars ,(car body*-change)) . ,(cdr body*-change)))
		(remove-duplicates
		 (rewrite-tree-changes vars body)
		 )))))

  (define (step options done)
    (define (duplicate? alt)
      (or (memf (λ (option) (equal? (program-body (alternative-program alt))
				    (program-body (alternative-program option))))
		options)
	  (memf (λ (done-item) (equal? (program-body (alternative-program alt))
				       (program-body (alternative-program done-item))))
		done)))
    
    (let* ([parent (car options)]
	   [parent-stripped (if (green? (car (alternative-changes parent)))
				(remove-red parent)
				parent)]
           [rest (cdr options)]
           [children (generate-alternatives parent-stripped)])
      (values
       (sort (append rest (filter (negate duplicate?) children)) alternative<?)
       (cons parent-stripped done))))

  (let loop ([options (list (init-alternative prog))]
             [done '()])
    (if (or (null? options)
            (>= (length done) iters))
        done
        (let-values ([(options* done*)
                      (step options done)])
          (loop options* done*)))))

(define (error-sum errors) (foldl (λ (x y) (+ x y)) 0 errors))
(define green-threshold 50)
(define (green? change)
  (< green-threshold (- (error-sum (change-posterror change))
			(error-sum (change-preerror change)))))

(define (remove-red alternative)
  alternative) ;;Eventually this should return an alternative with red changes undone.

(define (improve prog iterations)
  (define-values (points exacts) (prepare-points prog))

  (map alternative-program (sort (brute-force-search prog iterations points exacts) alternative<?)))

(struct annotation (expr exact-value approx-value local-error total-error) #:transparent)

(define (analyze-expressions prog inputs)
  (define varmap (map cons (program-variables prog) inputs))

  (define (real-op->bigfloat-op op)
    (list-ref (hash-ref operations op) mode:bf))

  (define (real-op->float-op op)
    (list-ref (hash-ref operations op) mode:fl))


  (program-induct
   prog

   #:constant
   (λ (c)
      (let* ([exact (bf c)] [approx (*precision* c)]
             [error (relative-error (->flonum exact) approx)])
        (annotation c exact approx error error)))

   #:variable
   (λ (v)
      (let* ([var (cdr (assoc v varmap))]
             [exact (bf var)] [approx (*precision* var)]
             [error (relative-error (->flonum exact) approx)])
        (annotation v exact approx error error)))

   #:primitive
   (λ (expr)
      (let* ([exact-op (real-op->bigfloat-op (car expr))]
             [approx-op (real-op->float-op (car expr))]
             [exact-inputs (map annotation-exact-value (cdr expr))]
             [semiapprox-inputs (map ->flonum exact-inputs)]
             [approx-inputs (map annotation-approx-value (cdr expr))]
             [exact-ans (apply exact-op exact-inputs)]
             [semiapprox-ans (apply approx-op semiapprox-inputs)]
             [approx-ans (apply approx-op approx-inputs)]
             [local-error (relative-error (->flonum exact-ans)
                                          semiapprox-ans)]
             [cumulative-error (relative-error (->flonum exact-ans)
                                               approx-ans)])
        (annotation expr exact-ans approx-ans local-error cumulative-error)))))

(define (find-most-local-error annot-prog)
  (define (search-expression expr loc-tail)
    (if (list? expr)
        (let continue ([expr (cdr expr)] [err 0] [loc loc-tail] [idx 1])
          (cond
           [(null? expr) (values err loc)]
           [#t
            (let-values ([(err* loc*) (search-annot (car expr) (cons idx loc-tail))])
              (if (> err* err)
                  (continue (cdr expr) err* loc* (+ idx 1))
                  (continue (cdr expr) err loc (+ idx 1))))]))
        (values 0 #f)))

  (define (search-annot annot loc)
    (let-values ([(err* loc*) (search-expression (annotation-expr annot) loc)])
      ; Why >=? Because all else equal, we're more interested in small subexpressions
      (if (>= err* (annotation-local-error annot))
          (values err* loc*)
          (values (annotation-local-error annot) loc))))

  (let-values ([(err loc) (search-annot (program-body annot-prog) '(2))])
    (if (= err 0) #f (reverse loc))))

(define (rewrite-at-location prog loc)
  (define vars (program-variables prog))

  (define (recursor exp loc)
    (if (null? loc)
        (rewrite-expression vars exp)
        (let ([front (take exp (car loc))] [tail (drop exp (+ (car loc) 1))]
              [middle (recursor (list-ref exp (car loc)) (cdr loc))])
          (for/list ([opt middle])
            (append front (cons opt tail))))))

  (if loc
      (recursor prog loc)
      (list prog)))

(define (improve-by-analysis prog iters points exacts)
  (define (pick-input prog)
    (argmax cadr (filter (λ (x) (< (cadr x) 1))
                  (enumerate (alternative-errors prog) points exacts))))

  (define (step prog input)
    (let ([annot (analyze-expressions (alternative-program prog) input)])
      (map make-alternative (rewrite-at-location (alternative-program prog)
                                                 (find-most-local-error annot)))))

  (define (make-alternative prog)
    (let ([errs (errors prog points exacts)])
      (alternative prog errs (program-cost prog) '())))

  (define start-prog (make-alternative prog))

  (let loop ([good-prog start-prog] [test-prog start-prog] [left iters]
             [input (pick-input start-prog)])
    (println "; Trying " (alternative-program test-prog) " at " (caddr input))
    (if (= left 0)
        good-prog
        (let* ([alts (step test-prog (caddr input))]
               [alts* (sort alts (curry alternative<-at? (car input)))]
               [new-prog (car alts*)])
          (cond
           [(null? alts*)
            good-prog]
           [(< (relative-error ((eval-prog (alternative-program new-prog) mode:fl) (caddr input))
                               (cadddr input) )
               (relative-error ((eval-prog (alternative-program good-prog) mode:fl) (caddr input))
                               (cadddr input)))
            (loop new-prog new-prog iters (pick-input new-prog))]
           [#t
            (loop good-prog new-prog (- left 1) input)])))))

;; For usage at the REPL, we define a few helper functions.
;;
;; PROGRAM-A and PROGRAM-B are two example programs to test.
;; (explore prog iters) returns a list of alternatives found
;; (improve prog iters) prints the found alternatives

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (print-alternatives alts)
  (for ([alt alts])
    (pretty-print (alternative-program alt))))

(define (print-improve prog iterations)
  (print-alternatives (take-up-to (improve prog iterations) 5)))

;(define (plot-alternatives prog iterations)
;  "Return a spectrum plot of the alternatives found."
;  (let* ([alts (explore prog iterations)]
;         [logs (map (lambda (x) (- (/ (log (alternative-error x)) (log 10)))) alts)]
;         [rands (for/list ([i (range (length logs))]) (random))])
;    (display "Found program with score ")
;    (display (alternative-score (car alts)))
;    (newline)
;    (pretty-print (alternative-program (car alts)))
;    (parameterize ([plot-width 800] [plot-height 100]
;                   [plot-x-label #f] [plot-y-label #f])
;      (plot (points (map vector logs rands))))))

(provide (all-defined-out))
