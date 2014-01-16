#lang racket

(require racket/match)
(require racket/flonum)
(require racket/pretty)
(require data/order)
(require math/bigfloat)
(require plot)

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

(define (list= l1 l2)
  (and l1 l2 (andmap = l1 l2)))

(define (list< list1 list2)
  "Compares lists lexicographically."
  ; Who picked this terrible API design of returning '< or '>
  (eq? (datum-order list1 list2) '<))

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
    (let ([hash (make-hash)])
      (for ([rec operations])
        (hash-set! hash (car rec) (cdr rec)))
      hash)))

(define mode:bf 0)
(define mode:fl 1)

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

;; We usually want to show the "Top N" alternatives.

(define (take-up-to l k)
  ; This is unnecessarily slow. It is O(l), not O(k).
  ; To be honest, it just isn't that big a deal for now.
  (take l (min k (length l))))

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

(define (max-error prog pts-list exacts)
  "Find the maximum error in a function's approximate evaluations at the given points
   (compared to the given exact results), and the number of evaluations that yield
   a special value."
  (let ([errors
         (let ([fn (eval-prog prog mode:fl)])
           (for/list ([pts pts-list] [exact exacts])
             (cons (relative-error (fn pts) exact) (cons pts exact))))])
    (let loop ([max-err 0] [specials 0] [errors errors] [max-err-pt #f] [max-err-ex #f])
      (if (null? errors)
          (values max-err specials max-err-pt max-err-ex)
          (if (and (ordinary-float? (caar errors)) (< (caar errors) 1.0))
              (if (> max-err (caar errors))
                  (loop max-err specials (cdr errors) max-err-pt max-err-ex)
                  (loop (caar errors) specials (cdr errors) (cadar errors) (cddar errors)))
              (loop max-err (+ specials 1) (cdr errors) max-err-pt max-err-ex))))))

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

;; Now to implement a search tool to find the best expression
;;
;; This is an A* search internally.

(define (heuristic-search start generator chooser make-alternative iterations)
  "Search for a better version of start,
   where generator creates new versions of a program to try,
   chooser picks a candidate to generate versions from,
   and make-alternative generates converts programs into alternatives."

  (define (step options done)
    (let*-values ([(parent rest) (chooser options)]
                  [(children)    (generator (alternative-program parent))])
      (values
       (append ; This is never precisely sorted, but it is always close
        rest
        (filter (λ (x) (not (or (member x rest) (member x done))))
                (map make-alternative children)))
       (cons parent done))))

  (let loop ([options (list (make-alternative start))]
             [done '()])
    (if (or (null? options)
            (>= (length done) iterations))
        (values options done)
        (let-values ([(options* done*)
                      (step options done)])
          (loop options* done*)))))

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

(struct alternative (program error specials cost) #:transparent)

;; TODO : think up a good scoring function
(define (alternative-score alt)
  "Measures how good a program is; lower is better.  Returns a list, to be sorted with list<."

  (list
   (+ (alternative-specials alt)
      (log (max (alternative-error alt) 1e-50))
      (* 0.01 (alternative-cost alt)))
   (alternative-error alt)
   (alternative-specials alt)
   (alternative-cost alt)))

(define (choose-min-error alts)
  "Choose the alternative with the least error"
  ; Invariant: alts is nonempty
  (let ([alts* (sort alts #:key alternative-score list<)])
    (values (car alts*) (cdr alts*))))

;; Now that we've defined the intermediate representation, we can
;; run the A* search with the alternatives structures.

(define (heuristic-execute prog iterations)
  (let*-values ([(pts exacts) (prepare-points prog)]
                [(evaluate) (curryr max-error pts exacts)]
                [(make-alternative)
                 (λ (prog)
                    (let-values ([(err specials pt ex) (evaluate prog)])
                      (alternative prog err specials (program-cost prog))))]
                [(generate) (λ (prog)
                             (let ([body (program-body prog)]
                                   [vars (program-variables prog)])
                               (map (λ (body*) `(λ ,vars ,body*))
                                    (remove-duplicates
                                     (rewrite-tree vars body)))))])
    (heuristic-search prog generate choose-min-error make-alternative iterations)))

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
    (let-values ([(err specials pt ex) (max-error (alternative-program prog) points exacts)])
      (cons pt ex)))
  
  (define (step prog input)
    (let ([annot (analyze-expressions (alternative-program prog) input)])
      (map make-alternative (rewrite-at-location (alternative-program prog)
                                                 (find-most-local-error annot)))))
  
  (define (make-alternative prog)
    (let-values ([(err specials pt ex) (max-error prog points exacts)])
      (alternative prog err specials (program-cost prog))))

  (define start-prog (make-alternative prog))

  (let loop ([good-prog start-prog] [test-prog start-prog] [left iters]
             [pt&ex (pick-input start-prog)])
    (if (= left 0)
        good-prog
        (let* ([alts (step test-prog (car pt&ex))]
               [alts* (sort alts #:key alternative-score list<)]
               [new-prog (car alts*)])
          (cond
           [(null? alts*)
            good-prog]
           [(< (relative-error (cdr pt&ex) (eval-prog (alternative-program new-prog) mode:fl))
               (relative-error (cdr pt&ex) (eval-prog (alternative-program good-prog) mode:fl)))
            (loop new-prog new-prog iters (pick-input new-prog))]
           [#t
            (loop good-prog new-prog (- left 1) pt&ex)])))))

;; For usage at the REPL, we define a few helper functions.
;;
;; PROGRAM-A and PROGRAM-B are two example programs to test.
;; (explore prog iters) returns a list of alternatives found
;; (improve prog iters) prints the found alternatives

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (explore prog iterations)
  (let-values ([(options done) (heuristic-execute prog iterations)])
    (sort (append options done) #:key alternative-score list<)))

(define (print-alternatives alts)
  (for ([alt alts])
    (display "; Alternative with score ")
    (display (alternative-score alt))
    (newline)
    (pretty-print (alternative-program alt))))

(define (improve prog iterations)
  (print-alternatives (take-up-to (explore prog iterations) 5)))

(define (plot-alternatives prog iterations)
  "Return a spectrum plot of the alternatives found."
  (let* ([alts (explore prog iterations)]
         [logs (map (lambda (x) (- (/ (log (alternative-error x)) (log 10)))) alts)]
         [rands (for/list ([i (range (length logs))]) (random))])
    (display "Found program with score ")
    (display (alternative-score (car alts)))
    (newline)
    (pretty-print (alternative-program (car alts)))
    (parameterize ([plot-width 800] [plot-height 100]
                   [plot-x-label #f] [plot-y-label #f])
      (plot (points (map vector logs rands))))))

(provide (all-defined-out))
