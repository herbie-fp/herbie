#lang racket

(require racket/match)
(require racket/flonum)
(require racket/pretty)
(require data/order)
(require math/bigfloat)
(require plot)

; 256 is a big number.
(bf-precision 256)

; Alternative: real->single-flonum
(define *precision* real->double-flonum)

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

; Functions used by our benchmarks
(define (cotan x)
  (/ 1 (tan x)))

(define (square x)
  (* x x))

;; We evaluate a program by comparing its results computed with floating point
;; to its results computed with arbitrary precision.

; TODO : This assumes that 100% relative error must be due to a "special occurance",
; which is a slack way to actually detect this condition.
(define (relative-error approx exact)
  (let ([ans
         (if (and (real? approx) (real? exact))
             (abs (/ (- exact approx) exact))
             +nan.0)])
    (if (or (= ans 1.0) (nan? ans))
        +nan.0
        ans)))

(define (real-op->bigfloat-op op)
  (hash-ref #hash([+ . bf+] [* . bf*] [- . bf-] [/ . bf/] [square . bfsqr]
                  [abs . bfabs] [sqrt . bfsqrt] [log . bflog] [exp . bfexp]
                  [expt . bfexpt] [sin . bfsin] [cos . bfcos] [tan . bftan]
                  [cotan . bfcot] [asin . bfasin] [acos . bfacos] [atan . bfatan]
                  [sinh . bfsinh] [cosh . bfcosh])
            op))

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

(define (eval-prog prog const-rule symbol-table)
  (let ([fn (eval (program-induct prog #:constant const-rule #:symbol symbol-table) eval-prog-ns)])
    (lambda (pts)
      (->flonum (apply fn (map const-rule pts))))))

; We evaluate  a program on random floating-point numbers.

(define (make-points dim)
  "Make a list of real numbers.  The list spans a large range of values"

  (define (exp->pt e m)
    (expt 2 (* m (+ e (random)))))

  (define (list-cartesian-power lst repetitions)
    (if (= repetitions 1)
        (map list lst)
        (let ([tails (list-cartesian-power lst (- repetitions 1))])
          (for*/list ([head lst] [tail tails])
            (cons head tail)))))

  ; Chosen emperically
  (define skip
    (list-ref '(1 3 6 10 15 20 25) dim))

  (define range-min (ceiling (/ -126 skip)))
  (define range-max (floor (/ 127 skip)))

  (let* ([idx (range range-min range-max)]
         [pts+ (map (curryr exp->pt skip) idx)]
         [pts (append pts+ (map - pts+))])
    (list-cartesian-power pts dim)))

(define (make-exacts prog pts)
  "Given a list of arguments, produce a list of exact evaluations of a program at those arguments"
  (map (eval-prog prog bf real-op->bigfloat-op) pts))
;  (map (eval-prog prog *precision* identity) pts))

(define (filter-points pts exacts)
  "Take only the points for which the exact value is normal"
  (map car (filter (lambda (x) (not (or (infinite? (cdr x)) (nan? (cdr x))))) (map cons pts exacts))))

(define (filter-exacts pts exacts)
  "Take only the exacts for which the exact value is normal"
  (filter (lambda (x) (not (or (infinite? x) (nan? x)))) exacts))

(define (max-error prog pts-list exacts)
  "Find the maximum error in a function's approximate evaluations at the given points
   (compared to the given exact results), and the number of evaluations that yield
   a special value."
  (let ([errors
         (let ([fn (eval-prog prog *precision* identity)])
           (for/list ([pts pts-list] [exact exacts])
             (relative-error (fn pts) exact)))])
    (let loop ([max-err 0] [specials 0] [errors errors])
      (if (null? errors)
          (values max-err specials)
          (if (or (infinite? (car errors)) (nan? (car errors)))
              (loop max-err (+ specials 1) (cdr errors))
              (loop (max max-err (car errors)) specials (cdr errors)))))))

;; Our main synthesis tool generates alternatives to an expression uses recursive rewrite tools

(define (apply-to-children-collect fn exp)
  "Given an S-expression exp, generate all versions where fn has been applied to an argument."
  (cond
    [(not (list? exp)) (fn exp)]
    [#t
     (let loop ([start (list (car exp))] ; Reversed prefix of the expression.  We skip the applicator.
                [end (cdr exp)] ; Suffix of the expression
                [out '()]) ; Versions already generated
       (if (null? end)
           out
           (let ([new-versions
                  ; fn returns a list of new versions
                  (map (λ (x) (append (reverse start) (cons x (cdr end))))
                       (fn (car end)))])
             (loop
              (cons (car end) start)
              (cdr end)
              (append
               new-versions ; Since new-versions is a list, we append it on
               out)))))]))

(define-syntax recursive-match
  (λ (stx)
    "Returns a list of all possible new results from applying a set of rewrite rules to some subexpression of a program"
    (syntax-case stx ()
      ; Syntax looks like (recursive-match expr [`(,my pat) `(,my result)])
      [(_ value [pattern expansion] ...)
       ; Turns into ...
       #'(letrec
             ; matcher returns a list of possible results from a single expression (no recursion)
             [(matcher
               (λ (exp)
                 (append
                  ; Collect the results of each individual match
                  (match exp
                    ; If match, return Just result
                    [pattern (list expansion)]
                    ; If no match, return None
                    [_ '()])
                  ...)))
              ; walker applies matcher at every level of the expression
              (walker
               (λ (exp)
                 (if (not (list? exp))
                     (matcher exp)
                     (append
                      ; Apply at the top-level
                      (matcher exp)
                      ; Apply to all subexpressions
                      (apply-to-children-collect walker exp)))))]
           ; Finally, kick off the recursion
           (walker value))])))

;; A quick helper function to define equality between alternatives

(define (alter-equal alter alter2)
  (equal? (alternative-program alter) (alternative-program alter2)))

;; Now to implement a search tool to find the best expression

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
        (append (filter (λ (x) (not (or (memf (λ (y) (alter-equal x y)) rest) (memf (λ (y) (alter-equal x y)) done)))) (map make-alternative children))))
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

  (define costs
    ; See "costs.c" for details of how these numbers were determined
    #hash((+ . 1) (- . 1) (* . 1) (/ . 1)
          (abs . 1) (sqrt . 1) (square . 1)
          (exp . 270) (log . 300)
          (sin . 145) (cos . 185) (tan . 160) (cotan . 160)
          (asin . 140) (acos . 155) (atan . 130)))

  (compile (program-body prog))

  (for/sum ([step assignments])
    (let ([fn (cadr step)])
      (hash-ref costs fn 100))))

;; To use this heuristic search mechanism, we'll need to implement a
;; few helper functions

(struct alternative (program error specials cost))

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

(define (list< list1 list2)
  "Compares lists lexicographically."
  ; Who picked this terrible API design of returning '< or '>
  (eq? (datum-order list1 list2) '<))

(define (choose-min-error alts)
  "Choose the alternative with the least error"
  ; Invariant: alts is nonempty
  (let ([alts* (sort alts #:key alternative-score list<)])
    (values (car alts*) (cdr alts*))))

(define (heuristic-execute prog iterations)
  (let* ([pts (make-points (length (program-variables prog)))]
         [exacts (make-exacts prog pts)]
         [pts* (filter-points pts exacts)]
         [exacts* (filter-exacts pts exacts)]
         [evaluate (curryr max-error pts* exacts*)]
         [make-alternative
          (λ (prog)
             (let-values ([(err specials) (evaluate prog)])
               (alternative prog err specials (program-cost prog))))]
         [generate (λ (prog)
                     (let ([body (program-body prog)]
                           [vars (program-variables prog)])
                       (map (λ (body*) `(λ ,vars ,body*))
                        (rewrite-rules vars body))))])
    (heuristic-search prog generate choose-min-error make-alternative iterations)))

(define (improve prog iterations)
  (let-values ([(done options) (heuristic-execute prog iterations)])
    (for ([alt (sort done #:key alternative-score list<)])
      (display "; Alternative with score ")
      (display (alternative-score alt))
      (newline)
      (pretty-print (alternative-program alt)))))

(define (explore prog iterations)
  (let-values ([(done options) (heuristic-execute prog iterations)])
    (sort (append options done) #:key alternative-score list<)))

(define (plot-alternatives prog iterations)
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

;; Now we define our rewrite rules.

(define (rewrite-rules vars expr)
  (recursive-match expr
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

(provide (all-defined-out))
