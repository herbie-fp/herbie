#lang racket

(require racket/match)
(require racket/flonum)
(require racket/pretty)
(require data/order)

;; Some evaluation programs.  Both evaluate the same function over the real
;; numbers, but the second has better numerical precision.

(define prog1 '(λ (x) (/ (- (exp x) 1) x)))
(define prog2 '(λ (x) (/ (- (exp x) 1) (log (exp x)))))

(define program-body caddr)
(define program-variable caadr)

;; We evaluate a program by comparing its results computed with single precision
;; to its results computed with extended precision.

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

(define (rewrite-constants rule expr)
  (cond
   [(real? expr)
    (rule expr)]
   [(list? expr)
    (map (curry rewrite-constants rule) expr)]
   [#t
    expr]))

(define (eval-prog prog rule)
  (let ([fn (eval (rewrite-constants rule prog))])
    (lambda (pt) (fn (rule pt)))))

; We evaluate  a program on random floating-point numbers.

(define (make-points)
  "Make a list of real numbers.  The list spans a large range of values"
  (build-list 505 (λ (idx) (expt 2 (- (/ (+ idx (random)) 2) 126)))))

(define (make-exacts prog pts)
  "Given a list of arguments, produce a list of exact evaluations of a program at those arguments"
  (map (eval-prog prog real->double-flonum) pts))

(define (max-error prog pts exacts)
  "Find the maximum error in a function's approximate evaluations at the given points
   (compared to the given exact results), and the number of evaluations that yield
   a special value."
  (let ([errors
         (let ([fn (eval-prog prog real->single-flonum)])
           (for/list ([pt pts] [exact exacts])
             (relative-error (fn pt) exact)))])
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
        (append (map make-alternative children)))
       (cons parent done))))
  
  (let loop ([options (list (make-alternative start))]
             [done '()])
    (if (or (null? options)
            (>= (length done) iterations))
        done
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
          (abs . 1) (sqrt . 1)
          (exp . 270) (log . 300)
          (sin . 145) (cos . 185) (tan . 160)
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
   (+ (* 0.1 (alternative-specials alt))
      (log (max (alternative-error alt) 1e-50))
      (* 0.005 (alternative-cost alt)))
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
  (let* ([pts (make-points)]
         [exacts (make-exacts prog pts)]
         [evaluate (curryr max-error pts exacts)]
         [make-alternative
          (λ (prog)
             (let-values ([(err specials) (evaluate prog)])
               (alternative prog err specials (program-cost prog))))]
         [generate (λ (prog)
                     (let ([body (program-body prog)]
                           [var (program-variable prog)])
                       (map (λ (body*) `(λ (,var) ,body*))
                        (rewrite-rules var body))))])
    (heuristic-search prog generate choose-min-error make-alternative iterations)))

(define (improve prog iterations)
  (for ([alt (sort (heuristic-execute prog iterations) #:key alternative-score list<)])
    (display "; Alternative with score ")
    (display (alternative-score alt))
    (newline)
    (pretty-print (alternative-program alt))))

;; Now we define our rewrite rules.

(define (rewrite-rules var expr)
  (recursive-match expr
    ;[`(list - ,x ,x) 0]
    [`(+ ,a (+ ,b ,c)) `(+ (+ ,a ,b) ,c)]
    [`(+ (+ ,a ,b) ,c) `(+ ,a (+ ,b ,c))]
    [`(* ,a (+ ,b ,c)) `(+ (* ,a ,b) (* ,a ,c))]
    [`(+ (* ,a ,b) (* ,a ,c)) `(* ,a (+ ,b ,c))]
    [x `(exp (log ,x))]
    [x `(log (exp ,x))]))
    ;[`(/ (+ ,x (sqrt ,y)) ,c) `(/ (- (expt ,x 2) ,y) (* ,c (- ,x (sqrt ,y))))]))
