#lang racket

(require racket/match)
(require racket/flonum)

;; Some evaluation programs.  Both evaluate the same function over the real
;; numbers, but the second has better numerical precision.

(define prog1 '(λ (x) (/ (- (exp x) 1) x)))
(define prog2 '(λ (x) (/ (- (exp x) 1) (log (exp x)))))

(define program-body caddr)
(define program-variable caadr)

;; We evaluate a program by comparing its results computed with single precision
;; to its results computed with extended precision.

(define (relative-error approx exact)
  (if (and (real? approx) (real? exact))
      (abs (/ (- exact approx) exact))
      +inf.0))

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

(define (drop-arg f)
  "Make a one-argument function out of a no-argument function"
  (lambda (x) (f)))

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

(struct alternative (program error))

(define (alternative< alt1 alt2)
  (< (alternative-error alt1) (alternative-error alt2)))

(define (choose-min-error alts)
  "Choose the alternative with the least error"
  ; Invariant: alts is nonempty
  (let ([alts* (sort alts alternative<)])
    (values (car alts*) (cdr alts*))))

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

(define (heuristic-improve prog iterations)
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

;; Now we define our rewrite rules.

(define (rewrite-rules var expr)
  (recursive-match expr
    ;[`(list - ,x ,x) 0]
    ;[`(+ ,a (+ ,b ,c)) `(+ (+ ,a ,b) ,c)]
    [x `(exp (log ,x))]
    [x `(log (exp ,x))]))
    ;[`(/ (+ ,x (sqrt ,y)) ,c) `(/ (- (expt ,x 2) ,y) (* ,c (- ,x (sqrt ,y))))]))
