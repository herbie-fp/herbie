#lang racket

(require rackunit)
(require "../common.rkt")
(require "../points.rkt")
(require "../float.rkt")
(require "../alternative.rkt")
(require "../glue.rkt")
(require "../syntax/syntax.rkt")
(require "../syntax/rules.rkt")
(require math/bigfloat)

(error "Does not work now that the ruleset framework has changed.")

(define *rulesets* '())

(define-ruleset dummy-rules
  [f->g1        (f x)          (g1 x)]
  [f->g2        (f x)          (g2 x)]
  [f->g3        (f x)          (g3 x)]
  [f->g4        (f x)          (g4 x)]
  [f->g5        (f x)          (g5 x)]
  [f->g6        (f x)          (g6 x)])

(define bfg1 (const (bf 10)))
(define bfg2 (const (bf 20)))
(define bfg3 (const (bf 30)))
(define bfg4 (const (bf 40)))
(define bfg5 (const (bf 50)))
(define bfg6 (const (bf 60)))

(define flg1 (const 10.0))
(define flg2 (const 20.0))
(define flg3 (const 30.0))
(define flg4 (const 40.0))
(define flg5 (const 50.0))
(define flg6 (const 60.0))

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define-table dummy-operations
  [g1        bfg1          flg1           1]
  [g2        bfg2          flg2           1]
  [g3        bfg3          flg3           1]
  [g4        bfg4          flg4           1]
  [g5        bfg5          flg5           1]
  [g6        bfg6          flg6           1]
  [if        if-fn         if-fn          1]
  [>         bf>           >              1]
  [<         bf<           <              1]
  [<=        bf<=          <=             1]
  [>=        bf>=          >=             1]
  [and       and-fn        and-fn         1]
  [or        or-fn         or-fn          1])
  
(define (synthesize-op op1 op2 split)
  (let ([bfop1 (list-ref (hash-ref dummy-operations op1) 0)]
	[bfop2 (list-ref (hash-ref dummy-operations op2) 0)]
	[bfsplit (map bf split)])
  (hash-set! dummy-operations 'f
	     (list (λ (x)
		     (if (bf< x (car bfsplit))
			 (bfop1 x)
			 (bfop2 x)))
		   (λ (x) 0.0)
		   1))))

(define (get-branch-info prog)
  (match prog
    [`(λ ,vars (if (< ,var ,branch) (,f1 ,var) (,f2 ,var)))
     (values branch f1 f2)]
    [_ (values +inf.0 #f #f)]))

(define (test-regimes)
  (define branch-point (sample-float))
  (define (rand-op)
    (car (rule-output (list-ref dummy-rules (random 6)))))
  (debug #:from 'test-regimes "Selected branch point " branch-point)
  (define op1 (rand-op))
  (define op2 (let loop ()
		(let ([op (rand-op)])
		  (if (eq? op op1)
		      (loop)
		      op))))
  (debug #:from 'test-regimes "Selected ops" op1 "and" op2)
  (synthesize-op op1 op2 branch-point)
  (let*-values ([(res) (alt-program
			(parameterize ([*rules* dummy-rules] [*operations* dummy-operations])
			  (improve '(λ (x) (f x)) (*num-iterations*))))]
		[(actual-branch actual-left-func actual-right-func)
		 (get-branch-info res)]
		[(expected-branch) (car branch-point)])
    (list actual-branch
	  expected-branch
	  (list op1 actual-left-func)
	  (list op2 actual-right-func))))

(define (error-of res)
  (bit-difference (car res) (cadr res)))

(define (rel-error-of res)
  (/ (abs (- (car res) (cadr res))) (car res)))


(define (gen-file n filename)
  (write-file filename
	      (gen-data n)))

(define (gen-data n)
  (for ([i (range n)])
    (let* ([res (test-regimes)]
	   [bits (error-of res)]
	   [rel (rel-error-of res)]
	   [actual (car res)]
	   [expected (cadr res)]
	   [op1 (third res)]
	   [op2 (fourth res)])
      (printf "~a, ~a, ~a, ~a, ~a, ~a\n" actual expected bits rel op1 op2))))

(define regimes-tests
  (test-suite "Test that regimes finds the correct branch point, within a bit."
    (for ([i (range 3)])
      (let* ([res (test-regimes)])
	(check-true (2 . > . (error-of res)))))))
