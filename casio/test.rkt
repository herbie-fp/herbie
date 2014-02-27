#lang racket

(require casio/alternative)
(require casio/programs)
(require casio/main)
(require rackunit)

(define *num-iterations* (make-parameter 10))

(define (unfold-let expr)
  (match expr
    [`(let* ,vars ,body)
     (let loop ([vars vars] [body body])
       (if (null? vars)
           body
           (let ([var (caar vars)] [val (cadar vars)])
             (loop (map (replace-var var val) (cdr vars))
                   ((replace-var var val) body)))))]
    [`(,head ,args ...)
     (cons head (map unfold-let args))]
    [x
     x]))

(define (expand-associativity expr)
  (match expr
    [(list (? (curryr member '(+ - * /)) op) a ..2 b)
     (list op (expand-associativity (cons op a)) (expand-associativity b))]
    [(list op a ...)
     (cons op (map expand-associativity a))]
    [_
     expr]))

(define ((replace-var var val) expr)
  (cond
   [(eq? expr var) val]
   [(list? expr)
    (cons (car expr) (map (replace-var var val) (cdr expr)))]
   [#t
    expr]))

(define (compile-program prog)
  (expand-associativity (unfold-let prog)))

(define-binary-check (check-member (lambda (x y) (member y x)) elt lst))

(define-syntax (casio-test stx)
  (syntax-case stx ()
    [(_ vars name input output)
     #`(let* ([prog `(lambda vars ,(compile-program 'input))]
              [altn (improve prog (*num-iterations*))])
	 (if (member 'output (program-body (alt-program altn)))
	     #t
	     (begin
	       (printf "failure on ~a\ninput: ~a\ndesired output: ~a\nactual output: ~a\n"
		       'name 'input 'output altn)
	       #f)))]))

(define (bench-results name inerr outerr)
  (printf "~a orders: ~s\n"
          (/ (round (* (- (/ (log (/ outerr (max inerr 1e-16)))
                             (log 10))) 10)) 10)
          name))

(define-syntax (casio-bench stx)
  (syntax-case stx ()
    [(_ vars name input)
     #`(let* ([prog `(lambda vars ,(unfold-let 'input))]
              [altn (improve prog (*num-iterations*))])
	 'ok #;(bench-results name prog altn))]))

(provide casio-test casio-bench)
