#lang racket

(require casio/alternative)
(require casio/programs)

(provide (struct-out test) *tests* *num-iterations* casio-test casio-bench)

(define *num-iterations* (make-parameter 5))

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
     (list op
           (expand-associativity (cons op a))
           (expand-associativity b))]
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

(struct test (name vars input output))
(define *tests* (make-parameter '()))

(define (save-test t)
  (*tests* (cons t (*tests*))))

(define-syntax (casio-test stx)
  (syntax-case stx ()
    [(_ vars name input output)
     #`(save-test (test name 'vars (compile-program 'input) (compile-program 'output)))]))

(define-syntax (casio-bench stx)
  (syntax-case stx ()
    [(_ vars name input)
     #`(save-test (test name 'vars (compile-program 'input) #f))]))
