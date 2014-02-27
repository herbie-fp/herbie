#lang racket

(require casio/main)
(require rackunit)

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

(define (bench-results name inerr outerr)
  (printf "~a orders: ~s\n"
          (/ (round (* (- (/ (log (/ outerr (max inerr 1e-16)))
                             (log 10))) 10)) 10)
          name))

(define (compile-program prog)
  (expand-associativity (unfold-let prog)))

(define-binary-check (check-member (lambda (x y) (member y x)) elt lst))

(define-syntax (casio-test stx)
  (syntax-case stx ()
    [(_ vars name input output)
     #`(let* ([prog `(lambda vars ,(compile-program 'input))]
              [opts (improve prog 10)])
	 (if (member 'output (map program-body opts))
	     (exit 0)
	     (begin
	       (printf "failure on ~a\ninput: ~a\ndesired output: ~a\nactual output: ~a\n"
		       'name 'input 'output (map program-body opts))
	       (exit 1))))]))

;(define-syntax (casio-bench stx)
;  (syntax-case stx ()
;    [(_ vars name input)
;     #`(let* ([pts (make-points (length 'vars))]
;              [prog `(lambda vars ,(unfold-let 'input))]
;              [exacts (make-exacts prog pts)])
;         (let-values ([(done opts) (improve prog 5)]
;                      [(prog) (car (sort done #:key alternative-score list<))]
;                      [(prog-score prog-specials) (max-error prog pts exacts)]
;                      [(goal-score goal-specials) (max-error output pts exacts)])
;           (bench-results name prog-score goal-score)))]))

(provide casio-test casio-bench cotan)
