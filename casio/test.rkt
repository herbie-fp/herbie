#lang racket

(require casio/alternative)
(require casio/programs)
(require casio/points)
(require casio/main)

(provide (struct-out test) *tests* *num-iterations*
         test-improvement test-succeeds?
         casio-test casio-bench)

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
(define *test-cache* (make-hash))

(define-syntax (casio-test stx)
  (syntax-case stx ()
    [(_ vars name input output)
     #`(*tests* (cons (test name 'vars (compile-program 'input)
                            (compile-program 'output)) (*tests*)))]))

(define-syntax (casio-bench stx)
  (syntax-case stx ()
    [(_ vars name input)
     #`(*tests* (cons (test name 'vars (compile-program 'input) #f)
                      (*tests*)))]))

(define (make-prog test)
  `(λ ,(test-vars test) ,(test-input test)))

(define (test-improvement test)
  (let*-values ([(end stt) (improve (make-prog test) (*num-iterations*))]
                [(diff) (errors-diff-score (alt-errors stt) (alt-errors end))])
    (/ diff (length (alt-errors end)))))

(define (test-succeeds? test)
  (if (test-output test)
      (let*-values ([(end stt)
                     (improve (make-prog test) (*num-iterations*))])
        (equal? (test-output test) (program-body (alt-program end))))
      (error "Not a real test case (no output given)" test)))

(define (load-bench . path)
  (define bench-dir (string->path "../bench/"))
  (when (not (directory-exists? bench-dir))
    (error "Benchmark directory not found" (simplify-path bench-dir)))

  (define subdirs (map string->path-element path))
  (define p (apply build-path bench-dir subdirs))

  (cond
   [(directory-exists? p)
    (let* ([fs (filter file-exists? (directory-list p #:build? #t))])
      (parameterize ([*tests* '()])
        (for ([f fs]) (dynamic-require f 0))
        (if (null? (*tests*))
            (begin (hash-ref *test-cache* p '()))
            (begin (hash-set! *test-cache* p (*tests*))
                   (*tests*)))))]
   [(file-exists? p)
    (parameterize ([*tests* '()])
      (dynamic-require p 0)
      (if (null? (*tests*))
          (begin (hash-ref *test-cache* p '()))
          (begin (hash-set! *test-cache* p (*tests*))
                 (*tests*))))]
   [else
    (error "Didn't find a directory or file at" (simplify-path p))]))
