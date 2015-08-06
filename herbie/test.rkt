#lang racket

(require "common.rkt")
(require "alternative.rkt")
(require "programs.rkt")
(require "points.rkt")
(require racket/runtime-path)

(provide (struct-out test) test-program test-samplers parse-test
         load-tests
         test-target load-file)

;; This is sort of hacky, because I don't want to introduce another
;; dependency on our syntax by including special support for looping
;; forms, but I can't use the -induct functions as I normally would
;; because they don't support let*'s, since they assume they've
;; already been unfolded.
(define (unfold-let* expr)
  (match expr
    [`(let* ([,vars ,vals] ...) ,body)
     (for/fold ([acc (unfold-let* body)])
	 ([var (reverse vars)]
          [val (reverse vals)])
       `(let ([,var ,(unfold-let* val)]) ,acc))]
    [(list items ...)
     (map unfold-let* expr)]
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
    [`(let ([,vars ,vals] ...) ,body)
     `(let ,(for/list ([var vars] [val vals])
              `(,var ,(expand-associativity val)))
        ,(expand-associativity body))]
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
  (expand-associativity (unfold-let* prog)))

(define (test-program test)
  `(λ ,(test-vars test) ,(test-input test)))

(define (test-target test)
  `(λ ,(test-vars test) ,(test-output test)))

(struct test (name vars sampling-expr input output) #:prefab)

(define (get-op op)
  (match op ['> >] ['< <] ['>= >=] ['<= <=]))

(define (parse-sampler-arg arg)
  (cond [(number? arg)
	 (curryr make-list arg)]
	[(procedure? arg)
	 arg]
	[#t (get-sampler arg)]))

(define (get-sampler expr)
  (match expr
    [(? procedure? f) f] ; This can only come up from internal recusive calls
    ['float sample-float]
    ['double sample-double]
    ['default sample-default]
    [`(positive ,e) (compose (curry map abs) (get-sampler e))]
    [`(uniform ,a ,b) (sample-uniform a b)]
    [(? number? x) (λ (n) (for/list ([i (in-range n)]) x))]
    [`(list ,length ,mean ,deviation-ratio)
     (sample-list (parse-sampler-arg length)
		  (parse-sampler-arg mean)
		  (parse-sampler-arg deviation-ratio))]
    ['integer sample-integer]
    [`(integer-range ,a ,b) (sample-integer-range a b)]
    ['expbucket sample-expbucket]
    [`(,(and op (or '< '> '<= '>=)) ,a ,(? number? b))
     (let ([sa (get-sampler a)] [test (curryr (get-op op) b)])
       (λ (n) (for/list ([va (sa n)]) (if (test va) va +nan.0))))]
    [`(,(and op (or '< '> '<= '>=)) ,(? number? a) ,b)
     (let ([sb (get-sampler b)] [test (curry (get-op op) a)])
       (λ (n) (for/list ([vb (sb n)]) (if (test vb) vb +nan.0))))]
    [`(,(and op (or '< '> '<= '>=)) ,a ,b ...)
     ; The justification for this is that (< (< 0 float) 1) is interpreted as
     ; samples from (< 0 float) that are (< ? 1), which is just what we want
     (get-sampler `(,op ,a ,(get-sampler `(,op ,@b))))]))

(define (test-samplers test)
  (for/list ([var (test-vars test)] [samp (test-sampling-expr test)])
    (cons var (get-sampler samp))))

(define (parse-test expr)
  (define (var&dist expr)
    (match expr
      [(list var samp) (cons var samp)]
      [var (cons var 'default)]))

  (match expr
    [(list 'herbie-test (list vars ...) name input output)
     (let* ([parse-args (map var&dist vars)])
       (let ([vars (map car parse-args)] [samp (map cdr parse-args)])
         (test name vars samp (compile-program input) (compile-program output))))]
    [(list 'herbie-test (list vars ...) name input)
     (let* ([parse-args (map var&dist vars)])
       (let ([vars (map car parse-args)] [samp (map cdr parse-args)])
         (test name vars samp (compile-program input) #f)))]))

(define (load-file p)
  (let ([fp (open-input-file p)])
    (let loop ()
      (let ([test (read fp)])
        (if (eof-object? test)
            '()
            (cons (parse-test test) (loop)))))))

(define (is-racket-file? f)
  (and (equal? (filename-extension f) #"rkt") (file-exists? f)))

(define (walk-tree p callback)
  (cond
   [(file-exists? p)
    (callback p)]
   [(directory-exists? p)
    (for ([obj (directory-list p #:build? #t)])
      (walk-tree obj callback))]))

(define (load-tests [path benchmark-path])
  (define (handle-file sow p)
    (when (is-racket-file? p)
      (sow (load-file p))))

  (apply append
         (reap [sow]
               (walk-tree path (curry handle-file sow)))))
