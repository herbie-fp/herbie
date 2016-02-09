#lang racket

(require "common.rkt")
(require "alternative.rkt")
(require "programs.rkt")
(require "points.rkt")

(provide (struct-out test) test-program test-samplers
         load-tests load-file test-target parse-test test-successful?)

(define (test-program test)
  `(λ ,(test-vars test) ,(test-input test)))

(define (test-target test)
  `(λ ,(test-vars test) ,(test-output test)))

(define (test-successful? test input-bits target-bits output-bits)
  (match* ((test-output test) (test-expected test))
    [(_ #f) #t]
    [(_ (? number? n)) (>= n output-bits)]
    [(#f #t) (>= input-bits output-bits)]
    [(_ #t) (>= target-bits (- output-bits 1))]))

(struct test (name vars sampling-expr input output expected) #:prefab)

(define (get-op op)
  (match op ['> >] ['< <] ['>= >=] ['<= <=]))

(define (get-sampler expr)
  (match expr
    ['default sample-default]
    [`(positive ,e)
     (define sub (get-sampler e))
     (λ () (let ([y (sub)]) (and y (abs y))))]
    [`(uniform ,a ,b) (sample-uniform a b)]
    [(? number? x) (const x)]
    ['integer sample-integer]
    [`(,(and op (or '< '> '<= '>=)) ,a ,(? number? b))
     (let ([sa (get-sampler a)] [test (curryr (get-op op) b)])
       (λ () (let ([va (sa)]) (and (test va) va))))]
    [`(,(and op (or '< '> '<= '>=)) ,(? number? a) ,b)
     (let ([sb (get-sampler b)] [test (curry (get-op op) a)])
       (λ () (let ([vb (sb)]) (and (test vb) vb))))]
    [`(,(and op (or '< '> '<= '>=)) ,(? number? a) ,t ,(? number? b))
     (let ([st (get-sampler t)] [test (λ (x) ((get-op op) a x b))])
       (λ () (let ([vt (st)]) (and (test vt) vt))))]))

(define (test-samplers test)
  (for/list ([var (test-vars test)] [samp (test-sampling-expr test)])
    (cons var (get-sampler samp))))

(define (var&dist expr)
  (match expr
    [(list var samp) (cons var samp)]
    [var (cons var 'default)]))

(define (args&body* args)
  (match args
    [(list (? keyword? name) value args* ...)
     (define out* (args&body* args*))
     (cons (car out*) (cons (cons name value) (cdr out*)))]
    [(list body args* ...)
     (define out* (args&body* args*))
     (assert (not (car out*)) #:extra-info (λ () (format "Two body expressions ~a and ~a" (car out*) body)))
     (cons body (cdr out*))]
    [(list)
     (cons #f '())]))

(define (args&body args)
  (define out* (args&body* args))
  (assert (car out*) #:extra-info (λ () "No body expression"))
  out*)

(define (parse-test expr)
  (define-values (vars* args*)
    (match expr
      [(list 'herbie-test (list vars ...) (? string? name) input)
       (values vars (list '#:name name input))]
      [(list 'herbie-test (list vars ...) (? string? name) input output)
       (values vars (list '#:name name '#:target output input))]
      [(list 'herbie-test (list vars ...) input output)
       (values vars (list '#:name "Unnamed Test" '#:target output input))]
      [(list 'lambda (list vars ...) args ...)
       (values vars args)]
      [(list 'define name (list vars ...) args ...)
       (values vars (list*'#:name name args))]))
  (match-define (list (cons vars samp) ...) (map var&dist vars*))
  (match-define (list body args ...) (args&body args*))
  (define (get kw default)
    (let ([rec (assoc kw args)])
      (if rec (cdr rec) default)))

  (test (~a (get '#:name body))
        vars samp
        (desugar-program body)
        (desugar-program (get '#:target #f))
        (get '#:expected #t)))

(define (load-file file)
  (call-with-input-file file
    (λ (port)
      (for/list ([test (in-port read port)])
        (parse-test test)))))

(define (is-racket-file? f)
  (and (equal? (filename-extension f) #"rkt") (file-exists? f)))

(define (load-directory dir)
  (for/append ([fname (in-directory dir)] #:when (is-racket-file? fname))
    (load-file fname)))

(define (load-tests [path benchmark-path])
  (if (directory-exists? path)
      (load-directory path)
      (load-file path)))
