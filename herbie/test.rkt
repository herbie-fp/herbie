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

(struct test (name vars sampling-expr input output) #:prefab)
(define (test-successful? test output)
  (let ([input-error (errors-score (errors (test-program test) (*pcontext*)))]
        [output-error (errors-score (errors output (*pcontext*)))]
        [target-error (if (test-output test) (errors-score (errors (test-target test) (*pcontext*))) #f)])
    (match* ((test-output test))
      [(#f) (>= (ulps->bits input-error) (- (ulps->bits output-error) 1))]
      [(#t) (>= (ulps->bits target-error) (- (ulps->bits output-error) 1))])))


(define (get-op op)
  (match op ['> >] ['< <] ['>= >=] ['<= <=]))

(define (get-sampler expr)
  (match expr
    [(? procedure? f) f] ; This can only come up from internal recusive calls
    ['float sample-float]
    ['double sample-double]
    ['default sample-default]
    [`(positive ,e) (compose (curry map abs) (get-sampler e))]
    [`(uniform ,a ,b) (sample-uniform a b)]
    [(? number? x) (λ (n) (for/list ([i (in-range n)]) x))]
    ['integer sample-integer]
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
        (desugar-program (get '#:target #f))))

(define (load-file file)
  (call-with-input-file file
    (λ (port)
      (for/list ([test (in-port read port)])
        (parse-test test)))))

(define (is-racket-file? f)
  (and (equal? (filename-extension f) #"rkt") (file-exists? f)))

(define (load-directory dir)
  (for/append ([fname (in-directory dir)])
    (when (is-racket-file? fname)
      (load-file fname))))

(define (load-tests [path benchmark-path])
  (if (directory-exists? path)
      (load-directory path)
      (load-file path)))
