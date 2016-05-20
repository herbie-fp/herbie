#lang racket

(require "../common.rkt")
(require "../alternative.rkt")
(require "../programs.rkt")
(require "../syntax/distributions.rkt")

(provide (struct-out test) test-program test-samplers
         load-tests load-file test-target parse-test test-successful? test<?
         ; for convresion tool
         var&dist args&body)

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

(define (test-samplers test)
  (for/list ([var (test-vars test)] [samp (test-sampling-expr test)])
    (cons var (eval-sampler samp))))

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
;  (define-values (vars* args*)
;    (match expr
;      [(list 'herbie-test (list vars ...) (? string? name) input)
;       (values vars (list '#:name name input))]
;      [(list 'herbie-test (list vars ...) (? string? name) input output)
;       (values vars (list '#:name name '#:target output input))]
;      [(list 'herbie-test (list vars ...) input output)
;       (values vars (list '#:name "Unnamed Test" '#:target output input))]
;      [(list 'lambda (list vars ...) args ...)
;       (values vars args)]
;      [(list 'define name (list vars ...) args ...)
;       (values vars (list*'#:name name args))]))

  (match-define (list 'FPCore (list args ...) props ... body) expr)
  (define prop-dict
    (let loop ([props props] [out '()])
      (if (null? props)
        out
        (loop (cddr props) (cons (cons (first props) (second props)) out)))))
  ;(match-define (list (cons vars samp) ...) (map var&dist vars*))
  ;(match-define (list body args ...) (args&body args*))
  ;(define (get kw default)
  ;  (let ([rec (assoc kw args)])
  ;    (if rec (cdr rec) default)))

  (test (~a (dict-ref prop-dict ':name body))
        args (dict-ref prop-dict ':herbie-samplers (map (const 'default) args))
        (desugar-program body)
        (desugar-program (dict-ref prop-dict ':target #f))
        (dict-ref prop-dict ':herbie-expected #t)))

(define (load-file file)
  (call-with-input-file file
    (λ (port)
      (define tests (for/list ([test (in-port read port)])
                      (parse-test test)))
      (let ([duplicate-name (check-duplicates tests #:key test-name)])
        (assert (not duplicate-name)
                #:extra-info (λ () (format "Two tests with the same name ~a" duplicate-name))))
      tests)))

(define (is-racket-file? f)
  (and (equal? (filename-extension f) #"rkt") (file-exists? f)))

(define (load-directory dir)
  (for/append ([fname (in-directory dir)] #:when (is-racket-file? fname))
    (load-file fname)))

(define (load-tests [path benchmark-path])
  (if (directory-exists? path)
      (load-directory path)
      (load-file path)))

(define (test<? t1 t2)
  (cond
   [(and (test-output t1) (test-output t2))
    (string<? (test-name t1) (test-name t2))]
   [(and (not (test-output t1)) (not (test-output t2)))
    (string<? (test-name t1) (test-name t2))]
   [else
    ; Put things with an output first
    (test-output t1)]))
