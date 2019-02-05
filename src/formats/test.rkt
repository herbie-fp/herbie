#lang racket

(require "../common.rkt" "../errors.rkt")
(require "../programs.rkt" "../syntax-check.rkt" "../type-check.rkt")

(provide (struct-out test) test-program test-target test-successful? test<?
         load-tests load-file parse-test)

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

(struct test (name vars input output expected precondition precision) #:prefab)

(define (parse-test stx)
  (assert-program! stx)
  (assert-program-type! stx)
  (define expr (syntax->datum stx))
  (match expr
    [(list 'FPCore (list args ...) props ... body)
     (define prop-dict
       (let loop ([props props] [out '()])
         (if (null? props)
             (reverse out)
             (loop (cddr props) (cons (cons (first props) (second props)) out)))))
     ;; Default to 'real because types and precisions are mixed up right now
     (define ctx-prec (dict-ref prop-dict ':precision 'real))
     (define type-ctx (map (λ (x) (cons x (if (or (eq? ctx-prec 'binary32) (eq? ctx-prec 'binary64))
                                              'real
                                              ctx-prec)))
                           args))

     (test (~a (dict-ref prop-dict ':name body))
           args
           (desugar-program body type-ctx)
           (desugar-program (dict-ref prop-dict ':herbie-target #f) type-ctx)
           (dict-ref prop-dict ':herbie-expected #t)
           (desugar-program (dict-ref prop-dict ':pre 'TRUE) type-ctx)
           (dict-ref prop-dict ':precision 'binary64))]
    [(list (or 'λ 'lambda 'define 'herbie-test) _ ...)
     (raise-herbie-error "Herbie 1.0+ no longer supports input formats other than FPCore."
                         #:url "input.html")]
    [_
     (raise-herbie-error "Invalid input expression." #:url "input.html")]))

(define (load-stdin)
  (for/list ([test (in-port (curry read-syntax "stdin") (current-input-port))])
    (parse-test test)))

(define (load-file file)
  (call-with-input-file file
    (λ (port)
      (for/list ([test (in-port (curry read-syntax file) port)])
        (parse-test test)))))

(define (load-directory dir)
  (for/append ([fname (in-directory dir)]
               #:when (file-exists? fname)
               #:when (equal? (filename-extension fname) #"fpcore"))
    (load-file fname)))

(define (load-tests path)
  (define path* (if (string? path) (string->path path) path))
  (cond
   [(equal? path "-")
    (load-stdin)]
   [(directory-exists? path*)
    (load-directory path*)]
   [else
    (load-file path*)]))

(define (test<? t1 t2)
  (cond
   [(and (test-output t1) (test-output t2))
    (string<? (test-name t1) (test-name t2))]
   [(and (not (test-output t1)) (not (test-output t2)))
    (string<? (test-name t1) (test-name t2))]
   [else
    ; Put things with an output first
    (test-output t1)]))
