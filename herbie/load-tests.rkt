#lang racket

(require "common.rkt")
(require "programs.rkt")
(require "test.rkt")
(require "points.rkt")
(require "alternative.rkt")

(define *test-cache* (make-hash))

(provide load-tests)

(define (load-file p)
  (parameterize ([*tests* '()])
    (dynamic-require p 0)
    (if (null? (*tests*))
        (begin (hash-ref *test-cache* p '()))
        (begin (hash-set! *test-cache* p (*tests*))
               (*tests*)))))

(define (is-racket-file? f)
  (and (equal? (filename-extension f) #"rkt") (file-exists? f)))

(define (walk-tree p callback)
  (cond
   [(file-exists? p)
    (callback p)]
   [(directory-exists? p)
    (for ([obj (directory-list p #:build? #t)])
      (walk-tree obj callback))]))

(define (load-tests [path "../bench/"])
  (define (handle-file sow p)
    (when (is-racket-file? p)
      (sow (load-file p))))

  (apply append
         (reap [sow]
               (walk-tree path (curry handle-file sow)))))
