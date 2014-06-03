#lang racket

(require casio/common)
(require casio/programs)
(require casio/test)
(require casio/points)
(require casio/alternative)
(require casio/main)

(define *test-cache* (make-hash))

(provide load-tests test-improvement test-succeeds?)

(define (test-improvement test)
  (let*-values ([(end stt) (improve (make-prog test) (*num-iterations*))])
    (errors-diff-score (alt-errors stt) (alt-errors end))))

(define (test-succeeds? test)
  (if (test-output test)
      (let*-values ([(end stt)
                     (improve (make-prog test) (*num-iterations*))])
        (equal? (test-output test) (program-body (alt-program end))))
      (error "Not a real test case (no output given)" test)))

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
