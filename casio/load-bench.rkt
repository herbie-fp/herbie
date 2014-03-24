#lang racket

(require casio/common)
(require casio/programs)
(require casio/test)
(require casio/points)
(require casio/alternative)
(require casio/main)

(define *test-cache* (make-hash))

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
