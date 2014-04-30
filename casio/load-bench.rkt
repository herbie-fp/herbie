#lang racket

(require casio/common)
(require casio/programs)
(require casio/test)
(require casio/points)
(require casio/alternative)
(require casio/main)

(define *test-cache* (make-hash))

(provide load-file load-bench load-all make-prog test-improvement test-succeeds?)

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

(define (load-file p)
  (parameterize ([*tests* '()])
    (dynamic-require p 0)
    (if (null? (*tests*))
        (begin (hash-ref *test-cache* p '()))
        (begin (hash-set! *test-cache* p (*tests*))
               (*tests*)))))

(define (load-bench . path)
  (define bench-dir (string->path "../bench/"))
  (when (not (directory-exists? bench-dir))
    (error "Benchmark directory not found" (simplify-path bench-dir)))

  (define subdirs (map string->path-element path))
  (define p (apply build-path bench-dir subdirs))

  (cond
   [(directory-exists? p)
    (let* ([fs (filter is-racket-file? (directory-list p #:build? #t))])
      (apply append (map load-file fs)))]
   [(file-exists? p)
    (load-file p)]
   [else
    (error "Didn't find a directory or file at" (simplify-path p))]))

(define (is-racket-file? f)
  (and (equal? (filename-extension f) #"rkt") (file-exists? f)))

(define (walk-tree p callback)
  (cond
   [(file-exists? p)
    (callback p)]
   [(directory-exists? p)
    (for ([obj (directory-list p #:build? #t)])
      (walk-tree obj callback))]))

(define (load-all #:bench-path-string [path-string "../bench/"])
  (define bench-dir (string->path path-string))
  (apply append
         (reap [sow]
               (walk-tree bench-dir (λ (p)
                                       (when (is-racket-file? p) (sow (load-file p))))))))
