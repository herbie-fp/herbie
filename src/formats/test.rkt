#lang racket

(require "../common.rkt" "../errors.rkt")
(require "../programs.rkt" "../syntax-check.rkt" "../type-check.rkt")

(provide (struct-out test) test-program test-target test-specification load-tests parse-test)

(struct test (name vars input output expected spec precondition precision) #:prefab)

(define (test-program test)
  `(λ ,(test-vars test) ,(test-input test)))

(define (test-target test)
  `(λ ,(test-vars test) ,(test-output test)))

(define (test-specification test)
  `(λ ,(test-vars test) ,(test-spec test)))

(define (parse-test stx)
  (assert-program! stx)
  (assert-program-type! stx)
  (match-define (list 'FPCore (list args ...) props ... body) (syntax->datum stx))

  (define prop-dict
    (let loop ([props props])
      (match props
        ['() '()]
        [(list prop val rest ...) (cons (cons prop val) (loop rest))])))

  (define ctx-prec
    ;; Default to 'real because types and precisions are mixed up right now
    (match (dict-ref prop-dict ':precision 'real)
      ['binary32 'real]
      ['binary64 'real]
      [x x]))
  (define type-ctx (map (curryr cons ctx-prec) args))

  (test (~a (dict-ref prop-dict ':name body))
        args
        (desugar-program body type-ctx)
        (desugar-program (dict-ref prop-dict ':herbie-target #f) type-ctx)
        (dict-ref prop-dict ':herbie-expected #t)
        (desugar-program (dict-ref prop-dict ':spec body) type-ctx)
        (desugar-program (dict-ref prop-dict ':pre 'TRUE) type-ctx)
        (dict-ref prop-dict ':precision 'binary64)))

(define (load-stdin)
  (for/list ([test (in-port (curry read-syntax "stdin") (current-input-port))])
    (parse-test test)))

(define (load-file file)
  (call-with-input-file file
    (λ (port)
      (port-count-lines! port)
      (for/list ([test (in-port (curry read-syntax file) port)])
        (parse-test test)))))

(define (load-directory dir)
  (for/append ([fname (in-directory dir)]
               #:when (file-exists? fname)
               #:when (equal? (filename-extension fname) #"fpcore"))
    (load-file fname)))

(define (load-tests path)
  (define path* (if (string? path) (string->path path) path))
  (define out
    (cond
     [(equal? path "-")
      (load-stdin)]
     [(directory-exists? path*)
      (load-directory path*)]
     [else
      (load-file path*)]))
  (define duplicates (find-duplicates (map test-name out)))
  (unless (null? duplicates)
    (warn 'duplicate-names
          "Duplicate ~a ~a used for multiple cores"
          (if (equal? (length duplicates) 1) "name" "names")
          (string-join (map (curry format "\"~a\"") duplicates) ", ")))
  out)
