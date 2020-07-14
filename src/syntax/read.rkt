#lang racket

(require "../common.rkt" "../errors.rkt" "../programs.rkt" "syntax-check.rkt" "type-check.rkt")

(provide (struct-out test)
         test-program test-target test-specification load-tests parse-test
         test-precondition)


(struct test (name vars input output expected spec pre
                   output-prec var-precs) #:prefab)

(define (test-program test)
  `(λ ,(test-vars test) ,(test-input test)))

(define (test-target test)
  `(λ ,(test-vars test) ,(test-output test)))

(define (test-specification test)
  `(λ ,(test-vars test) ,(test-spec test)))

(define (test-precondition test)
  `(λ ,(test-vars test) ,(test-pre test)))

(define (parse-test stx)
  (assert-program! stx)
  (assert-program-typed! stx)
  (match-define (list 'FPCore (list args ...) props ... body) (syntax->datum stx))
  ;; TODO(interface): Currently, this code doesn't fire because annotations aren't
  ;; allowed for variables because of the syntax checker yet. This should run correctly
  ;; once the syntax checker is updated to the FPBench 1.1 standard.
  (define arg-names (for/list ([arg args])
                      (if (list? arg)
                        (last arg)
                        arg)))

  (define prop-dict
    (let loop ([props props])
      (match props
        ['() '()]
        [(list prop val rest ...) (cons (cons prop val) (loop rest))])))

  (define default-prec (dict-ref prop-dict ':precision
                                 (if (flag-set? 'precision 'double)
                                   'binary64
                                   'binary32)))
  (define var-precs (for/list ([arg args] [arg-name arg-names])
                      (if (and (list? arg) (set-member? args ':precision))
                        (cons arg-name
                              (list-ref args (add1 (index-of args ':precision))))
                        (cons arg-name default-prec))))

  (define ctx-prec (dict-ref prop-dict ':precision 'binary64))
  (define type-ctx (map (curryr cons ctx-prec) args))

  (test (~a (dict-ref prop-dict ':name body))
        arg-names
        (desugar-program body default-prec var-precs)
        (desugar-program (dict-ref prop-dict ':herbie-target #f) default-prec var-precs)
        (dict-ref prop-dict ':herbie-expected #t)
        (desugar-program (dict-ref prop-dict ':spec body) default-prec var-precs)
        (desugar-program (dict-ref prop-dict ':pre 'TRUE) default-prec var-precs)
        default-prec
        var-precs))

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
