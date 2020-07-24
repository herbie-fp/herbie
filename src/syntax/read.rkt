#lang racket

(require "../common.rkt" "../errors.rkt" "../programs.rkt" "../interface.rkt"
         "syntax-check.rkt" "type-check.rkt")

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

(define (parse-test stx [override-ctx '()])
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

  (define prop-dict* ; override fpcore props
    (let loop ([prop-dict prop-dict] [ctx override-ctx])
      (match ctx
       ['() prop-dict]
       [(list (cons prop val) rest ...)
        (loop (dict-set* prop-dict prop val) rest)])))
  
  (define default-prec (dict-ref prop-dict* ':precision 'binary64))
  (define var-precs (for/list ([arg args] [arg-name arg-names])
                      (if (and (list? arg) (set-member? args ':precision))
                        (cons arg-name
                              (list-ref args (add1 (index-of args ':precision))))
                        (cons arg-name default-prec))))

  (define default-repr (get-representation default-prec))
  (define var-reprs (for/list ([(var prec) (in-dict var-precs)])
                        (cons var (get-representation prec))))

  (test (~a (dict-ref prop-dict* ':name body))
        arg-names
        (desugar-program body default-repr var-reprs)
        (desugar-program (dict-ref prop-dict* ':herbie-target #f) default-repr var-reprs)
        (dict-ref prop-dict* ':herbie-expected #t)
        (desugar-program (dict-ref prop-dict* ':spec body) default-repr var-reprs)
        (desugar-program (dict-ref prop-dict* ':pre 'TRUE) default-repr var-reprs)
        default-prec
        var-precs))

(define (load-stdin override-ctx)
  (for/list ([test (in-port (curry read-syntax "stdin") (current-input-port))])
    (parse-test test override-ctx)))

(define (load-file file override-ctx)
  (call-with-input-file file
    (λ (port)
      (port-count-lines! port)
      (for/list ([test (in-port (curry read-syntax file) port)])
        (parse-test test override-ctx)))))

(define (load-directory dir override-ctx)
  (for/append ([fname (in-directory dir)]
               #:when (file-exists? fname)
               #:when (equal? (filename-extension fname) #"fpcore"))
    (load-file fname override-ctx)))

(define (load-tests path [override-ctx '()])
  (define path* (if (string? path) (string->path path) path))
  (define out
    (cond
     [(equal? path "-")
      (load-stdin override-ctx)]
     [(directory-exists? path*)
      (load-directory path* override-ctx)]
     [else
      (load-file path* override-ctx)]))
  (define duplicates (find-duplicates (map test-name out)))
  (unless (null? duplicates)
    (warn 'duplicate-names
          "Duplicate ~a ~a used for multiple cores"
          (if (equal? (length duplicates) 1) "name" "names")
          (string-join (map (curry format "\"~a\"") duplicates) ", ")))
  out)
