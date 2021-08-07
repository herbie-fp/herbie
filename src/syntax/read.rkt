#lang racket

(require "../common.rkt" "../errors.rkt" "../programs.rkt" "../interface.rkt"
         "../preprocess.rkt" "../conversions.rkt"
         "syntax-check.rkt" "type-check.rkt" "sugar.rkt")

(provide (struct-out test)
         test-program test-target test-specification load-tests parse-test
         test-precondition test-output-repr)


(struct test (name identifier vars input output expected spec pre
              preprocess output-prec var-precs conversions) #:prefab)

(define (test-program test)
  `(λ ,(test-vars test) ,(test-input test)))

(define (test-target test)
  `(λ ,(test-vars test) ,(test-output test)))

(define (test-specification test)
  `(λ ,(test-vars test) ,(test-spec test)))

(define (test-precondition test)
  `(λ ,(test-vars test) ,(test-pre test)))

(define (test-output-repr test)
  (get-representation (test-output-prec test)))

(define (parse-test stx [override-ctx '()])
  (assert-program! stx)
  (assert-program-typed! stx)
  (define-values (func-name args props body)
    (match (syntax->datum stx)
     [(list 'FPCore name (list args ...) props ... body)
      (values name args props body)]
     [(list 'FPCore (list args ...) props ... body)
      (values #f args props body)]))

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
  (define default-repr (get-representation default-prec))
  (define var-reprs 
    (for/list ([arg args] [arg-name arg-names])
      (cons arg-name
            (if (and (list? arg) (set-member? args ':precision))
                (get-representation (cadr (member ':precision args)))
                default-repr))))

  ;; Named fpcores need to be added to function table
  (when func-name (register-function! func-name args default-repr body))

  ;; Try props first, then identifier, else the expression itself
  (define name
    (or (dict-ref prop-dict* ':name #f)
        func-name
        body))

  ;; load conversion operators for desugaring
  (define convs (dict-ref prop-dict* ':herbie-conversions '()))
  (generate-conversions convs)

  ;; inline and desugar
  (define body* (desugar-program body default-repr var-reprs))
  (define pre* (desugar-program (dict-ref prop-dict* ':pre 'TRUE) default-repr var-reprs))
  (define target (desugar-program (dict-ref prop-dict* ':herbie-target #f) default-repr var-reprs))
  (define spec (desugar-program (dict-ref prop-dict* ':spec body) default-repr var-reprs))
  (check-unused-variables arg-names body* pre*)
  (check-weird-variables arg-names)

  (test (~a name)
        func-name
        arg-names
        body*
        target
        (dict-ref prop-dict* ':herbie-expected #t)
        spec
        pre*
        (map sexp->preprocess (dict-ref prop-dict* ':herbie-preprocess empty))
        (representation-name default-repr)
        (map (λ (pair) (cons (car pair) (representation-name (cdr pair)))) var-reprs)
        convs))

(define (check-unused-variables vars precondition expr)
  ;; Fun story: you might want variables in the precondition that
  ;; don't appear in the `expr`, because that can allow you to do
  ;; non-uniform sampling. For example, if you have the precondition
  ;; `(< x y)`, where `y` is otherwise unused, then `x` is sampled
  ;; non-uniformly (biased toward small values).
  (define used (set-union (free-variables expr) (free-variables precondition)))
  (unless (set=? vars used)
    (define unused (set-subtract vars used))
    (warn 'unused-variable
          "unused ~a ~a" (if (equal? (set-count unused) 1) "variable" "variables")
          (string-join (map ~a unused) ", "))))

(define (check-weird-variables vars)
  (for* ([var vars] [const (in-hash-keys parametric-constants)])
    (when (string-ci=? (symbol->string var) (symbol->string const))
      (warn 'strange-variable
            "unusual variable ~a; did you mean ~a?" var const))))

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
  (apply append
         (for/list ([fname (in-directory dir)]
                    #:when (file-exists? fname)
                    #:when (equal? (filename-extension fname) #"fpcore"))
           (load-file fname override-ctx))))

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
