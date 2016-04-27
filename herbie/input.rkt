#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     racket/sequence
                     racket/set
                     syntax/id-set))

(module+ test
  (require racket/function
           racket/port
           rackunit
           syntax/macro-testing))

(module reader syntax/module-reader
  #:language '(submod herbie/input initial-bindings))

(module+ initial-bindings
  (provide #%datum
           #%module-begin
           #%top-interaction
           herbie-test))


(begin-for-syntax
  (define (first-or vs default)
    (if (empty? vs) default (first vs)))
  (define-syntax-class unique-var-list
    (pattern (name:id ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(name ...)))
             "Duplicate equation variable"
             #:attr bound-id-set
             (for/fold ([bidset (immutable-bound-id-set (set))])
                       ([name (in-syntax #'(name ...))])
               (bound-id-set-add bidset name))))
  (define-syntax-class herbie-equation
    (pattern expr))
  (define (operation-id? id-stx)
    (not (not (member (identifier-binding-symbol id-stx)
                      '(+ - * / abs sqr sqrt exp log expt
                          sin cos tan cotan asin acos atan atan2
                          sinh cosh tanh expm1 log1p hypot
                          if = < > <= >= and or not
                          let* pi e)))))
  (define (syntax-identifiers stx)
    (define maybe-subparts (syntax->list stx))
    (cond
      [(identifier? stx) (list stx)]
      [maybe-subparts (append-map syntax-identifiers maybe-subparts)]
      [else '()]))
  (define (herbie-used-variables equation-stx)
    (immutable-bound-id-set
     (list->set
      (filter-not operation-id?
                  (syntax-identifiers equation-stx)))))
  (define (herbie-unused-variables variables equation-stx)
    (define used (herbie-used-variables equation-stx))
    (set->list (bound-id-set-subtract variables used)))
  (define (herbie-undefined-variables variables equation-stx)
    (define used (herbie-used-variables equation-stx))
    (set->list (bound-id-set-subtract used variables))))

(define-simple-macro (herbie-test description:str
                                  variables:unique-var-list
                                  equation:herbie-equation)
  #:do [(define unused
          (herbie-unused-variables (attribute variables.bound-id-set)
                                   #'equation))
        (define undefined
          (herbie-undefined-variables (attribute variables.bound-id-set)
                                      #'equation))]
  #:fail-when (first-or unused #f) "Unused equation variable"
  #:fail-when (first-or undefined #f) "Undefined equation variable"
  ;; TODO: Construct a test case and pass it to run-herbie
  (printf "Test ~v successfully checked" description))

(module+ test
  (check-exn #rx"Duplicate equation variable"
             (thunk (convert-syntax-error (herbie-test "duplicate" (x x) (+ x x)))))
  (check-exn #rx"Unused equation variable"
             (thunk (convert-syntax-error (herbie-test "unused" (x y) (+ x x)))))
  (check-exn #rx"Undefined equation variable"
             (thunk (convert-syntax-error (herbie-test "undefined" (x) (+ x y)))))
  (define-syntax-rule (without-output body ...)
    (void (with-output-to-string (thunk body ...))))
  (without-output
   (check-not-exn (thunk (convert-syntax-error (herbie-test "valid" (x y) (+ x y)))))))
