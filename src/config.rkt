#lang racket

(provide (all-defined-out))

;;; Flags

(define all-flags
  #hash([precision . (double fallback)]
        [setup . (simplify search)]
        [generate . (rr taylor simplify better-rr proofs)]
        [reduce . (regimes avg-error binary-search branch-expressions)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic numerics special bools branches)]))

(define default-flags
  #hash([precision . ()]
        [setup . (simplify search)]
        [generate . (rr taylor simplify proofs)]
        [reduce . (regimes avg-error binary-search branch-expressions)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic numerics special bools branches)]))

(define (check-flag-deprecated! category flag)
  (match* (category flag)
    [('precision 'double)
     (eprintf "The precision:double option has been removed.\n")
     (eprintf "  The double-precision representation is specified with :precision binary64.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/input.html> for more.\n" *herbie-version*)]
    [('precision 'fallback)
     (eprintf "The precision:fallback option has been removed.\n")
     (eprintf "  The fallback representation is specified with :precision racket.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/input.html> for more.\n" *herbie-version*)]
    [('generate 'better-rr)
     (eprintf "The generate:better-rr option has been removed.\n")
     (eprintf "  The current recursive rewriter does not support the it.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [(_ _)
     (void)]))

(define (enable-flag! category flag)
  (check-flag-deprecated! category flag)
  (define (update cat-flags) (set-add cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (disable-flag! category flag)
  (check-flag-deprecated! category flag)
  (define (update cat-flags) (set-remove cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (flag-set? class flag)
  (set-member? (dict-ref (*flags*) class) flag))

(define (flag-deprecated? category flag)
  (match* (category flag)
    [('precision 'double) #t]
    [('precision 'fallback) #t]
    [('generate 'better-rr) #t]
    [(_ _) #f]))

; `hash-copy` returns a mutable hash, which makes `dict-update` invalid
(define *flags* (make-parameter (make-immutable-hash (hash->list default-flags))))

(define (changed-flags)
  (filter identity
          (for*/list ([(class flags) all-flags] [flag flags])
            (match* ((flag-set? class flag)
                     (parameterize ([*flags* default-flags]) (flag-set? class flag)))
              [(#t #t) #f]
              [(#f #f) #f]
              [(#t #f) (list 'enabled class flag)]
              [(#f #t) (list 'disabled class flag)]))))

;;; Herbie internal parameters

;; Number of points to sample for evaluating program accuracy
(define *num-points* (make-parameter 256))

;; Number of iterations of the core loop for improving program accuracy
(define *num-iterations* (make-parameter 4))

;; The maximum depth for splitting the space when searching for valid areas of points.
(define *max-find-range-depth* (make-parameter 12))

;; The maximum number of consecutive skipped points for sampling valid points
(define *max-skipped-points* (make-parameter 100))

(define *max-bsearch-bits* (make-parameter 48))

;; Maximum MPFR precision allowed during exact evaluation
(define *starting-prec* (make-parameter 256))
(define *max-mpfr-prec* (make-parameter 10000))
(define *ground-truth-extra-bits* (make-parameter 20))

;; The maximum size of an egraph
(define *node-limit* (make-parameter 8000))
(define *proof-max-length* (make-parameter 200))
(define *proof-max-string-length* (make-parameter 10000))

;; In localization, the maximum number of locations returned
(define *localize-expressions-limit* (make-parameter 4))

;; How long of a Taylor series to generate; too long and we time out
(define *taylor-order-limit* (make-parameter 4))

;; How accurate to make the binary search
(define *binary-search-test-points* (make-parameter 16))
(define *binary-search-accuracy* (make-parameter 48))

;; Pherbie related options
(define *pareto-mode* (make-parameter #t))
(define *pareto-pick-limit* (make-parameter 5))

;; In mainloop, cache improvements between iterations
(define *use-improve-cache* (make-parameter #t))

(define *default-precision* (make-parameter 'binary64))
(define *default-platform-name* (make-parameter 'default))

(define *platform-name* (make-parameter (*default-platform-name*)))
(define *loose-plugins* (make-parameter '()))

;;; About Herbie:

(define (run-command cmd)
  (parameterize ([current-error-port (open-output-nowhere)])
    (string-trim (with-output-to-string (λ () (system cmd))))))

(define (git-command #:default [default ""] gitcmd . args)
  (if (directory-exists? ".git")
      (let* ([cmd (format "git ~a ~a" gitcmd (string-join args " "))]
             [out (run-command cmd)])
          (if (equal? out "") default out))
      default))

(define *herbie-version* "2.0")

(define *hostname* (run-command "hostname"))

(define *herbie-commit*
  (git-command "rev-parse" "HEAD" #:default *herbie-version*))

(define *herbie-branch*
  (git-command "rev-parse" "--abbrev-ref" "HEAD" #:default "release"))

;;; The "reset" mechanism for clearing caches and such

(define resetters '())

(define (register-reset! fn #:priority [priority 0])
  (set! resetters (cons (cons priority fn) resetters)))

;; Defines a resetter as a parameter
;; An initialize and reset function must be provided
;; A finializer may be optionally specified
(define-syntax define-resetter
  (syntax-rules ()
    [(_ name init-fn reset-fn)
     (define-resetter name init-fn reset-fn (λ _ (void)))]
    [(_ name init-fn reset-fn finalize-fn)
     (begin
       (define name (make-parameter (init-fn)))
       (register-reset!
         (λ ()
           (finalize-fn (name))
           (name (reset-fn)))))]))

(define (reset!)
  (for ([fn-rec (sort resetters < #:key car)]) ((cdr fn-rec))))
