#lang racket
(provide (all-defined-out))

;;; Flags

(define all-flags
  #hash([precision . (double fallback)]
        [setup . (simplify search)]
        [generate . (rr taylor simplify better-rr)]
        [reduce . (regimes avg-error binary-search branch-expressions)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic numerics special bools branches)]))

(define default-flags
  #hash([precision . (fallback)]
        [setup . (simplify search)]
        [generate . (rr taylor simplify)]
        [reduce . (regimes avg-error binary-search branch-expressions)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic numerics special bools branches)]))

(define (check-flag-deprecated! category flag)
  (match* (category flag)
    [('precision 'double)
     (eprintf "The precision:double option has been removed.\n")
     (eprintf "  Please use :precision binary32 and :precision binary64 instead.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/input.html> for more.\n" *herbie-version*)]
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
(define *max-find-range-depth* (make-parameter 14))

;; The maximum number of consecutive skipped points for sampling valid points
(define *max-skipped-points* (make-parameter 100))

;; Maximum MPFR precision allowed during exact evaluation
(define *max-mpfr-prec* (make-parameter 10000))

;; The maximum size of an egraph
(define *node-limit* (make-parameter 5000))

;; In localization, the maximum number of locations returned
(define *localize-expressions-limit* (make-parameter 4))
(define *localize-limit-for-new* (make-parameter #f))

;; How accurate to make the binary search
(define *binary-search-test-points* (make-parameter 16))
(define *binary-search-accuracy* (make-parameter 48))

;; Pherbie related options
(define *pareto-mode* (make-parameter #f))
(define *pareto-pick-limit* (make-parameter 5))

;; In mainloop, cache improvements between iterations
(define *use-improve-cache* (make-parameter #t))

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

(define *herbie-version* "1.5")

(define *hostname* (run-command "hostname"))

(define *herbie-commit*
  (git-command "rev-parse" "HEAD" #:default *herbie-version*))

(define *herbie-branch*
  (git-command "rev-parse" "--abbrev-ref" "HEAD" #:default "release"))

;;; The "reset" mechanism for clearing caches and such

(define resetters '())

(define (register-reset fn #:priority [priority 0])
  (set! resetters (cons (cons priority fn) resetters)))

(define (reset!)
  (for ([fn-rec (sort resetters < #:key car)]) ((cdr fn-rec))))

;; OBSOLETE

;; The step size with which arbitrary-precision precision is increased
;; DANGEROUS TO CHANGE
(define *precision-step* (make-parameter 256))

;; In periodicity analysis,
;; this is how small the period of a function must be to count as periodic
(define *max-period-coeff* 20)
