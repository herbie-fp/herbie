#lang racket

(provide (all-defined-out))

;;; Flags

(define all-flags
  #hash([precision . (double fallback)]
        [setup . (simplify search)]
        [localize . (costs errors)]
        [generate . (rr taylor simplify better-rr proofs egglog evaluate)]
        [reduce . (regimes avg-error binary-search branch-expressions simplify)]
        [rules
         . (arithmetic polynomials
                       fractions
                       exponents
                       trigonometry
                       hyperbolic
                       numerics
                       special
                       bools
                       branches)]
        [dump . (egg rival egglog trace)]))

(define default-flags
  #hash([precision . ()]
        [setup . (search)]
        [localize . ()]
        [generate . (rr taylor proofs evaluate)]
        [reduce . (regimes binary-search branch-expressions)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic)]
        [dump . ()]))

(define (flag-deprecated? category flag)
  (match* (category flag)
    [('precision 'double) #t]
    [('precision 'fallback) #t]
    [('setup 'simplify) #t]
    [('generate 'better-rr) #t]
    [('generate 'simplify) #t]
    [('reduce 'simplify) #t]
    [('reduce 'avg-error) #t]
    [('localize 'costs) #t]
    [('localize 'errors) #t]
    [('rules 'numerics) #t]
    [('rules 'special) #t]
    [('rules 'bools) #t]
    [('rules 'branches) #t]
    [(_ _) #f]))

; `hash-copy` returns a mutable hash, which makes `dict-update` invalid
(define *flags* (make-parameter (make-immutable-hash (hash->list default-flags))))

(define (flag-set? class flag)
  (set-member? (dict-ref (*flags*) class) flag))

(define (enable-flag! category flag)
  (when (flag-deprecated? category flag)
    (warn-flag-deprecated! category flag))
  (define (update cat-flags)
    (set-add cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (disable-flag! category flag)
  (when (flag-deprecated? category flag)
    (warn-flag-deprecated! category flag))
  (define (update cat-flags)
    (set-remove cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (warn-flag-deprecated! category flag)
  (match* (category flag)
    [('precision 'double)
     (eprintf "The precision:double option has been removed.\n")
     (eprintf "  The double-precision representation is specified with :precision binary64.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/input.html> for more.\n" *herbie-version*)]
    [('precision 'fallback)
     (eprintf "The precision:fallback option has been removed.\n")
     (eprintf "  The fallback representation is specified with :precision racket.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/input.html> for more.\n" *herbie-version*)]
    [('setup 'simplify)
     (eprintf "The setup:simplify option has been removed.\n")
     (eprintf "  Initial simplification is no longer needed.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [('generate 'better-rr)
     (eprintf "The generate:better-rr option has been removed.\n")
     (eprintf "  The current recursive rewriter does not support the it.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [('generate 'simplify)
     (eprintf "The generate:simplify option has been removed.\n")
     (eprintf "  Simplification is no longer performed as a separate step.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [('reduce 'simplify)
     (eprintf "The reduce:simplify option has been removed.\n")
     (eprintf "  Final-simplification is no longer performed.\n")
     (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [('reduce 'avg-error)
     (eprintf "The reduce:avg-error option has been removed.\n")
     (eprintf "  Herbie now always uses average error for pruning.\n")
     (eprintf "See <herbie://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [('localize 'costs)
     (eprintf "The localize:costs option has been removed.\n")
     (eprintf "  Herbie no longer performs localization.\n")
     (eprintf "See <herbie://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [('localize 'errors)
     (eprintf "The localize:errors option has been removed.\n")
     (eprintf "  Herbie no longer performs localization.\n")
     (eprintf "See <herbie://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [('rules _)
     (eprintf "The rules:~a ruleset has been removed.\n")
     (eprintf "  These rules are no longer used by Herbie.\n")
     (eprintf "See <herbie://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
    [(_ _) (void)]))

(define (changed-flags)
  (filter identity
          (for*/list ([(class flags) all-flags]
                      [flag flags])
            (match* ((flag-set? class flag) (parameterize ([*flags* default-flags])
                                              (flag-set? class flag)))
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

;; Maximum MPFR precision allowed during exact evaluation
(define *max-mpfr-prec* (make-parameter 10000))

;; The maximum size of an egraph
(define *node-limit* (make-parameter 4000))
(define *proof-max-length* (make-parameter 200))
(define *proof-max-string-length* (make-parameter 10000))

;; How long of a Taylor series to generate; too long and we time out
(define *taylor-order-limit* (make-parameter 4))

;; How accurate to make the binary search
(define *binary-search-test-points* (make-parameter 16))
(define *binary-search-accuracy* (make-parameter 48))

;; Pherbie related options
(define *pareto-pick-limit* (make-parameter 5))

;; If `:precision` is unspecified, which representation should we use?
(define *default-precision* (make-parameter 'binary64))

;; The platform that Herbie will evaluate with.
(define *platform-name* (make-parameter (if (equal? (system-type 'os) 'windows) "c-windows" "c")))

;; Sets the number of total points for Herbie to sample.
(define *reeval-pts* (make-parameter 8000))

;; Time out for a given run. 2.5 minutes currently.
(define *timeout* (make-parameter (* 1000 60 5/2)))

;; The number of variants extracted from egglog
(define *egglog-variants-limit* (make-parameter 1000000))

;; The number of iterations for the egglog search
(define *default-egglog-iter-limit* (make-parameter 50))

;;; The random seed

(define the-seed #f)

(define (get-seed)
  (or the-seed (error "Seed is not set yet!")))

(define (set-seed! seed)
  "Reset the random number generator to a new seed"
  (set! the-seed seed)
  (if (vector? seed)
      (current-pseudo-random-generator (vector->pseudo-random-generator seed))
      (random-seed seed)))

;;; About Herbie:

(define (run-command cmd)
  (parameterize ([current-error-port (open-output-nowhere)])
    (string-trim (with-output-to-string (λ () (system cmd))))))

(define (git-command #:default [default ""] gitcmd . args)
  (cond
    [(or (directory-exists? ".git") (file-exists? ".git")) ; gitlinks like for worktrees
     (define cmd (format "git ~a ~a" gitcmd (string-join args " ")))
     (define out (run-command cmd))
     (if (equal? out "") default out)]
    [else default]))

(define *herbie-version* "2.3")

(define *herbie-commit* (git-command "rev-parse" "HEAD" #:default *herbie-version*))

(define *herbie-branch* (git-command "rev-parse" "--abbrev-ref" "HEAD" #:default "release"))

;;; The "reset" mechanism for clearing caches and such

(define resetters '())
(define (register-resetter! fn)
  (set! resetters (cons fn resetters)))

(define (reset!)
  (for ([fn (in-list resetters)])
    (fn)))

(define-syntax define/reset
  (syntax-rules ()
    ; default resetter sets parameter to `value`
    [(_ name value) (define/reset name value (λ () (name value)))]
    ; initial value and resetter
    [(_ name value reset-fn)
     (define name
       (let ([param (make-parameter value)])
         (register-resetter! reset-fn)
         param))]))
