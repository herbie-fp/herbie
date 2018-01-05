#lang racket
(require racket/runtime-path)
(provide (all-defined-out))

(define-runtime-path viz-output-path "../www/viz/")
(define-runtime-path web-resource-path "web/")

;; Flag Stuff

(define all-flags
  #hash([precision . (double)]
        [setup . (simplify early-exit)]
        [generate . (rr taylor simplify)]
        [reduce . (regimes taylor simplify avg-error post-process binary-search)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic numerics)]))

(define default-flags
  #hash([precision . (double)]
        [setup . (simplify)]
        [generate . (rr taylor simplify)]
        [reduce . (regimes taylor simplify avg-error)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic)]))

(define (enable-flag! category flag)
  (define (update cat-flags) (set-add cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (disable-flag! category flag)
  (define (update cat-flags) (set-remove cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (has-flag? class flag)
  (set-member? (dict-ref (*flags*) class) flag))

; `hash-copy` returns a mutable hash, which makes `dict-update` invalid
(define *flags* (make-parameter (make-immutable-hash (hash->list default-flags))))

(define (changed-flags)
  (filter identity
          (for*/list ([(class flags) all-flags] [flag flags])
            (match* ((has-flag? class flag)
                     (parameterize ([*flags* default-flags]) (has-flag? class flag)))
              [(#t #t) #f]
              [(#f #f) #f]
              [(#t #f) (list 'enabled class flag)]
              [(#f #t) (list 'disabled class flag)]))))

(define ((flag type f) a b)
  (if (has-flag? type f) a b))

;; Number of points to sample for evaluating program accuracy
(define *num-points* (make-parameter 256))

;; Number of iterations of the core loop for improving program accuracy
(define *num-iterations* (make-parameter 3))

;; The step size with which arbitrary-precision precision is increased
;; DANGEROUS TO CHANGE
(define *precision-step* (make-parameter 256))

;; When doing a binary search in regime inference,
;; this is the fraction of the gap between two points that the search must reach
(define *epsilon-fraction* (/ 1 200))

;; In periodicity analysis,
;; this is how small the period of a function must be to count as periodic
(define *max-period-coeff* 20)

;; In localization, the maximum number of locations returned
(define *localize-expressions-limit* (make-parameter 4))

(define *binary-search-test-points* (make-parameter 16))

;;; About Herbie:

(define (git-command #:default [default ""] gitcmd . args)
  (if (directory-exists? ".git")
      (let ([cmd (format "git ~a ~a" gitcmd (string-join args " "))])
        (or (string-trim (with-output-to-string (λ () (system cmd)))) default))
      default))

(define *herbie-version* "1.1")

(define *herbie-commit*
  (git-command "rev-parse" "HEAD" #:default *herbie-version*))

(define *herbie-branch*
  (git-command "rev-parse" "--abbrev-ref" "HEAD" #:default "release"))
