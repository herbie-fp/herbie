#lang racket

(require math/bigfloat
         math/flonum
         (only-in fpbench interval range-table-ref condition->range-table))
(require "../syntax/batch.rkt"
         "../syntax/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/rival.rkt"
         "../utils/timeline.rkt"
         "alternative.rkt"
         "batch-reduce.rkt"
         "points.rkt"
         "programs.rkt"
         "taylor.rkt")

(provide compute-taylor-covers
         taylor-covers-precondition
         wrap-taylor-cover-alts)

;; A cover replaces the program with `expr` on the closed interval [lo, hi] of
;; the variable. One endpoint is infinite for the covers at infinity.
(struct taylor-cover (name var var-repr out-repr lo hi expr exponent))

;; How many nonzero series terms to look at: the one the cover keeps, plus the
;; ones it drops.
(define terms-considered 3)

;; Relative error target for dropping the rest of the series: one ulp, the error
;; that rounding the result can already introduce.
(define (precision-epsilon repr)
  (match (representation-name repr)
    ['binary64 (expt 2 -53)]
    ['binary32 (expt 2 -24)]
    [_ #f]))

;; Covers test the variable against a float boundary, so the variable must be a
;; format whose values compare with `<=`.
(define (unary-real-variable ctx)
  (match (context-vars ctx)
    [(list var) (and (precision-epsilon (context-lookup ctx var)) var)]
    [_ #f]))

;; The bounds of the first precondition interval that a cover on [lo, hi] can
;; apply to. A cover no sampled point can reach is useless.
(define (overlapping-bounds intervals lo hi)
  (for/first ([iv (in-list intervals)]
              #:do [(match-define (interval iv-lo iv-hi _ _) iv)]
              #:when (and (< iv-lo hi) (< lo iv-hi)))
    (cons iv-lo iv-hi)))

;; Round an endpoint into the interval, so the covered region never grows past
;; the radius the series was checked on, or past the precondition.
(define (round-inward value dir repr)
  (define rounded
    (parameterize ([bf-rounding-mode dir])
      ((representation-bf->repr repr) (bf value))))
  (and (rational? rounded) rounded))

;; A cover at infinity starts at 1/radius. Round up, so the covered tail stays
;; inside the radius.
(define (reciprocal-up radius repr)
  (define value
    (parameterize ([bf-rounding-mode 'up])
      ((representation-bf->repr repr) (bf/ 1.bf (bf radius)))))
  (and (rational? value) (positive? value) value))

(define (predecessor value repr)
  ((representation-ordinal->repr repr) (sub1 ((representation-repr->ordinal repr) value))))

;; Dropping every term after the first costs at most `epsilon` relative error
;; while the transformed variable stays inside this radius. Every dropped term is
;; checked, not just the first one, because a small next coefficient otherwise
;; inflates the radius past where the series is accurate.
(define (cover-radius coeffs exponents epsilon)
  (define kept (first coeffs))
  (define kept-exponent (first exponents))
  (for/fold ([radius #f])
            ([coeff (in-list (rest coeffs))]
             [exponent (in-list (rest exponents))])
    (define candidate
      (and (not (zero? coeff))
           (expt (* epsilon (/ (abs kept) (abs coeff))) (/ 1 (- exponent kept-exponent)))))
    (if (and (rational? candidate) (positive? candidate))
        (min candidate (or radius candidate))
        radius)))

;; The interval of inputs the cover applies to, or #f if no sampled point could
;; land in it.
(define (cover-interval name radius var-repr intervals)
  (match name
    [0
     (define bounds (overlapping-bounds intervals (- radius) radius))
     (define lo (and bounds (round-inward (max (- radius) (car bounds)) 'up var-repr)))
     (define hi (and bounds (round-inward (min radius (cdr bounds)) 'down var-repr)))
     (and lo hi (< lo hi) (cons lo hi))]
    ['inf
     (define threshold (reciprocal-up radius var-repr))
     (and threshold (overlapping-bounds intervals threshold +inf.0) (cons threshold +inf.0))]
    ['-inf
     (define threshold (reciprocal-up radius var-repr))
     (and threshold
          (overlapping-bounds intervals -inf.0 (- threshold))
          (cons -inf.0 (- threshold)))]))

;; Evaluate every coefficient of the series in one Rival machine. A coefficient
;; that still mentions the variable means the series did not resolve here.
(define (coefficient-values batch coeffs out-repr)
  (define exprs (map (batch-exprs batch) coeffs))
  (cond
    [(not (andmap (lambda (expr) (null? (free-variables expr))) exprs)) #f]
    [else
     (define ctx (context '() out-repr '()))
     (define-values (const-batch brfs) (progs->batch exprs #:ctx ctx))
     (define compiler (make-real-compiler const-batch brfs (map (const out-repr) exprs)))
     (define-values (status outs) (real-apply compiler (vector)))
     (define nums (and (equal? status 'valid) (map (lambda (out) (repr->real out out-repr)) outs)))
     (and nums (andmap rational? nums) nums)]))

;; Rewrite the kept term back into the original variable. The covers at infinity
;; expand in 1/x and -1/x, so the exponent flips and odd powers of -1/x flip sign.
(define (term-in-input name coeff exponent)
  (match name
    [0 (values coeff exponent)]
    ['inf (values coeff (- exponent))]
    ['-inf
     (values (if (odd? exponent)
                 (- coeff)
                 coeff)
             (- exponent))]))

;; A term with exponent -1 divides rather than multiplying by a reciprocal, so it
;; costs one rounding instead of two.
(define (taylor-term->spec coeff exponent var)
  (define monomial
    (match exponent
      [1 var]
      [_ `(pow ,var ,exponent)]))
  (match* (coeff exponent)
    [(_ 0) coeff]
    [(_ -1) `(/ ,coeff ,var)]
    [(1 _) monomial]
    [(-1 _) `(neg ,monomial)]
    [(_ _) `(* ,coeff ,monomial)]))

(define (build-cover batch series transform ctx var var-repr epsilon intervals)
  (match-define (list name forward inverse) transform)
  (define out-repr (context-repr ctx))
  (let/ec return
    (define next-term
      (make-taylor-term-generator series batch var #:transform (cons forward inverse)))
    (define terms (build-list terms-considered (lambda (_) (next-term))))
    (define exponents (map taylor-term-exponent terms))
    ;; A fractional exponent would raise a negative input to a fractional power,
    ;; which is undefined, and every cover but the one at +infinity spans
    ;; negative inputs.
    (unless (andmap exact-integer? exponents)
      (return #f))
    (define coeffs (coefficient-values batch (map taylor-term-coeff terms) out-repr))
    (unless coeffs
      (return #f))
    (define radius (cover-radius coeffs exponents epsilon))
    (unless radius
      (return #f))
    (define bounds (cover-interval name radius var-repr intervals))
    (unless bounds
      (return #f))
    (define-values (coeff exponent) (term-in-input name (first coeffs) (first exponents)))
    (taylor-cover name
                  var
                  var-repr
                  out-repr
                  (car bounds)
                  (cdr bounds)
                  (fpcore->prog (taylor-term->spec coeff exponent var) ctx)
                  (first exponents))))

;; The error bound is not rigorous, so a cover is only worth a branch if the one
;; term it keeps really does beat the program it replaces, on the training points
;; the cover claims.
(define (cover-improves? cover expr pcontext ctx)
  (define lo (taylor-cover-lo cover))
  (define hi (taylor-cover-hi cover))
  (match-define (list cover-errs expr-errs)
    (exprs-errors (list (taylor-cover-expr cover) expr) pcontext ctx))
  (define-values (cover-total expr-total)
    (for/fold ([cover-total 0.0]
               [expr-total 0.0])
              ([(pt _) (in-pcontext pcontext)]
               [cover-err (in-flvector cover-errs)]
               [expr-err (in-flvector expr-errs)]
               #:when (<= lo (vector-ref pt 0) hi))
      (values (+ cover-total cover-err) (+ expr-total expr-err))))
  (< cover-total expr-total))

(define (compute-taylor-covers spec pre expr pcontext ctx)
  (define var (unary-real-variable ctx))
  (define out-repr (context-repr ctx))
  (define epsilon (precision-epsilon out-repr))
  (cond
    [(not (and var epsilon)) '()]
    [else
     (timeline-event! 'series)
     (define var-repr (context-lookup ctx var))
     (define intervals (range-table-ref (condition->range-table pre) var))
     (define-values (batch brfs) (progs->batch (list spec) #:ctx ctx))
     (define (try-cover transform coefficients)
       (define cover
         (build-cover batch (first coefficients) transform ctx var var-repr epsilon intervals))
       (and cover
            (let ([kept? (cover-improves? cover expr pcontext ctx)])
              (timeline-push! 'taylor-count
                              (~a (first transform))
                              (taylor-cover-exponent cover)
                              1
                              1
                              (if kept? 1 0))
              (and kept? cover))))
     (parameterize ([reduce (batch-reduce batch)]
                    [add (lambda (x) (batch-add! batch x))])
       (filter-map try-cover
                   taylor-transforms
                   (taylor-coefficients batch brfs (list var) taylor-transforms)))]))

;; The search skips whatever the covers already handle.
(define (cover-outside cover)
  (define var (taylor-cover-var cover))
  (define var-repr (taylor-cover-var-repr cover))
  (define lo (taylor-cover-lo cover))
  (define hi (taylor-cover-hi cover))
  (define (bound value)
    (repr->real value var-repr))
  (cond
    [(infinite? lo) `(< ,(bound hi) ,var)]
    [(infinite? hi) `(< ,var ,(bound lo))]
    [else `(or (< ,var ,(bound lo)) (< ,(bound hi) ,var))]))

(define (taylor-covers-precondition pre covers)
  (for/fold ([pre pre]) ([cover (in-list covers)])
    `(and ,pre ,(cover-outside cover))))

(define (cover-condition cover)
  (define var (taylor-cover-var cover))
  (define var-repr (taylor-cover-var-repr cover))
  (define lo (taylor-cover-lo cover))
  (define hi (taylor-cover-hi cover))
  (define <=-impl (get-fpcore-impl '<= '() (list var-repr var-repr)))
  (define (bound value)
    (literal (repr->real value var-repr) (representation-name var-repr)))
  (cond
    [(infinite? lo) `(,<=-impl ,var ,(bound hi))]
    [(infinite? hi) `(,<=-impl ,(bound lo) ,var)]
    [else
     (define and-impl (get-fpcore-impl 'and '() (list <bool> <bool>)))
     `(,and-impl (,<=-impl ,(bound lo) ,var) (,<=-impl ,var ,(bound hi)))]))

(define (cover-splitpoints cover)
  (define var (taylor-cover-var cover))
  (define var-repr (taylor-cover-var-repr cover))
  (define lo (taylor-cover-lo cover))
  (define hi (taylor-cover-hi cover))
  (cond
    [(infinite? lo) (list (sp 1 var hi) (sp 0 var +nan.0))]
    [(infinite? hi) (list (sp 0 var (predecessor lo var-repr)) (sp 1 var +nan.0))]
    [else (list (sp 0 var (predecessor lo var-repr)) (sp 1 var hi) (sp 0 var +nan.0))]))

(define (wrap-cover altn cover)
  (define out-repr (taylor-cover-out-repr cover))
  (define taylor-expr (taylor-cover-expr cover))
  (define if-impl (get-fpcore-impl 'if '() (list <bool> out-repr out-repr)))
  (define taylor-altn
    (alt taylor-expr
         `(taylor ,(alt-expr altn)
                  ,(taylor-cover-name cover)
                  ,(taylor-cover-var cover)
                  ,(taylor-cover-exponent cover))
         (list altn)))
  (alt `(,if-impl ,(cover-condition cover) ,taylor-expr ,(alt-expr altn))
       `(regimes ,(cover-splitpoints cover))
       (list altn taylor-altn)))

;; Report the covered and uncovered forms of every alternative. A cover that
;; turns out to cost more than it saves is then outvoted by the error and cost
;; sort, instead of being forced on every result.
(define (wrap-taylor-cover-alts altns covers)
  (cond
    [(null? covers) altns]
    [else
     (append altns
             (for/list ([altn (in-list altns)])
               (for/fold ([altn altn]) ([cover (in-list covers)])
                 (wrap-cover altn cover))))]))
