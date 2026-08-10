#lang racket

(require math/flonum
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

(struct taylor-cover (name var var-repr out-repr lo hi expr exponent))

(define (precision-epsilon repr)
  (match (representation-name repr)
    ['binary64 (expt 2 -53)]
    ['binary32 (expt 2 -24)]
    [_ #f]))

(define (overlapping-bounds intervals lo hi)
  (for/or ([iv (in-list intervals)])
    (match-define (interval iv-lo iv-hi _ _) iv)
    (and (< iv-lo hi) (< lo iv-hi) (cons iv-lo iv-hi))))

(define (predecessor value repr)
  ((representation-ordinal->repr repr) (sub1 ((representation-repr->ordinal repr) value))))

(define (cover-radius coeffs exponents epsilon)
  (match-define (list kept dropped) coeffs)
  (match-define (list kept-exponent dropped-exponent) exponents)
  (expt (* epsilon (/ (abs kept) (abs dropped))) (/ 1 (- dropped-exponent kept-exponent))))

(define (cover-interval name radius var-repr intervals)
  (match name
    [0
     (match (overlapping-bounds intervals (- radius) radius)
       [(cons iv-lo iv-hi)
        (define lo (real->repr (max (- radius) iv-lo) var-repr))
        (define hi (real->repr (min radius iv-hi) var-repr))
        ;; Rational checks to guard against infinities
        (and (rational? lo) (rational? hi) (< lo hi) (cons lo hi))]
       [#f #f])]
    ['inf
     (define threshold (real->repr (/ 1 radius) var-repr))
     (and (rational? threshold)
          (positive? threshold)
          (overlapping-bounds intervals threshold +inf.0)
          (cons threshold +inf.0))]
    ['-inf
     (define threshold (real->repr (/ 1 radius) var-repr))
     (and (rational? threshold)
          (positive? threshold)
          (overlapping-bounds intervals -inf.0 (- threshold))
          (cons -inf.0 (- threshold)))]))

(define (coefficient-values batch coeffs out-repr)
  (define exprs (map (batch-exprs batch) coeffs))
  (define ctx (context '() out-repr '()))
  (define-values (const-batch brfs) (progs->batch exprs #:ctx ctx))
  (define compiler (make-real-compiler const-batch brfs (map (const out-repr) exprs)))
  (define-values (status outs) (real-apply compiler (vector)))
  (define nums (and (equal? status 'valid) (map (lambda (out) (repr->real out out-repr)) outs)))
  (and nums (andmap (lambda (n) (and (rational? n) (not (zero? n)))) nums) nums))

(define (build-cover batch series transform ctx epsilon intervals)
  (match-define (list name forward inverse) transform)
  (match-define (context (list var) out-repr (list var-repr)) ctx)
  (define tform (cons forward inverse))
  (define next-term (taylor-terms series batch var #:transform tform))
  (define terms (list (next-term) (next-term)))
  ;; Ensure both coefficients are pure constants and then evaluate
  (define coeffs
    (and (andmap (lambda (term) (and term (null? (free-variables ((batch-exprs batch) (car term))))))
                 terms)
         (coefficient-values batch (map car terms) out-repr)))
  (define radius (and coeffs (cover-radius coeffs (map cdr terms) epsilon)))
  (define bounds (and radius (cover-interval name radius var-repr intervals)))
  (and bounds
       (let ([kept-term (cons (first coeffs) (cdr (first terms)))])
         (taylor-cover
          name
          var
          var-repr
          out-repr
          (car bounds)
          (cdr bounds)
          (fpcore->prog ((batch-exprs batch) (horner-form (list kept-term) var #:transform tform))
                        ctx)
          (cdr kept-term)))))

;; A cover is likely only worth a branch if it beats the original program on
;; the initial sample of training points.
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
  (define epsilon (precision-epsilon (context-repr ctx)))
  (match (context-vars ctx)
    [(list var) ; For now, covers only apply to univariate functions
     #:when epsilon
     (timeline-event! 'series)
     (define intervals (range-table-ref (condition->range-table pre) var))
     (define-values (batch brfs) (progs->batch (list spec) #:ctx ctx))
     (define (try-cover transform coefficients)
       (define cover (build-cover batch (first coefficients) transform ctx epsilon intervals))
       (cond
         [cover
          (define kept? (cover-improves? cover expr pcontext ctx))
          (timeline-push! 'taylor-count
                          (~a (first transform))
                          (taylor-cover-exponent cover)
                          1
                          1
                          (if kept? 1 0))
          (and kept? cover)]
         [else #f]))
     (parameterize ([reduce (batch-reduce batch)]
                    [add (lambda (x) (batch-add! batch x))])
       (filter-map try-cover
                   taylor-transforms
                   (taylor-coefficients batch brfs (list var) taylor-transforms)))]
    [_ '()]))

(define (cover-outside cover)
  (match-define (taylor-cover _ var var-repr _ lo hi _ _) cover)
  (define (bound value)
    (repr->real value var-repr))
  (cond
    [(infinite? lo) `(< ,(bound hi) ,var)]
    [(infinite? hi) `(< ,var ,(bound lo))]
    [else `(or (< ,var ,(bound lo)) (< ,(bound hi) ,var))]))

;; Skip whatever the covers already handle.
(define (taylor-covers-precondition pre covers)
  (for/fold ([pre pre]) ([cover (in-list covers)])
    `(and ,pre ,(cover-outside cover))))

(define (cover-condition cover)
  (match-define (taylor-cover _ var var-repr _ lo hi _ _) cover)
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
  (match-define (taylor-cover _ var var-repr _ lo hi _ _) cover)
  (cond
    [(infinite? lo) (list (sp 1 var hi) (sp 0 var +nan.0))]
    [(infinite? hi) (list (sp 0 var (predecessor lo var-repr)) (sp 1 var +nan.0))]
    [else (list (sp 0 var (predecessor lo var-repr)) (sp 1 var hi) (sp 0 var +nan.0))]))

(define (wrap-cover altn cover)
  (match-define (taylor-cover name var _ out-repr _ _ taylor-expr exponent) cover)
  (define if-impl (get-fpcore-impl 'if '() (list <bool> out-repr out-repr)))
  (define taylor-altn (alt taylor-expr `(taylor ,(alt-expr altn) ,name ,var ,exponent) (list altn)))
  (alt `(,if-impl ,(cover-condition cover) ,taylor-expr ,(alt-expr altn))
       `(regimes ,(cover-splitpoints cover))
       (list altn taylor-altn)))

;; Report the covered and uncovered forms of every alternative.
(define (wrap-taylor-cover-alts altns covers)
  (cond
    [(null? covers) altns]
    [else
     (append altns
             (for/list ([altn (in-list altns)])
               (for/fold ([altn altn]) ([cover (in-list covers)])
                 (wrap-cover altn cover))))]))
