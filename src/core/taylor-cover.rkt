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
         "taylor.rkt"
         "taylor-model.rkt")

(provide compute-taylor-covers
         taylor-covers-precondition
         wrap-taylor-cover-alts)

(struct taylor-cover (name var var-repr out-repr lo hi expr exponent))

(define (precision-epsilon repr)
  (match (representation-name repr)
    ['binary64 (expt 2 -53)]
    ['binary32 (expt 2 -24)]
    [_ #f]))

(define (unary-real-variable ctx)
  (match (context-vars ctx)
    [(list var) (and (precision-epsilon (context-lookup ctx var)) var)]
    [_ #f]))

(define (overlapping-bounds intervals lo hi)
  (for/first ([iv (in-list intervals)]
              #:do [(match-define (interval iv-lo iv-hi _ _) iv)]
              #:when (and (< iv-lo hi) (< lo iv-hi)))
    (cons iv-lo iv-hi)))

(define (round-inward value dir repr)
  (define rounded
    (parameterize ([bf-rounding-mode dir])
      ((representation-bf->repr repr) value)))
  (and (rational? rounded) rounded))

(define (reciprocal-up radius repr)
  (define value
    (round-inward (parameterize ([bf-rounding-mode 'up])
                    (bf/ 1.bf (bf radius)))
                  'up
                  repr))
  (and value (positive? value) value))

(define (predecessor value repr)
  ((representation-ordinal->repr repr) (sub1 ((representation-repr->ordinal repr) value))))

;; Unrigorous starting point for sound interval.
(define (cover-candidate coeffs exponents epsilon var-repr)
  (match-define (list kept dropped) coeffs)
  (match-define (list kept-exponent dropped-exponent) exponents)
  (define radius
    (and (not (zero? dropped))
         (expt (* epsilon (/ (abs kept) (abs dropped))) (/ 1 (- dropped-exponent kept-exponent)))))
  (and (rational? radius)
       (positive? radius)
       (let ([rounded (parameterize ([bf-rounding-mode 'up])
                        ((representation-bf->repr var-repr) (bf radius)))])
         (and (rational? rounded) (positive? rounded) rounded))))

(define (cover-domain name radius)
  (if (equal? name 0)
      (ival (bf (- radius)) (bf radius))
      (ival (bf 0) (bf radius))))

;; Largest radius <= candidate that the model certifies.
(define (certified-radius model coeff epsilon candidate var-repr)
  (define ->radius (representation-ordinal->repr var-repr))
  (define (fits? ordinal)
    (taylor-model-fits? model coeff epsilon (->radius ordinal)))
  (and (fits? 0)
       (->radius (let loop ([lo 0]
                            [hi (add1 ((representation-repr->ordinal var-repr) candidate))])
                   (define mid (quotient (+ lo hi) 2))
                   (cond
                     [(= mid lo) lo]
                     [(fits? mid) (loop mid hi)]
                     [else (loop lo mid)])))))

(define (cover-interval name radius var-repr intervals)
  (match name
    [0
     (define bounds (overlapping-bounds intervals (- radius) radius))
     (define lo (and bounds (round-inward (bfmax (bf (- radius)) (bf (car bounds))) 'up var-repr)))
     (define hi (and bounds (round-inward (bfmin (bf radius) (bf (cdr bounds))) 'down var-repr)))
     (and lo hi (< lo hi) (cons lo hi))]
    ['inf
     (define threshold (reciprocal-up radius var-repr))
     (and threshold (overlapping-bounds intervals threshold +inf.0) (cons threshold +inf.0))]
    ['-inf
     (define threshold (reciprocal-up radius var-repr))
     (and threshold
          (overlapping-bounds intervals -inf.0 (- threshold))
          (cons -inf.0 (- threshold)))]))

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

;; The covers at infinity expand in 1/x and -1/x, so the exponent flips and odd
;; powers of -1/x flip sign.
(define (taylor-term->spec name coeff exponent var)
  (define-values (coeff* exponent*)
    (match name
      [0 (values coeff exponent)]
      ['inf (values coeff (- exponent))]
      ['-inf
       (values (if (odd? exponent)
                   (- coeff)
                   coeff)
               (- exponent))]))
  (define monomial
    (match exponent*
      [1 var]
      [_ `(pow ,var ,exponent*)]))
  (match* (coeff* exponent*)
    [(_ 0) coeff*]
    [(_ -1) `(/ ,coeff* ,var)] ; Save cost
    [(1 _) monomial]
    [(-1 _) `(neg ,monomial)]
    [(_ _) `(* ,coeff* ,monomial)]))

(define (cover-model batch brf transform var radius)
  (match-define (list name forward _) transform)
  (define replaced ((batch-replace-expression! batch var (forward var)) brf))
  (taylor-model batch ((expand-taylor! batch) ((reduce) replaced)) var (cover-domain name radius)))

(define (build-cover batch brf series transform ctx var var-repr epsilon intervals)
  (match-define (list name forward inverse) transform)
  (define out-repr (context-repr ctx))
  (define next-term (car (approximate (list series) batch var #:transform (cons forward inverse))))
  ;; The term the cover keeps, and the first one it drops
  (define terms (list (next-term) (next-term)))
  (define exponents (map taylor-term-exponent terms))
  (define coeffs
    (and (andmap exact-integer? exponents) ; No fractional exponents, and the series went on
         (coefficient-values batch (map taylor-term-coeff terms) out-repr)))
  (define candidate (and coeffs (cover-candidate coeffs exponents epsilon var-repr)))
  (define model (and candidate (cover-model batch brf transform var candidate)))
  (define radius
    (and model
         (equal? (tmodel-offset model) (first exponents))
         (certified-radius model (first coeffs) epsilon candidate var-repr)))
  (define bounds (and radius (cover-interval name radius var-repr intervals)))
  (and bounds
       (taylor-cover name
                     var
                     var-repr
                     out-repr
                     (car bounds)
                     (cdr bounds)
                     (fpcore->prog (taylor-term->spec name (first coeffs) (first exponents) var) ctx)
                     (first exponents))))

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
         (build-cover batch
                      (first brfs)
                      (first coefficients)
                      transform
                      ctx
                      var
                      var-repr
                      epsilon
                      intervals))
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

;; Skip whatever the covers already handle.
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

;; Report the covered and uncovered forms of every alternative.
(define (wrap-taylor-cover-alts altns covers)
  (cond
    [(null? covers) altns]
    [else
     (append altns
             (for/list ([altn (in-list altns)])
               (for/fold ([altn altn]) ([cover (in-list covers)])
                 (wrap-cover altn cover))))]))
