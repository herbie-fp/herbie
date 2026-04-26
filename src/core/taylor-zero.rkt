#lang racket

(require math/bigfloat
         (only-in fpbench interval range-table-ref condition->range-table))
(require "../config.rkt"
         "../syntax/batch.rkt"
         "../syntax/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/rival.rkt"
         "../utils/errors.rkt"
         "alternative.rkt"
         "batch-reduce.rkt"
         "programs.rkt"
         "taylor.rkt")

(provide (struct-out taylor-zero-cover)
         compute-taylor-zero-cover
         taylor-zero-precondition
         wrap-taylor-zero-alts)

(struct taylor-zero-cover
        (var var-repr out-repr radius radius-value taylor-expr kept-exponent omitted-exponent)
  #:transparent)

(define transforms-at-zero (list (list 'zero identity identity)))

(define (real-representation? repr)
  (equal? (representation-type repr) 'real))

(define (unary-real-variable ctx)
  (match (context-vars ctx)
    [(list var)
     (define repr (context-lookup ctx var))
     (and (real-representation? repr) var)]
    [_ #f]))

(define (precision-epsilon repr)
  (match (representation-name repr)
    ['binary64 (expt 2 -53)]
    ['binary32 (expt 2 -24)]
    [_ #f]))

(define (finite-real? x)
  (and (real? x) (not (nan? x)) (not (infinite? x))))

(define (zero-contained? lo hi lo? hi?)
  (and (or (< lo 0) (and (= lo 0) lo?)) (or (< 0 hi) (and (= hi 0) hi?))))

(define (interval-cap pre var)
  (define range-table (condition->range-table pre))
  (for/first ([iv (in-list (range-table-ref range-table var))]
              #:do [(match-define (interval lo hi lo? hi?) iv)]
              #:when (zero-contained? lo hi lo? hi?))
    (min (if (infinite? lo)
             +inf.0
             (abs lo))
         (if (infinite? hi)
             +inf.0
             (abs hi)))))

(define (round-radius-down value cap repr)
  (define capped-bf (bf (min value cap)))
  (define rounded
    (parameterize ([bf-rounding-mode 'down])
      ((representation-bf->repr repr) capped-bf)))
  (define rounded-ordinal ((representation-repr->ordinal repr) rounded))
  (define rounded-bf ((representation-repr->bf repr) rounded))
  (define cap-bf (and (finite-real? cap) (bf cap)))
  (define ordinal
    (if (and cap-bf (>= value cap) (bf>= rounded-bf cap-bf))
        (sub1 rounded-ordinal)
        rounded-ordinal))
  (define radius-value ((representation-ordinal->repr repr) ordinal))
  (and (finite-real? radius-value) (positive? radius-value) radius-value))

(define (constant-value expr repr)
  (define-values (batch brfs) (progs->batch (list expr)))
  (define ctx (context '() repr '()))
  (define compiler (make-real-compiler batch brfs (list ctx)))
  (define-values (status values) (real-apply compiler (vector)))
  (and (equal? status 'valid) (repr->real (first values) repr)))

(define (coefficient-value batch coeff repr)
  (define expr ((batch-exprs batch) coeff))
  (and (null? (free-variables expr))
       (let ([value (constant-value expr repr)]) (and value (finite-real? value) value))))

(define (spec->impl expr ctx)
  (fpcore->prog (prog->fpcore expr ctx) ctx))

(define (build-taylor-zero-cover spec ctx var var-repr out-repr epsilon cap)
  (define-values (batch brfs) (progs->batch (list spec)))
  (parameterize ([reduce (batch-reduce batch)]
                 [add (lambda (x) (batch-add! batch x))])
    (define coeffs (taylor-coefficients batch brfs (list var) transforms-at-zero))
    (define series (caar coeffs))
    (define next-term (make-taylor-term-generator series batch var #:iters 8))
    (define kept (next-term))
    (define omitted (next-term))
    (cond
      [(not (taylor-term-coeff kept)) #f]
      [(not (taylor-term-coeff omitted)) #f]
      [(negative? (taylor-term-exponent kept)) #f]
      [else
       (define kept-value (coefficient-value batch (taylor-term-coeff kept) out-repr))
       (define omitted-value (coefficient-value batch (taylor-term-coeff omitted) out-repr))
       (cond
         [(not kept-value) #f]
         [(not omitted-value) #f]
         [(zero? omitted-value) #f]
         [else
          (define exponent-delta (- (taylor-term-exponent omitted) (taylor-term-exponent kept)))
          ;; Not Lagrange, but should be a decent starting point for the radius
          (define radius*
            (expt (* epsilon (/ (abs kept-value) (abs omitted-value))) (/ 1 exponent-delta)))
          (define radius-value (round-radius-down radius* cap var-repr))
          (cond
            [(not radius-value) #f]
            [else
             (define radius (repr->real radius-value var-repr))
             (define taylor-expr (spec->impl ((batch-exprs batch) (taylor-term-expr kept)) ctx))
             (taylor-zero-cover var
                                var-repr
                                out-repr
                                radius
                                radius-value
                                taylor-expr
                                (taylor-term-exponent kept)
                                (taylor-term-exponent omitted))])])])))

(define (compute-taylor-zero-cover spec pre ctx)
  (define var (unary-real-variable ctx))
  (cond
    [(not (flag-set? 'setup 'taylor-zero)) #f]
    [(not var) #f]
    [(not (real-representation? (context-repr ctx))) #f]
    [else
     (define var-repr (context-lookup ctx var))
     (define out-repr (context-repr ctx))
     (define epsilon (precision-epsilon out-repr))
     (define cap (and epsilon (interval-cap pre var)))
     (cond
       [(not epsilon) #f]
       [(not cap) #f]
       [(not (positive? cap)) #f]
       [else (build-taylor-zero-cover spec ctx var var-repr out-repr epsilon cap)])]))

(define (taylor-zero-precondition pre cover)
  (define var (taylor-zero-cover-var cover))
  (define radius (taylor-zero-cover-radius cover))
  `(and ,pre (or (< ,var ,(- radius)) (< ,radius ,var))))

(define (taylor-zero-lower-radius-value cover)
  (define var-repr (taylor-zero-cover-var-repr cover))
  (define radius-value (taylor-zero-cover-radius-value cover))
  ((representation-ordinal->repr var-repr)
   (sub1 ((representation-repr->ordinal var-repr) (- radius-value)))))

(define (wrap-taylor-zero-alt altn cover)
  (define var (taylor-zero-cover-var cover))
  (define var-repr (taylor-zero-cover-var-repr cover))
  (define out-repr (taylor-zero-cover-out-repr cover))
  (define radius (taylor-zero-cover-radius cover))
  (define radius-value (taylor-zero-cover-radius-value cover))
  (define lower-radius-value (taylor-zero-lower-radius-value cover))
  (define radius-lit (literal radius (representation-name var-repr)))
  (define fabs-impl (get-fpcore-impl 'fabs (repr->prop var-repr) (list var-repr)))
  (define <=-impl (get-fpcore-impl '<= '() (list var-repr var-repr)))
  (define if-impl (get-fpcore-impl 'if '() (list <bool> out-repr out-repr)))
  (define fabs-expr `(,fabs-impl ,var))
  (define cond-expr `(,<=-impl ,fabs-expr ,radius-lit))
  (define taylor-expr (taylor-zero-cover-taylor-expr cover))
  (define taylor-alt
    (alt taylor-expr
         `(taylor ,(alt-expr altn) zero ,var ,(taylor-zero-cover-kept-exponent cover))
         (list altn)))
  (define expr `(,if-impl ,cond-expr ,taylor-expr ,(alt-expr altn)))
  (alt expr
       `(regimes ,(list (sp 0 var lower-radius-value) (sp 1 var radius-value) (sp 0 var +nan.0)))
       (list altn taylor-alt)))

(define (wrap-taylor-zero-alts altns cover)
  (if cover
      (map (curryr wrap-taylor-zero-alt cover) altns)
      altns))
