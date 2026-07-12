#lang racket

(require math/bigfloat
         (only-in fpbench interval range-table-ref condition->range-table))
(require "../syntax/batch.rkt"
         "../syntax/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/rival.rkt"
         "alternative.rkt"
         "batch-reduce.rkt"
         "programs.rkt"
         "taylor.rkt")

(provide compute-taylor-covers
         taylor-covers-precondition
         wrap-taylor-cover-alts)

(struct taylor-cover (name var var-repr out-repr boundary boundary-value expr exponent))

(define (real-representation? repr)
  (equal? (representation-type repr) 'real))

(define (unary-real-variable ctx)
  (match (context-vars ctx)
    [(list var) (and (real-representation? (context-lookup ctx var)) var)]
    [_ #f]))

(define (precision-epsilon repr)
  (match (representation-name repr)
    ['binary64 (expt 2 -53)]
    ['binary32 (expt 2 -24)]
    [_ #f]))

(define (finite-real? x)
  (and (real? x) (not (nan? x)) (not (infinite? x))))

(define (contains-zero? iv)
  (match-define (interval lo hi lo? hi?) iv)
  (and (or (< lo 0) (and (= lo 0) lo?)) (or (< 0 hi) (and (= hi 0) hi?))))

(define (zero-cap intervals)
  (for/first ([iv (in-list intervals)]
              #:when (contains-zero? iv))
    (match-define (interval lo hi _ _) iv)
    (min (abs lo) (abs hi))))

(define (tail-present? intervals name threshold)
  (for/or ([iv (in-list intervals)])
    (match-define (interval lo hi lo? hi?) iv)
    (match name
      ['inf (or (< threshold hi) (and (= threshold hi) hi?))]
      ['-inf (or (< lo (- threshold)) (and (= lo (- threshold)) lo?))])))

(define (round-radius-down radius cap repr)
  (define radius-value
    (parameterize ([bf-rounding-mode 'down])
      ((representation-bf->repr repr) (bf (min radius cap)))))
  (define cap-value
    (and (finite-real? cap)
         (>= radius cap)
         (>= (repr->real radius-value repr) cap)
         ((representation-ordinal->repr repr)
          (sub1 ((representation-repr->ordinal repr) radius-value)))))
  (define value (or cap-value radius-value))
  (and (finite-real? value) (positive? value) value))

(define (round-threshold-up radius-value repr)
  (define radius ((representation-repr->bf repr) radius-value))
  (define value
    (parameterize ([bf-rounding-mode 'up])
      ((representation-bf->repr repr) (bf/ 1.bf radius))))
  (and (finite-real? value) (positive? value) value))

(define (constant-value expr repr)
  (define ctx (context '() repr '()))
  (define-values (batch brfs) (progs->batch (list expr) #:ctx ctx))
  (define compiler (make-real-compiler batch brfs (list repr)))
  (define-values (status values) (real-apply compiler (vector)))
  (and (equal? status 'valid) (repr->real (first values) repr)))

(define (coefficient-value batch coeff repr)
  (define expr ((batch-exprs batch) coeff))
  (and (null? (free-variables expr))
       (let ([value (constant-value expr repr)]) (and value (finite-real? value) value))))

(define (taylor-term->spec coeff exponent var)
  (define monomial
    (match exponent
      [0 1]
      [1 var]
      [-1 `(/ 1 ,var)]
      [_ `(pow ,var ,exponent)]))
  (match* (coeff exponent)
    [(_ 0) coeff]
    [(1 _) monomial]
    [(-1 _) `(neg ,monomial)]
    [(_ _) `(* ,coeff ,monomial)]))

(define (term-in-input name coeff exponent)
  (match name
    ['zero (values coeff exponent)]
    ['inf (values coeff (- exponent))]
    ['-inf
     (values (if (odd? exponent)
                 (- coeff)
                 coeff)
             (- exponent))]))

(define (build-cover batch series transform ctx var var-repr out-repr epsilon intervals)
  (match-define (list transform-name forward inverse) transform)
  (define name (if (equal? transform-name 0) 'zero transform-name))
  (define next-term
    (make-taylor-term-generator series batch var #:transform (cons forward inverse) #:iters 8))
  (define kept (next-term))
  (define omitted (next-term))
  (define kept-exponent (taylor-term-exponent kept))
  (define omitted-exponent (taylor-term-exponent omitted))
  (cond
    [(or (not (taylor-term-coeff kept))
         (not (taylor-term-coeff omitted))
         (not (exact-integer? kept-exponent))
         (not (exact-integer? omitted-exponent))
         (<= omitted-exponent kept-exponent)
         (and (equal? name 'zero) (negative? kept-exponent)))
     #f]
    [else
     (define kept-value (coefficient-value batch (taylor-term-coeff kept) out-repr))
     (define omitted-value (coefficient-value batch (taylor-term-coeff omitted) out-repr))
     (cond
       [(or (not kept-value) (zero? kept-value) (not omitted-value) (zero? omitted-value)) #f]
       [else
        (define delta (- omitted-exponent kept-exponent))
        (define radius (expt (* epsilon (/ (abs kept-value) (abs omitted-value))) (/ 1 delta)))
        (define cap
          (if (equal? name 'zero)
              (zero-cap intervals)
              +inf.0))
        (define radius-value
          (and cap
               (positive? cap)
               (finite-real? radius)
               (positive? radius)
               (round-radius-down radius cap var-repr)))
        (cond
          [(not radius-value) #f]
          [else
           (define boundary-value
             (match name
               ['zero radius-value]
               ['inf (round-threshold-up radius-value var-repr)]
               ['-inf
                (define threshold (round-threshold-up radius-value var-repr))
                (and threshold (- threshold))]))
           (cond
             [(not boundary-value) #f]
             [(and (not (equal? name 'zero))
                   (not (tail-present? intervals name (abs (repr->real boundary-value var-repr)))))
              #f]
             [else
              (define-values (input-coeff input-exponent)
                (term-in-input name kept-value kept-exponent))
              (define spec (taylor-term->spec input-coeff input-exponent var))
              (define expr (fpcore->prog (prog->fpcore spec ctx) ctx))
              (taylor-cover name
                            var
                            var-repr
                            out-repr
                            (repr->real boundary-value var-repr)
                            boundary-value
                            expr
                            kept-exponent)])])])]))

(define (compute-taylor-covers spec pre ctx)
  (define var (unary-real-variable ctx))
  (define out-repr (context-repr ctx))
  (define epsilon (and (real-representation? out-repr) (precision-epsilon out-repr)))
  (cond
    [(not (and var epsilon)) '()]
    [else
     (define var-repr (context-lookup ctx var))
     (define intervals (range-table-ref (condition->range-table pre) var))
     (define-values (batch brfs) (progs->batch (list spec) #:ctx ctx))
     (parameterize ([reduce (batch-reduce batch)]
                    [add (lambda (x) (batch-add! batch x))])
       (define coefficientss (taylor-coefficients batch brfs (list var) taylor-transforms))
       (for/list ([transform (in-list taylor-transforms)]
                  [coefficients (in-list coefficientss)]
                  #:do [(define cover
                          (build-cover batch
                                       (first coefficients)
                                       transform
                                       ctx
                                       var
                                       var-repr
                                       out-repr
                                       epsilon
                                       intervals))]
                  #:when cover)
         cover))]))

(define (outside-cover cover)
  (define var (taylor-cover-var cover))
  (define boundary (taylor-cover-boundary cover))
  (match (taylor-cover-name cover)
    ['zero `(or (< ,var ,(- boundary)) (< ,boundary ,var))]
    ['inf `(< ,var ,boundary)]
    ['-inf `(< ,boundary ,var)]))

(define (taylor-covers-precondition pre covers)
  (for/fold ([pre pre]) ([cover (in-list covers)])
    `(and ,pre ,(outside-cover cover))))

(define (predecessor value repr)
  ((representation-ordinal->repr repr) (sub1 ((representation-repr->ordinal repr) value))))

(define (wrap-cover altn cover)
  (define name (taylor-cover-name cover))
  (define var (taylor-cover-var cover))
  (define var-repr (taylor-cover-var-repr cover))
  (define out-repr (taylor-cover-out-repr cover))
  (define boundary (taylor-cover-boundary cover))
  (define boundary-value (taylor-cover-boundary-value cover))
  (define boundary-lit (literal boundary (representation-name var-repr)))
  (define <=-impl (get-fpcore-impl '<= '() (list var-repr var-repr)))
  (define if-impl (get-fpcore-impl 'if '() (list <bool> out-repr out-repr)))
  (define condition
    (match name
      ['zero
       (define fabs-impl (get-fpcore-impl 'fabs (repr->prop var-repr) (list var-repr)))
       `(,<=-impl (,fabs-impl ,var) ,boundary-lit)]
      ['inf `(,<=-impl ,boundary-lit ,var)]
      ['-inf `(,<=-impl ,var ,boundary-lit)]))
  (define taylor-expr (taylor-cover-expr cover))
  (define taylor-alt
    (alt taylor-expr
         `(taylor ,(alt-expr altn) ,name ,var ,(taylor-cover-exponent cover))
         (list altn)))
  (define splitpoints
    (match name
      ['zero
       (list (sp 0 var (predecessor (- boundary-value) var-repr))
             (sp 1 var boundary-value)
             (sp 0 var +nan.0))]
      ['inf (list (sp 0 var (predecessor boundary-value var-repr)) (sp 1 var +nan.0))]
      ['-inf (list (sp 1 var boundary-value) (sp 0 var +nan.0))]))
  (alt `(,if-impl ,condition ,taylor-expr ,(alt-expr altn))
       `(regimes ,splitpoints)
       (list altn taylor-alt)))

(define (wrap-taylor-cover-alts altns covers)
  (for/fold ([altns altns]) ([cover (in-list covers)])
    (for/list ([altn (in-list altns)])
      (wrap-cover altn cover))))
