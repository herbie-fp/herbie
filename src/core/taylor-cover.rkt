#lang racket

(require math/flonum)
(require "../syntax/block.rkt"
         "../syntax/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/rival.rkt"
         "../utils/timeline.rkt"
         "../utils/common.rkt"
         "alternative.rkt"
         "points.rkt"
         "reduce.rkt"
         "taylor.rkt")

(provide compute-taylor-covers
         taylor-covers-precondition
         wrap-taylor-cover-alts)

(struct taylor-cover (name var var-repr out-repr lo hi expr exponent))

(define (precision-epsilon repr)
  (- 1 (repr->real (predecessor (real->repr 1 repr) repr) repr)))

(define (predecessor value repr)
  ((representation-ordinal->repr repr) (sub1 ((representation-repr->ordinal repr) value))))

(define (cover-radius kept dropped out-repr)
  (match-define (cons kept-coeff kept-exponent) kept)
  (match-define (cons dropped-coeff dropped-exponent) dropped)
  (expt (* (precision-epsilon out-repr) (/ (abs kept-coeff) (abs dropped-coeff)))
        (/ 1 (- dropped-exponent kept-exponent))))

(define (cover-interval name radius var-repr)
  (define bound
    (real->repr (if (equal? name 0)
                    radius
                    (/ 1 radius))
                var-repr))
  (match name
    [0 (cons (- bound) bound)]
    ['inf (cons bound +inf.0)]
    ['-inf (cons -inf.0 (- bound))]))

(define (evaluate-term block term out-repr)
  (define constant-block (block-empty (context '() #f '())))
  (define v (and term ((block-copy-only! constant-block block) (car term))))
  (define free-vars (block-free-vars constant-block))
  ;; Ensure both coefficients are pure constants before evaluating.
  (cond
    [(and v (set-empty? (free-vars v)))
     (define compiler (make-real-compiler constant-block (list v) (list out-repr)))
     (define-values (status outs) (real-apply compiler (vector)))
     (define num (and (equal? status 'valid) (repr->real (first outs) out-repr)))
     (and num (rational? num) (not (zero? num)) (cons num (cdr term)))]
    [else #f]))

(define (build-covers spec var var-repr ctx)
  (define out-repr (context-repr ctx))
  (define-values (block vs) (progs->block (list spec) #:ctx ctx))
  (reap [sow]
        (parameterize ([reduce (block-reduce block)]
                       [add (lambda (x) (block-add! block x))])
          (define block->expr (block-exprs block))
          (define all-series (map first (taylor-coefficients block vs (list var) taylor-transforms)))
          (for ([series (in-list all-series)]
                [transform (in-list taylor-transforms)])
            (match-define (list name forward inverse) transform)
            (define tform (cons forward inverse))
            (define next-term (taylor-terms series block var #:transform tform))
            ;; Evaluate term coefficients.
            (define kept-term (evaluate-term block (next-term) out-repr))
            (define dropped-term (evaluate-term block (next-term) out-repr))
            (when (and kept-term dropped-term)
              (define radius (cover-radius kept-term dropped-term out-repr))
              (match-define (cons lo hi) (cover-interval name radius var-repr))
              (define cover-expr
                (fpcore->prog (block->expr (horner-form (list kept-term) var #:transform tform)) ctx))
              (sow (taylor-cover name var var-repr out-repr lo hi cover-expr (cdr kept-term))))))))

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

(define (compute-taylor-covers spec expr pcontext ctx)
  (match ctx
    [(context (list var) _ (list var-repr)) ; For now, covers only apply to univariate functions.
     (timeline-event! 'series)
     (define candidates (build-covers spec var var-repr ctx))
     ;; Keep the covers that improve over the original train-pcontext; if any do,
     ;; sandbox.rkt samples a new train-pcontext with their regions excluded.
     (define covers (filter (curryr cover-improves? expr pcontext ctx) candidates))
     (for ([cover (in-list candidates)])
       (timeline-push! 'taylor-count
                       (~a (taylor-cover-name cover))
                       (taylor-cover-exponent cover)
                       1
                       1
                       (if (memq cover covers) 1 0)))
     covers]
    [_ '()]))

(define (cover-condition cover)
  (match-define (taylor-cover _ var var-repr _ lo hi _ _) cover)
  (define <=-impl (get-fpcore-impl '<= '() (list var-repr var-repr)))
  (define (bound value)
    (literal (repr->real value var-repr) (representation-name var-repr)))
  (cond
    [(infinite? lo) `(,<=-impl ,var ,(bound hi))]
    [(infinite? hi) `(,<=-impl ,(bound lo) ,var)]
    [else
     (define fabs-impl (get-fpcore-impl 'fabs (repr->prop var-repr) (list var-repr)))
     `(,<=-impl (,fabs-impl ,var) ,(bound hi))]))

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

;; Skip whatever the covers already handle.
(define (taylor-covers-precondition pre covers)
  (for/fold ([pre pre]) ([cover (in-list covers)])
    `(and ,pre (not ,(prog->spec (cover-condition cover))))))

;; Report the covered and uncovered forms of every alternative.
(define (wrap-taylor-cover-alts altns covers)
  (cond
    [(null? covers) altns]
    [else
     (append altns
             (for/list ([altn (in-list altns)])
               (for/fold ([altn altn]) ([cover (in-list covers)])
                 (wrap-cover altn cover))))]))
