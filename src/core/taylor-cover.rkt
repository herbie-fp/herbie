#lang racket

(require "../syntax/block.rkt"
         "../syntax/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "../syntax/rival.rkt"
         "../utils/common.rkt"
         "reduce.rkt"
         "taylor.rkt")

(provide compute-taylor-covers
         covers-constraint
         cover-wrap
         taylor-cover?
         taylor-cover-name
         taylor-cover-var)

(struct taylor-cover (name var var-repr out-repr bound expr)
  #:methods gen:custom-write
  [(define (write-proc cover port mode)
     (fprintf port "(cover ~a ~a)" (taylor-cover-name cover) (taylor-cover-var cover)))])

(define (precision-epsilon repr)
  (- 1 (repr->real (predecessor (real->repr 1 repr) repr) repr)))

(define (predecessor value repr)
  ((representation-ordinal->repr repr) (sub1 ((representation-repr->ordinal repr) value))))

(define (cover-radius kept dropped out-repr)
  (match-define (cons kept-coeff kept-exponent) kept)
  (match-define (cons dropped-coeff dropped-exponent) dropped)
  (expt (* (precision-epsilon out-repr) (/ (abs kept-coeff) (abs dropped-coeff)))
        (/ 1 (- dropped-exponent kept-exponent))))

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

(define (build-covers block spec-v var var-repr ctx)
  (define out-repr (context-repr ctx))
  (define taylor-block (block-empty ctx))
  (define v ((block-copy-only! taylor-block block) spec-v))
  (reap [sow]
        (parameterize ([reduce (block-reduce taylor-block)]
                       [add (lambda (x) (block-add! taylor-block x))])
          (define block->expr (block-exprs taylor-block))
          (define all-series
            (map first (taylor-coefficients taylor-block (list v) (list var) taylor-transforms)))
          (for ([series (in-list all-series)]
                [transform (in-list taylor-transforms)])
            (match-define (list name forward inverse) transform)
            (define tform (cons forward inverse))
            (define next-term (taylor-terms series taylor-block var #:transform tform))
            ;; Evaluate term coefficients.
            (define kept-term (evaluate-term taylor-block (next-term) out-repr))
            (define dropped-term (evaluate-term taylor-block (next-term) out-repr))
            (when (and kept-term dropped-term)
              (define radius (cover-radius kept-term dropped-term out-repr))
              (define bound
                (if (equal? name 0)
                    radius
                    (/ 1 radius)))
              (define cover-expr (block->expr (horner-form (list kept-term) var #:transform tform)))
              (sow (taylor-cover name var var-repr out-repr bound cover-expr)))))))

(define (compute-taylor-covers block spec-v ctx)
  (match ctx
    ;; For now, covers only apply to univariate functions.
    [(context (list var) _ (list var-repr)) (build-covers block spec-v var var-repr ctx)]
    [_ '()]))

(define (cover-condition cover)
  (match-define (taylor-cover name var _ _ bound _) cover)
  (match name
    [0 `(<= (fabs ,var) ,bound)]
    ['inf `(<= ,bound ,var)]
    ['-inf `(<= ,var ,(- bound))]))

(define (covers-constraint covers)
  (define outsides
    (for/list ([cover (in-list covers)])
      `(not ,(cover-condition cover))))
  (foldl (lambda (outside constraint) `(and ,constraint ,outside)) (first outsides) (rest outsides)))

(define (cover-wrap cover expression ctx)
  (match-define (taylor-cover _ var var-repr out-repr _ arm) cover)
  (define if-impl (get-fpcore-impl 'if '() (list <bool> out-repr out-repr)))
  (define condition
    (spec->prog (cover-condition cover) (context (list var) var-repr (list var-repr))))
  `(,if-impl ,condition ,(spec->prog arm ctx) ,expression))
