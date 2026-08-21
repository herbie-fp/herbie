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
         taylor-cover?)

(struct taylor-cover (name var bound expr)
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
  (define v ((block-copy-only! constant-block block) (car term)))
  (define free-vars (block-free-vars constant-block))
  (cond
    [(set-empty? (free-vars v))
     (define compiler (make-real-compiler constant-block (list v) (list out-repr)))
     (define-values (status outs) (real-apply compiler (vector)))
     (define num (and (equal? status 'valid) (repr->real (first outs) out-repr)))
     (and num (rational? num) (not (zero? num)) (cons num (cdr term)))]
    [else #f]))

(define (cover-lowerable? name repr)
  (and (get-fpcore-impl 'if '() (list <bool> repr repr))
       (get-fpcore-impl '<= '() (list repr repr))
       ;; fabs is not used for -inf/+inf covers.
       (or (not (equal? name 0)) (get-fpcore-impl 'fabs (repr->prop repr) (list repr)))))

(define (build-covers block spec-v var ctx)
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
                [transform (in-list taylor-transforms)]
                #:when (cover-lowerable? (first transform) out-repr))
            (match-define (list name forward inverse) transform)
            (define tform (cons forward inverse))
            (define next-term (taylor-terms series taylor-block var #:transform tform))
            (define kept (next-term))
            (define dropped (next-term))
            (when (and kept dropped)
              (define kept-term (evaluate-term taylor-block kept out-repr))
              (define dropped-term (evaluate-term taylor-block dropped out-repr))
              (when (and kept-term dropped-term)
                (define radius (cover-radius kept-term dropped-term out-repr))
                (define bound
                  (if (equal? name 0)
                      radius
                      (/ 1 radius)))
                (define cover-expr (block->expr (horner-form (list kept-term) var #:transform tform)))
                (sow (taylor-cover name var bound cover-expr))))))))

(define (compute-taylor-covers block spec-v ctx)
  (match ctx
    ;; For now, covers only apply to univariate, scalar functions with the using only one repr.
    [(context (list var) repr (list var-repr))
     #:when (and (equal? var-repr repr) (not (array-representation? repr)))
     (build-covers block spec-v var ctx)]
    [_ '()]))

(define (cover-condition cover)
  (match-define (taylor-cover name var bound _) cover)
  (match name
    [0 `(<= (fabs ,var) ,bound)]
    ['inf `(<= ,bound ,var)]
    ['-inf `(<= ,var ,(- bound))]))

(define (covers-constraint covers)
  (match (for/list ([cover (in-list covers)])
           `(not ,(cover-condition cover)))
    [(list outside) outside]
    [outsides `(and ,@outsides)]))

(define (cover-wrap cover expression ctx)
  (match-define (taylor-cover _ _ _ arm) cover)
  (define repr (context-repr ctx))
  (define if-impl (get-fpcore-impl 'if '() (list <bool> repr repr)))
  `(,if-impl ,(spec->prog (cover-condition cover) ctx) ,(spec->prog arm ctx) ,expression))
