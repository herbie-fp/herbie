;; A narrow shim for Rival's "machine" abstraction.
;; A Rival "machine" performs real evaluation for multiple expressions on a point.
;;
;; Ensure this file has minimal dependencies since `<herbie>/syntax/syntax.rkt`
;; requires the file to synthesize floating-point implementations!
;;

#lang racket

(require rival)

(require "../config.rkt"
         "../syntax/types.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt")

(provide (struct-out real-evaluator)
         make-real-evaluator
         run-real-evaluator
         real-evaluator-clear!
         real-evaluator-unsamplable?)

(define (expr-size expr)
  (if (list? expr)
      (apply + 1 (map expr-size (cdr expr)))
      1))

(define (repr->discretization repr)
  (discretization
    (representation-total-bits repr)
    (representation-bf->repr repr)
    (lambda (x y) (- (ulp-difference x y repr) 1))))

;; Herbie's wrapper around the Rival machine abstraction.
(struct real-evaluator (pre vars var-reprs exprs reprs machine))

;; Creates a Rival machine.
;; Requires a context for the input variables and their representations,
;; and an association list of expressions and their output representations.
;; Optionally, takes a precondition.
(define (make-real-evaluator ctx specs&reprs #:pre [pre '(TRUE)])
  (define vars (context-vars ctx))
  (define var-reprs (context-var-reprs ctx))
  (define specs (map car specs&reprs))
  (define reprs (map cdr specs&reprs))
  ; create the machine
  (define exprs (cons `(assert ,pre) specs))
  (define discs (cons boolean-discretization (map repr->discretization reprs)))
  (define machine (rival-compile exprs vars discs))
  (timeline-push! 'compiler
                  (apply + 1 (expr-size pre) (map expr-size specs))
                  (+ (length vars) (rival-profile machine 'instructions)))
  ; wrap it with useful information for Herbie
  (real-evaluator pre vars var-reprs specs reprs machine))

;; Runs a Rival machine on an input point.
(define (run-real-evaluator evaluator pt)
  (match-define (real-evaluator _ vars var-reprs _ _ machine) evaluator)
  (define start (current-inexact-milliseconds))
  (define pt*
    (for/vector #:length (length vars)
                ([val (in-list pt)] [repr (in-list var-reprs)])
      ((representation-repr->bf repr) val)))
  (define-values (status value)
    (with-handlers
      ([exn:rival:invalid? (lambda (e) (values 'invalid #f))]
       [exn:rival:unsamplable? (lambda (e) (values 'exit #f))])
      (parameterize ([*rival-max-precision* (*max-mpfr-prec*)]
                     [*rival-max-iterations* 5])
        (values 'valid (rest (vector->list (rival-apply machine pt*))))))) ; rest = drop precondition
  (when (> (rival-profile machine 'bumps) 0)
    (warn 'ground-truth "Could not converge on a ground truth"
          #:extra (for/list ([var (in-list vars)] [val (in-list pt)])
                    (format "~a = ~a" var val))))
  (define executions (rival-profile machine 'executions))
  (when (>= (vector-length executions) (*rival-profile-executions*))
    (warn 'profile "Rival profile vector overflowed, profile may not be complete"))
  (define prec-threshold (exact-floor (/ (*max-mpfr-prec*) 25)))
  (for ([execution (in-vector executions)])
    (define name (symbol->string (execution-name execution)))
    (define precision (- (execution-precision execution)
                         (remainder (execution-precision execution) prec-threshold)))
    (timeline-push!/unsafe 'mixsample (execution-time execution) name precision))
  (timeline-push!/unsafe 'outcomes (- (current-inexact-milliseconds) start)
                         (rival-profile machine 'iterations) (~a status) 1)
  (values status value))

;; Clears profiling data.
(define (real-evaluator-clear! evaluator)
  (rival-profile (real-evaluator-machine evaluator) 'executions)
  (void))

;; Returns whether the machine is guaranteed to raise an exception
;; for the given inputs range. The result is an interval representing
;; how certain the result is: no, maybe, yes.
(define (real-evaluator-unsamplable? evaluator input-ranges)
  (rival-analyze (real-evaluator-machine evaluator) input-ranges))