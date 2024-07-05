;; A narrow shim for Rival.
;; The main abstraction is the Rival "machine" which performs
;; real evaluation for multiple expressions on a point.
;;
;; Ensure this file has minimal dependencies since `<herbie>/syntax/syntax.rkt`
;; requires the file to synthesize floating-point implementations!
;;

#lang racket

(require rival)

(require "../syntax/types.rkt"
         "../config.rkt"
         "../errors.rkt"
         "../float.rkt"
         "../timeline.rkt")

(provide real-evaluator?
         real-evaluator-ctx
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
   (representation-bf->repr repr)
   (lambda (x y) (- (ulp-difference x y repr) 1))))

;; Herbie's wrapper around the Rival machine abstraction.
(struct real-evaluator (pre specs ctx machine))

;; Creates a Rival machine from a list of specifications and a context.
;; A precondition can optionally be provided.
(define (make-real-evaluator specs ctx #:pre [pre '(TRUE)])
  (match-define (context vars repr _) ctx)
  (define exprs (cons `(assert ,pre) specs))
  (define discs (cons boolean-discretization (map (const (repr->discretization repr)) specs)))
  (define machine (rival-compile exprs vars discs))
  (timeline-push! 'compiler
                  (apply + 1 (expr-size pre) (map expr-size specs))
                  (+ (length vars) (rival-profile machine 'instructions)))
  (real-evaluator pre specs ctx machine))

;; Runs a Rival machine on an input point.
(define (run-real-evaluator evaluator pt)
  (match-define (real-evaluator _ _ ctx machine) evaluator)
  (define start (current-inexact-milliseconds))
  (define pt*
    (for/vector ([val (in-list pt)] [repr (in-list (context-var-reprs ctx))])
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
          #:extra (for/list ([var (in-list (context-vars ctx))] [val (in-list pt)])
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
