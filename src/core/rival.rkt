;; A narrow shim for Rival's "machine" abstraction.
;; A Rival "machine" performs real evaluation for multiple expressions on a point.
;;
;; Ensure this file has minimal dependencies since `<herbie>/syntax/syntax.rkt`
;; requires the file to synthesize floating-point implementations!
;;

#lang racket

(require math/bigfloat
         rival-herbie)

(require "../config.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt")

(provide (struct-out real-compiler)
         (contract-out
          [make-real-compiler
           (->i ([es (listof any/c)]
                 [ctxs (es) (and/c unified-contexts? (lambda (ctxs) (= (length es) (length ctxs))))])
                (#:pre [pre any/c])
                [c real-compiler?])]
          [real-apply
           (->* (real-compiler? vector?) ((or/c (vectorof any/c) boolean?)) (values symbol? any/c))]
          [real-compiler-clear! (-> real-compiler-clear! void?)]
          [real-compiler-analyze
           (->* (real-compiler? (vectorof ival?))
                ((or/c (vectorof any/c) boolean?))
                (listof any/c))]))

(define (unified-contexts? ctxs)
  (cond
    [((non-empty-listof context?) ctxs)
     (define ctx0 (car ctxs))
     (for/and ([ctx (in-list (cdr ctxs))])
       (and (equal? (context-vars ctx0) (context-vars ctx))
            (for/and ([var (in-list (context-vars ctx0))])
              (equal? (context-lookup ctx0 var) (context-lookup ctx var)))))]
    [else #f]))

(define (expr-size expr)
  (if (list? expr)
      (apply + 1 (map expr-size (cdr expr)))
      1))

(define (repr->discretization repr)
  (define type
    (match (representation-name repr)
      ['bool 0]
      ['binary32 1]
      ['binary64 2]
      [_ (error "Unknown representation for rival:" (representation-name repr))]))
  (discretization (representation-total-bits repr)
                  (representation-bf->repr repr)
                  (lambda (x y) (- (ulp-difference x y repr) 1))
                  type))

;; Herbie's wrapper around the Rival machine abstraction.
(struct real-compiler (pre vars var-reprs exprs reprs machine dump-file))

;; Creates a Rival machine.
;; Takes a context to encode input variables and their representations,
;; a list of expressions, and a list of output representations
;; for each expression. Optionally, takes a precondition.
(define (make-real-compiler specs ctxs #:pre [pre '(TRUE)])
  (define vars (context-vars (first ctxs)))
  (define reprs (map context-repr ctxs))
  ; create the machine
  (define exprs (cons `(assert ,pre) specs))
  (define discs (cons boolean-discretization (map repr->discretization reprs)))
  (define machine (rival-compile exprs vars discs))
  (timeline-push! 'compiler
                  (apply + 1 (expr-size pre) (map expr-size specs))
                  (+ (length vars) (rival-profile machine 'instructions)))

  (define dump-file
    (cond
      [(flag-set? 'dump 'rival)
       (define dump-dir "dump-rival")
       (unless (directory-exists? dump-dir)
         (make-directory dump-dir))
       (define name
         (for/first ([i (in-naturals)]
                     #:unless (file-exists? (build-path dump-dir (format "~a.rival" i))))
           (build-path dump-dir (format "~a.rival" i))))
       (define dump-file (open-output-file name #:exists 'replace))
       (pretty-print `(define (f ,@vars)
                        ,@specs)
                     dump-file
                     1)
       dump-file]
      [else #f]))

  ; wrap it with useful information for Herbie
  (real-compiler pre
                 (list->vector vars)
                 (list->vector (context-var-reprs (first ctxs)))
                 specs
                 (list->vector reprs)
                 machine
                 dump-file))

(define (bigfloat->readable-string x)
  (define real (bigfloat->real x)) ; Exact rational unless inf/nan
  (define float (real->double-flonum real))
  (if (= real float)
      (format "#i~a" float) ; The #i explicitly means nearest float
      (number->string real))) ; Backup is print as rational

;; Runs a Rival machine on an input point.
(define (real-apply compiler pt [hint #f])
  (match-define (real-compiler _ vars var-reprs _ _ machine dump-file) compiler)
  (define start (current-inexact-milliseconds))
  (define pt*
    (for/vector #:length (vector-length vars)
                ([val (in-vector pt)]
                 [repr (in-vector var-reprs)])
      ((representation-repr->bf repr) val)))
  (when dump-file
    (define args (map bigfloat->readable-string (vector->list pt*)))
    (fprintf dump-file "(eval f ~a)\n" (string-join args " ")))
  (define-values (status value)
    (with-handlers ([exn:rival:invalid? (lambda (e) (values 'invalid #f))]
                    [exn:rival:unsamplable? (lambda (e) (values 'exit #f))])
      (parameterize ([*rival-max-precision* (*max-mpfr-prec*)]
                     [*rival-max-iterations* 5])
        (define value (rest (vector->list (rival-apply machine pt* hint)))) ; rest = drop precondition
        (values 'valid value))))
  (when (> (rival-profile machine 'bumps) 0)
    (warn 'ground-truth
          "Could not converge on a ground truth"
          #:extra (for/list ([var (in-vector vars)]
                             [val (in-vector pt)])
                    (format "~a = ~a" var val))))
  (define executions (rival-profile machine 'executions))
  (when (>= (vector-length executions) (*rival-profile-executions*))
    (warn 'profile "Rival profile vector overflowed, profile may not be complete"))
  (define prec-threshold (exact-floor (/ (*max-mpfr-prec*) 25)))
  (for ([execution (in-vector executions)])
    (define name (symbol->string (execution-name execution)))
    (define precision
      (- (execution-precision execution) (remainder (execution-precision execution) prec-threshold)))
    (timeline-push!/unsafe 'mixsample
                           (execution-time execution)
                           name
                           precision
                           (execution-memory execution)))
  (timeline-push!/unsafe 'outcomes
                         (- (current-inexact-milliseconds) start)
                         (rival-profile machine 'iterations)
                         (symbol->string status)
                         1)
  (values status value))

;; Clears profiling data.
(define (real-compiler-clear! compiler)
  (rival-profile (real-compiler-machine compiler) 'executions)
  (void))

;; Returns whether the machine is guaranteed to raise an exception
;; for the given inputs range. The result is an interval representing
;; how certain the result is: no, maybe, yes.
(define (real-compiler-analyze compiler input-ranges [hint #f])
  (rival-analyze-with-hints (real-compiler-machine compiler) input-ranges hint))