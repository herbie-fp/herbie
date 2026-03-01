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
         "../core/arrays.rkt"
         "../utils/errors.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt")

(define (repr->discretization repr)
  (define type
    (match (representation-name repr)
      ['bool 0]
      ['binary32 1]
      ['binary64 2]
      [_ (error "Unknown representation for rival:" (representation-name repr))]))
  (define ulps (repr-ulps repr))
  (discretization (representation-total-bits repr)
                  (representation-bf->repr repr)
                  (lambda (x y) (- (ulps x y) 1))
                  type))

(provide (struct-out real-compiler)
         ival
         ival?
         ival-lo
         ival-hi
         (contract-out
          [make-real-compiler
           (->i
            ([batch batch?]
             [brfs (listof batchref?)]
             [ctxs (brfs) (and/c unified-contexts? (lambda (ctxs) (= (length brfs) (length ctxs))))])
            (#:pre [pre any/c])
            [c real-compiler?])]
          [real-apply
           (->* (real-compiler? vector?) ((or/c rival-hints? boolean?)) (values symbol? any/c))]
          [real-compiler-clear! (-> real-compiler? void?)]
          [real-compiler-analyze
           (->* (real-compiler? (vectorof ival?)) ((or/c rival-hints? boolean?)) (listof any/c))]))

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

;; Herbie's wrapper around the Rival machine abstraction.
(struct real-compiler
        (pre vars var-reprs exprs reprs machine dump-file assemble-point assemble-output))

;; Creates a Rival machine.
(define (make-real-compiler batch brfs ctxs #:pre [pre '(TRUE)])
  (define specs (map (batch-exprs batch) brfs))
  (define-values (specs* ctxs* pre* assemble-point assemble-output reprs)
    (flatten-arrays-for-rival specs ctxs pre))
  (define vars (context-vars (first ctxs*)))

  ; create the machine
  (define exprs (cons `(assert ,pre*) specs*))
  (define discs (cons boolean-discretization (map repr->discretization reprs)))
  (define machine (rival-compile exprs vars discs))
  (timeline-push! 'compiler
                  (apply + 1 (expr-size pre*) (map expr-size specs*))
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
                        ,@specs*)
                     dump-file
                     1)
       (flush-output dump-file)
       dump-file]
      [else #f]))

  ; wrap it with useful information for Herbie
  (real-compiler pre
                 (list->vector vars)
                 (list->vector (context-var-reprs (first ctxs*)))
                 specs*
                 (list->vector reprs)
                 machine
                 dump-file
                 assemble-point
                 assemble-output))

(define (bigfloat->readable-string x)
  (define real (bigfloat->real x)) ; Exact rational unless inf/nan
  (define float (real->double-flonum real))
  (if (= real float)
      (format "#i~a" float) ; The #i explicitly means nearest float
      (number->string real))) ; Backup is print as rational

;; Runs a Rival machine on an input point.
(define (real-apply compiler pt [hint #f])
  (match-define (real-compiler _ vars var-reprs _ _ machine dump-file _ _) compiler)
  (define start (current-inexact-milliseconds))
  (define pt*
    (for/vector #:length (vector-length vars)
                ([val (in-vector pt)]
                 [repr (in-vector var-reprs)])
      ((representation-repr->bf repr) val)))
  (when dump-file
    (define args (map bigfloat->readable-string (vector->list pt*)))
    (fprintf dump-file "(eval f ~a)\n" (string-join args " "))
    (flush-output dump-file))
  (define-values (status value bumps iterations aggregated-profile)
    (with-handlers ([exn:rival:invalid?
                     (lambda (e) (values 'invalid #f 0 0 (exn:rival:invalid-aggregated-profile e)))]
                    [exn:rival:unsamplable?
                     (lambda (e) (values 'exit #f 0 0 (exn:rival:unsamplable-aggregated-profile e)))])
      (parameterize ([*rival-max-precision* (*max-mpfr-prec*)]
                     [*rival-max-iterations* 5])
        (define-values (result bumps iterations aggregated-profile) (rival-apply machine pt* hint))
        (values 'valid
                (rest (vector->list result))
                bumps
                iterations
                aggregated-profile)))) ; rest = drop precondition
  (when (> bumps 0)
    (warn 'ground-truth
          "Could not converge on a ground truth"
          #:extra (for/list ([var (in-vector vars)]
                             [val (in-vector pt)])
                    (format "~a = ~a" var val))))
  (for ([entry (in-vector aggregated-profile)])
    (match-define (list name precision time memory) entry)
    (timeline-push!/unsafe 'mixsample time (symbol->string name) precision memory))
  (timeline-push!/unsafe 'outcomes
                         (- (current-inexact-milliseconds) start)
                         iterations
                         (symbol->string status)
                         1)
  (values status value))

;; Clears profiling data.
(define (real-compiler-clear! compiler)
  (void))

;; Returns whether the machine is guaranteed to raise an exception
;; for the given inputs range. The result is an interval representing
;; how certain the result is: no, maybe, yes.
(define (real-compiler-analyze compiler input-ranges [hint #f])
  (rival-analyze-with-hints (real-compiler-machine compiler) input-ranges hint))

(module+ test
  (require rackunit)
  (define <b64> <binary64>)
  (define arr-repr (make-array-representation #:elem <b64> #:len 3))
  (define arr-ctx (context '(v) arr-repr (list arr-repr)))
  (define-values (specs* ctxs* pre* _assemble-pt _assemble-out reprs*)
    (flatten-arrays-for-rival (list 'v) (list arr-ctx) 'TRUE))
  (check-equal? specs* '(v_0 v_1 v_2))
  (check-equal? (map context-vars ctxs*) '((v_0 v_1 v_2)))
  (check-equal? (map context-var-reprs ctxs*) (list (list <b64> <b64> <b64>)))
  (check-equal? reprs* (list <b64> <b64> <b64>))
  (check-equal? (_assemble-out '(1 2 3)) '(#(1 2 3)))
  (check-equal? pre* 'TRUE))
