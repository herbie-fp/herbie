;; A narrow shim for Rival's "machine" abstraction.
;; A Rival "machine" performs real evaluation for multiple expressions on a point.
;;
;; Ensure this file has minimal dependencies since `<herbie>/syntax/syntax.rkt`
;; requires the file to synthesize floating-point implementations!
;;

#lang racket

(require math/bigfloat
         (prefix-in r2: rival)
         (prefix-in r3: rival3-racket))

(require "../config.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt")

(define (use-rival3?)
  (flag-set? 'sampling 'rival3))

(define-syntax-rule (define/rival (name args ...) r2-impl r3-impl)
  (define (name args ...)
    (if (use-rival3?)
        (r3-impl args ...)
        (r2-impl args ...))))

(define/rival (rival-compile exprs vars discs) r2:rival-compile r3:rival-compile)
(define/rival (rival-apply machine pt hint) r2:rival-apply r3:rival-apply)
(define/rival (rival-analyze-with-hints machine rect hint)
              r2:rival-analyze-with-hints
              r3:rival-analyze-with-hints)
(define/rival (rival-profile machine param) r2:rival-profile r3:rival-profile)

(define (boolean-discretization)
  (if (use-rival3?) r3:boolean-discretization r2:boolean-discretization))

(define (repr->rival3-disc-type repr)
  (cond
    [(eq? repr <binary32>) 'f32]
    [(eq? repr <binary64>) 'f64]
    [else 'f64]))

(define (rival3-shared-target reprs)
  (apply max (map representation-total-bits reprs)))

(define (repr->rival2-discretization repr)
  (r2:discretization (representation-total-bits repr)
                     (representation-bf->repr repr)
                     (lambda (x y) (- (ulp-difference x y repr) 1))))

(define (repr->rival3-discretization repr target)
  (r3:discretization (repr->rival3-disc-type repr) target (representation-bf->repr repr)))

(define (exn:rival:invalid? e)
  (or (r2:exn:rival:invalid? e) (r3:exn:rival:invalid? e)))

(define (exn:rival:unsamplable? e)
  (or (r2:exn:rival:unsamplable? e) (r3:exn:rival:unsamplable? e)))

(define *rival-max-precision*
  (make-derived-parameter r2:*rival-max-precision*
                          identity
                          (lambda (v)
                            (r3:*rival-max-precision* v)
                            v)))

(define *rival-max-iterations*
  (make-derived-parameter r2:*rival-max-iterations*
                          identity
                          (lambda (v)
                            (r3:*rival-max-iterations* v)
                            v)))

(define (*rival-profile-executions*)
  (if (use-rival3?)
      (r3:*rival-profile-executions*)
      (r2:*rival-profile-executions*)))

(define/rival (execution-name exec) r2:execution-name r3:execution-name)
(define/rival (execution-precision exec) r2:execution-precision r3:execution-precision)
(define/rival (execution-time exec) r2:execution-time r3:execution-time)
(define/rival (execution-memory exec) r2:execution-memory r3:execution-memory)

(struct herbie-ival (lo hi) #:transparent)

(define (ival? x)
  (or (herbie-ival? x) (r2:ival? x) (r3:ival? x)))

(define (ival lo hi)
  (herbie-ival lo hi))

(define (ival-lo iv)
  (cond
    [(herbie-ival? iv) (herbie-ival-lo iv)]
    [(r3:ival? iv) (r3:ival-lo iv)]
    [else (r2:ival-lo iv)]))

(define (ival-hi iv)
  (cond
    [(herbie-ival? iv) (herbie-ival-hi iv)]
    [(r3:ival? iv) (r3:ival-hi iv)]
    [else (r2:ival-hi iv)]))

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
          [real-apply (->* (real-compiler? vector?) (any/c) (values symbol? any/c))]
          [real-compiler-clear! (-> real-compiler? void?)]
          [real-compiler-analyze (->* (real-compiler? (vectorof ival?)) (any/c) (listof any/c))]))

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
(struct real-compiler (pre vars var-reprs exprs reprs machine dump-file))

;; Creates a Rival machine.
;; Takes a batch, a list of batchrefs into the batch, and a context
;; to encode input variables and their representations.
;; Optionally, takes a precondition.
(define (make-real-compiler batch brfs ctxs #:pre [pre '(TRUE)])
  (define vars (context-vars (first ctxs)))
  (define reprs (map context-repr ctxs))
  ;; Convert batchrefs to expressions. This conversion is not slow because
  ;; Rival internally uses a hasheq-based deduplication optimization.
  (define specs (map (batch-exprs batch) brfs))
  ; create the machine
  (define exprs (cons `(assert ,pre) specs))
  (define discs
    (if (use-rival3?)
        (let* ([target (rival3-shared-target reprs)]
               [bool-disc (struct-copy r3:discretization r3:boolean-discretization [target target])])
          (cons bool-disc (map (lambda (repr) (repr->rival3-discretization repr target)) reprs)))
        (cons (boolean-discretization) (map repr->rival2-discretization reprs))))
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
       (flush-output dump-file)
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

(define (execution-name->string name)
  (if (string? name)
      name
      (symbol->string name)))

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
    (fprintf dump-file "(eval f ~a)\n" (string-join args " "))
    (flush-output dump-file))
  (define-values (status value)
    (with-handlers ([exn:rival:invalid? (lambda (e) (values 'invalid #f))]
                    [exn:rival:unsamplable? (lambda (e) (values 'exit #f))])
      (parameterize ([*rival-max-precision* (*max-mpfr-prec*)]
                     [*rival-max-iterations* 5])
        (define result (rival-apply machine pt* hint))
        (define value
          (for/list ([i (in-range 1 (vector-length result))])
            (vector-ref result i)))
        (values 'valid value))))
  (when (> (rival-profile machine 'bumps) 0)
    (warn 'ground-truth
          "Could not converge on a ground truth"
          #:extra (for/list ([var (in-vector vars)]
                             [val (in-vector pt)])
                    (format "~a = ~a" var val))))
  (define-values (iterations mixsample-data)
    (if (use-rival3?)
        (match-let ([(list summary _ iters) (rival-profile machine 'summary)])
          (values iters
                  (for/list ([entry (in-vector summary)])
                    (match-define (list name prec-bucket total-time _) entry)
                    (list total-time name prec-bucket 0))))
        (let ()
          (define executions (rival-profile machine 'executions))
          (when (>= (vector-length executions) (*rival-profile-executions*))
            (warn 'profile "Rival profile vector overflowed, profile may not be complete"))
          (define prec-threshold (exact-floor (/ (*max-mpfr-prec*) 25)))
          (define mixsample-table (make-hash))
          (for ([execution (in-vector executions)])
            (define name (execution-name->string (execution-name execution)))
            (define precision
              (- (execution-precision execution)
                 (remainder (execution-precision execution) prec-threshold)))
            (define key (cons name precision))
            ;; Uses vectors to avoid allocation; this is really allocation-heavy
            (define data (hash-ref! mixsample-table key (lambda () (make-vector 2 0))))
            (vector-set! data 0 (+ (vector-ref data 0) (execution-time execution)))
            (vector-set! data 1 (+ (vector-ref data 1) (execution-memory execution))))
          (values (rival-profile machine 'iterations)
                  (for/list ([(key val) (in-hash mixsample-table)])
                    (list (vector-ref val 0) (car key) (cdr key) (vector-ref val 1)))))))
  (for ([entry (in-list mixsample-data)])
    (match-define (list time name prec memory) entry)
    (timeline-push!/unsafe 'mixsample time name prec memory))
  (timeline-push!/unsafe 'outcomes
                         (- (current-inexact-milliseconds) start)
                         iterations
                         (symbol->string status)
                         1)
  (values status value))

;; Clears profiling data.
(define (real-compiler-clear! compiler)
  (unless (use-rival3?)
    (r2:rival-profile (real-compiler-machine compiler) 'executions))
  (void))

;; Returns whether the machine is guaranteed to raise an exception
;; for the given inputs range. The result is an interval representing
;; how certain the result is: no, maybe, yes.
(define (real-compiler-analyze compiler input-ranges [hint #f])
  (define rect*
    (for/vector #:length (vector-length input-ranges)
                ([iv (in-vector input-ranges)])
      (if (use-rival3?)
          (r3:ival (ival-lo iv) (ival-hi iv))
          (r2:ival (ival-lo iv) (ival-hi iv)))))
  (rival-analyze-with-hints (real-compiler-machine compiler) rect* hint))
