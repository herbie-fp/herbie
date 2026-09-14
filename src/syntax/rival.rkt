;; A narrow shim for Rival's "machine" abstraction.
;; A Rival "machine" performs real evaluation for multiple expressions on a point.
;;
;; Ensure this file has minimal dependencies since `<herbie>/syntax/syntax.rkt`
;; requires the file to synthesize floating-point implementations!
;;

#lang racket

(require math/bigfloat
         (prefix-in r2: rival)
         (prefix-in r3: rival3))

(require "../config.rkt"
         "../core/arrays.rkt"
         "../utils/errors.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../syntax/block.rkt")

(define (use-rival3?)
  (not (flag-set? 'setup 'rival2)))

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

(define (repr->disc-type repr)
  (cond
    [(eq? repr <bool>) 'bool]
    [(eq? repr <binary32>) 'f32]
    [(eq? repr <binary64>) 'f64]
    [else (error 'repr->disc-type "unsupported repr ~a" (representation-name repr))]))

(define (repr->disc-convert repr)
  (if (eq? repr <bool>)
      (r3:discretization-convert r3:boolean-discretization)
      (representation-bf->repr repr)))

(define (make-discretizations reprs)
  (cond
    [(use-rival3?)
     ;; Rival 3 requires that all discretizations share the target precision for now
     (define target (apply max (map representation-total-bits reprs)))
     (cons (struct-copy r3:discretization r3:boolean-discretization [target target])
           (for/list ([repr (in-list reprs)])
             (r3:discretization (repr->disc-type repr) target (repr->disc-convert repr))))]
    [else
     (cons r2:boolean-discretization
           (for/list ([repr (in-list reprs)])
             (define ulps (repr-ulps repr))
             (r2:discretization (representation-total-bits repr)
                                (representation-bf->repr repr)
                                (lambda (x y) (- (ulps x y) 1)))))]))

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
           (->i ([block block?] [vs (listof val?)]
                                [reprs
                                 (vs)
                                 (and/c (listof representation?)
                                        (lambda (reprs) (= (length vs) (length reprs))))])
                (#:pre [pre any/c])
                [c real-compiler?])]
          [real-apply (->* (real-compiler? vector?) (any/c) (values symbol? any/c))]
          [real-compiler-analyze (->* (real-compiler? (vectorof ival?)) (any/c) (listof any/c))]))

(define (expr-size expr)
  (if (list? expr)
      (apply + 1 (map expr-size (cdr expr)))
      1))

;; Herbie's wrapper around the Rival machine abstraction.
(struct real-compiler
        (pre vars var-reprs exprs reprs machine dump-file assemble-point assemble-output))

;; Creates a Rival machine.
(define (make-real-compiler block vs output-reprs #:pre [pre '(TRUE)])
  (define specs (map (block-exprs block) vs))
  (define ctxs
    (for/list ([repr (in-list output-reprs)])
      (context (block-vars block) repr (block-var-reprs block))))
  (define-values (specs* ctxs* pre* assemble-point assemble-output flattened-reprs)
    (flatten-arrays-for-rival specs ctxs pre))
  (define vars (context-vars (first ctxs*)))

  ; create the machine
  (define exprs (cons `(assert ,pre*) specs*))
  (define discs (make-discretizations flattened-reprs))
  (define machine (rival-compile exprs vars discs))
  (when (use-rival3?)
    (r3:rival-set-profiling! machine #f)) ; Herbie only reads iteration and bump counters
  (timeline-push! 'compiler
                  (apply + 1 (expr-size pre*) (map expr-size specs*))
                  (+ (length vars) (rival-profile machine 'instructions)))

  (define dump-file
    (cond
      [(flag-set? 'dump 'rival)
       (define dump-dir "dump-rival")
       (make-directory* dump-dir)
       (define dump-file
         (for/or ([i (in-naturals)])
           (with-handlers ([exn:fail:filesystem:exists? (const #f)])
             (open-output-file (build-path dump-dir (format "~a.rival" i)) #:exists 'error))))
       (pretty-print `(precision ,@(map representation-name flattened-reprs)) dump-file 1)
       (pretty-print `(define (f ,@vars)
                        ,@exprs)
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
                 (list->vector flattened-reprs)
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
  (define-values (status value)
    (with-handlers ([exn:rival:invalid? (lambda (e) (values 'invalid #f))]
                    [exn:rival:unsamplable? (lambda (e) (values 'exit #f))])
      (parameterize ([*rival-max-precision* (*max-mpfr-prec*)]
                     [*rival-max-iterations* 5])
        (define value (rest (vector->list (rival-apply machine pt* hint)))) ; rest = drop precondition
        (values 'valid value))))
  (when dump-file
    (fprintf dump-file "(answer ~a)\n" (string-join (map ~a (cons status (or value '()))) " "))
    (flush-output dump-file))
  (when (> (rival-profile machine 'bumps) 0)
    (warn 'ground-truth
          "Could not converge on a ground truth"
          #:extra (for/list ([var (in-vector vars)]
                             [val (in-vector pt)])
                    (format "~a = ~a" var val))))
  (timeline-push!/unsafe 'outcomes
                         (- (current-inexact-milliseconds) start)
                         (rival-profile machine 'iterations)
                         (symbol->string status)
                         1)
  (values status value))

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
