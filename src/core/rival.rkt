;; A narrow shim for Rival's "machine" abstraction.
;; A Rival "machine" performs real evaluation for multiple expressions on a point.
;;
;; Ensure this file has minimal dependencies since `<herbie>/syntax/syntax.rkt`
;; requires the file to synthesize floating-point implementations!
;;

#lang racket

(require math/bigfloat
         rival
         racket/hash)

(require "../config.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt")

(provide (struct-out real-compiler)
         (contract-out
          [make-real-compiler
           (->i
            ([batch batch?]
             [brfs (listof batchref?)]
             [ctxs (brfs) (and/c unified-contexts? (lambda (ctxs) (= (length brfs) (length ctxs))))])
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
  (discretization (representation-total-bits repr)
                  (representation-bf->repr repr)
                  (lambda (x y) (- (ulp-difference x y repr) 1))))

;; Herbie's wrapper around the Rival machine abstraction.
(struct real-compiler (pre vars var-reprs exprs reprs machine dump-file assemble))

;; Takes a context to encode input variables and their representations,
;; a list of expressions, and a list of output representations
;; for each expression. Optionally, takes a precondition.
;; Returns flattened specs/contexts for Rival plus a reassembler for points.
(define (flatten-arrays-for-rival specs ctxs pre)
  ;; Flatten array representations into multiple scalar vars/outputs without involving Rival arrays.
  (define (scalar-expr v who)
    (match v
      [`(scalar ,e) e]
      [`(array ,_ ,_) (error who "Expected scalar expression, got array")]))
  (define (select-component arr idx who)
    (unless (and (integer? idx) (<= 0 idx 1))
      (error who "Array index must be literal 0 or 1, got ~a" idx))
    (match arr
      [`(array ,a ,b) (if (zero? idx) a b)]
      [_ (error who "ref expects an array value, got ~a" arr)]))
  (define (lower-arr expr env)
    (match expr
      [(? number?) `(scalar ,expr)]
      [(? symbol? s) (hash-ref env s `(scalar ,s))]
      [`(array ,a ,b)
       `(array ,(scalar-expr (lower-arr a env) 'array) ,(scalar-expr (lower-arr b env) 'array))]
      [`(ref ,arr ,idx ,rest ...)
       (define arr* (lower-arr arr env))
       (define selected
         (for/fold ([current arr*]) ([i (in-list (cons idx rest))])
           (select-component current
                             (if (syntax? i)
                                 (syntax-e i)
                                 i)
                             'ref)))
       `(scalar ,selected)]
      [`(let ([,ids ,vals] ...) ,body)
       (define env* env)
       (for ([id (in-list ids)]
             [val (in-list vals)])
         (define lowered (lower-arr val env*))
         (set! env* (hash-set env* id lowered)))
       (lower-arr body env*)]
      [`(let* ([,ids ,vals] ...) ,body)
       (define env* env)
       (for ([id (in-list ids)]
             [val (in-list vals)])
         (define lowered (lower-arr val env*))
         (set! env* (hash-set env* id lowered)))
       (lower-arr body env*)]
      [`(if ,c ,t ,f)
       (let* ([c* (scalar-expr (lower-arr c env) 'if)]
              [t* (lower-arr t env)]
              [f* (lower-arr f env)]
              [t-expr (match t*
                        [`(scalar ,t**) t**]
                        [_ (error 'if "If branches must be scalars")])]
              [f-expr (match f*
                        [`(scalar ,f**) f**]
                        [_ (error 'if "If branches must be scalars")])])
         `(scalar (if ,c* ,t-expr ,f-expr)))]
      [`(! ,props ... ,body) `(scalar (! ,@props ,(scalar-expr (lower-arr body env) '!)))]
      [`(,op ,args ...)
       (define lowered-args (map (lambda (a) (scalar-expr (lower-arr a env) op)) args))
       `(scalar (,op ,@lowered-args))]
      [_ `(scalar ,expr)]))
  (define orig-vars (context-vars (first ctxs)))
  (define orig-reprs (map context-repr ctxs))
  (define orig-var-reprs (context-var-reprs (first ctxs)))
  (define taken (list->seteq orig-vars))
  (define (fresh base)
    (let loop ([i 0])
      (define candidate (string->symbol (format "~a_~a" base i)))
      (if (set-member? taken candidate)
          (loop (add1 i))
          candidate)))
  (define env (make-hasheq))
  (define new-vars '())
  (define new-var-reprs '())
  (for ([v orig-vars]
        [r orig-var-reprs])
    (cond
      [(eq? (representation-type r) 'array)
       (define base (symbol->string v))
       (define v0 (fresh base))
       (set! taken (set-add taken v0))
       (define v1 (fresh base))
       (set! taken (set-add taken v1))
       (hash-set! env v `(array ,v0 ,v1))
       (set! new-vars (append new-vars (list v0 v1)))
       (set! new-var-reprs
             (append new-var-reprs
                     (list (array-representation-elem r) (array-representation-elem r))))]
      [else
       (hash-set! env v `(scalar ,v))
       (set! new-vars (append new-vars (list v)))
       (set! new-var-reprs (append new-var-reprs (list r)))]))
  (define env-immutable env)
  (define (lower-scalar expr)
    (scalar-expr (lower-arr expr env-immutable) 'program))
  (define (lower-any expr)
    (lower-arr expr env-immutable))
  (define new-specs
    (append* (for/list ([spec (in-list specs)]
                        [repr orig-reprs])
               (cond
                 [(eq? (representation-type repr) 'array)
                  (define lowered (lower-any spec))
                  (list (select-component lowered 0 'flatten-arrays-for-rival)
                        (select-component lowered 1 'flatten-arrays-for-rival))]
                 [else (list (lower-scalar spec))]))))
  (define new-reprs
    (append* (for/list ([repr (in-list orig-reprs)])
               (match (representation-type repr)
                 ['array
                  (define len (apply * (array-representation-dims repr)))
                  (for/list ([i (in-range len)])
                    (array-representation-elem repr))]
                 [_ (list repr)]))))
  (define new-pre (lower-scalar pre))
  (define ctxs*
    (for/list ([ctx (in-list ctxs)])
      (define repr*
        (match (representation-type (context-repr ctx))
          ['array (array-representation-elem (context-repr ctx))]
          [_ (context-repr ctx)]))
      (context new-vars repr* new-var-reprs)))
  (define (assemble-point pt)
    (define idx 0)
    (list->vector (for/list ([r (in-list orig-var-reprs)])
                    (match (representation-type r)
                      ['array
                       (define len (apply * (array-representation-dims r)))
                       (define elems
                         (for/list ([i (in-range len)])
                           (define val (vector-ref pt idx))
                           (set! idx (add1 idx))
                           val))
                       (list->vector elems)]
                      [_
                       (define val (vector-ref pt idx))
                       (set! idx (add1 idx))
                       val]))))
  (values new-specs ctxs* new-pre assemble-point new-reprs))

;; Creates a Rival machine.
(define (make-real-compiler batch brfs ctxs #:pre [pre '(TRUE)])
  (define specs (map (batch-exprs batch) brfs))
  (define-values (vars reprs specs* ctxs* pre* assemble)
    (let-values ([(specs* ctxs* pre* assemble reprs*) (flatten-arrays-for-rival specs ctxs pre)])
      (values (context-vars (first ctxs*)) reprs* specs* ctxs* pre* assemble)))
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
                 assemble))

(define (bigfloat->readable-string x)
  (define real (bigfloat->real x)) ; Exact rational unless inf/nan
  (define float (real->double-flonum real))
  (if (= real float)
      (format "#i~a" float) ; The #i explicitly means nearest float
      (number->string real))) ; Backup is print as rational

;; Runs a Rival machine on an input point.
(define (real-apply compiler pt [hint #f])
  (match-define (real-compiler _ vars var-reprs _ _ machine dump-file _) compiler)
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
  (define mixsample-table (make-hash))
  (for ([execution (in-vector executions)])
    (define name (symbol->string (execution-name execution)))
    (define precision
      (- (execution-precision execution) (remainder (execution-precision execution) prec-threshold)))
    (define key (cons name precision))
    ;; Uses vectors to avoid allocation; this is really allocation-heavy
    (define data (hash-ref! mixsample-table key (lambda () (make-vector 2 0))))
    (vector-set! data 0 (+ (vector-ref data 0) (execution-time execution)))
    (vector-set! data 1 (+ (vector-ref data 1) (execution-memory execution))))
  (for ([(key val) (in-hash mixsample-table)])
    (timeline-push!/unsafe 'mixsample (vector-ref val 0) (car key) (cdr key) (vector-ref val 1)))
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

(module+ test
  (require rackunit)
  (define <b64> <binary64>)
  (define arr-repr (make-array-representation #:name 'arraybinary64 #:elem <b64> #:dims '(2)))
  (define arr-ctx (context '(v) arr-repr (list arr-repr)))
  (define-values (specs* ctxs* pre* _assemble reprs*)
    (flatten-arrays-for-rival (list 'v) (list arr-ctx) 'TRUE))
  (check-equal? specs* '(v_0 v_1))
  (check-equal? (map context-vars ctxs*) '((v_0 v_1)))
  (check-equal? (map context-var-reprs ctxs*) (list (list <b64> <b64>)))
  (check-equal? reprs* (list <b64> <b64>))
  (check-equal? pre* 'TRUE))
