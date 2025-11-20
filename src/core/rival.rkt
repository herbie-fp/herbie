;; A narrow shim for Rival's "machine" abstraction.
;; A Rival "machine" performs real evaluation for multiple expressions on a point.
;;
;; Ensure this file has minimal dependencies since `<herbie>/syntax/syntax.rkt`
;; requires the file to synthesize floating-point implementations!
;;

#lang racket

(require math/bigfloat
         (except-in ffi/unsafe ->)
         rival-herbie)

(require "../config.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt")

(provide (struct-out real-compiler)
         (contract-out
          [make-real-compiler
           (->* ((listof any/c) (non-empty-listof context?)) (#:pre any/c) real-compiler?)]
          [real-apply
           (->* (real-compiler? vector?)
                ((or/c #f list?))
                (values (or/c 'valid 'invalid 'unknown 'exit) (or/c #f (listof bigfloat?))))]
          [real-compiler-clear! (-> real-compiler? void?)]
          [real-compiler-analyze (->* (real-compiler? vector?) ((or/c #f list?)) list?)]))

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
(struct real-compiler (pre vars var-reprs exprs reprs machine dump-file callbacks))

;; Creates a Rival machine.
(define (make-real-compiler specs ctxs #:pre [pre '(TRUE)])
  (define vars (context-vars (first ctxs)))
  (define reprs (map context-repr ctxs))
  (define exprs (cons `(assert ,pre) specs))

  ;; Prepare arguments for rival_make_context
  (define exprs-str (string-append "(" (string-join (map ~a exprs) " ") ")"))
  (define vars-str (string-join (map symbol->string vars) " "))

  (define all-binary64? (andmap (lambda (r) (equal? (representation-name r) 'binary64)) reprs))
  (define use-callback (not all-binary64?))

  ;; Callbacks
  (define convert-cb
    (if use-callback
        (lambda (idx val-str)
          (define repr
            (if (= idx 0)
                <bool>
                (list-ref reprs (- idx 1))))
          (define bf-val (bf val-str))
          (define val ((representation-bf->repr repr) bf-val))
          (define bf* ((representation-repr->bf repr) val))
          (string->c-pointer (bigfloat->string bf*)))
        #f))

  (define distance-cb
    (if use-callback
        (lambda (idx lo-str hi-str)
          (define repr
            (if (= idx 0)
                <bool>
                (list-ref reprs (- idx 1))))
          (define lo (bf lo-str))
          (define hi (bf hi-str))
          (define lo-ord ((representation-repr->ordinal repr) ((representation-bf->repr repr) lo)))
          (define hi-ord ((representation-repr->ordinal repr) ((representation-bf->repr repr) hi)))
          (abs (- hi-ord lo-ord)))
        #f))

  (define free-cb (if use-callback c-free #f))

  (define machine
    (rival_make_context exprs-str vars-str use-callback 53 convert-cb distance-cb free-cb))

  (when (not machine)
    (error 'make-real-compiler "Failed to create Rival machine"))

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

  (real-compiler pre
                 (list->vector vars)
                 (list->vector (context-var-reprs (first ctxs)))
                 specs
                 (list->vector reprs)
                 machine
                 dump-file
                 (list convert-cb distance-cb))) ; Keep callbacks alive

(define (serialize-hint h)
  (match h
    ['execute "execute"]
    ['skip "skip"]
    [(list 'alias n) (format "(alias ~a)" n)]
    [(list 'known-bool b)
     (format "(known-bool ~a)" (if (or (equal? b 'true) (equal? b #t)) "true" "false"))]
    [_ (error 'serialize-hint "Unknown hint: ~a" h)]))

(define (serialize-hints hints)
  (if hints
      (string-append "(" (string-join (map serialize-hint hints) " ") ")")
      "false"))

(define (real-apply compiler pt [hint #f])
  (match-define (real-compiler _ vars var-reprs exprs _ machine dump-file _) compiler)
  (define start (current-inexact-milliseconds))
  (define pt*
    (for/vector #:length (vector-length vars)
                ([val (in-vector pt)]
                 [repr (in-vector var-reprs)])
      ((representation-repr->bf repr) val)))

  (define args-str (string-join (map bigfloat->string (vector->list pt*)) " "))
  (define hints-str (serialize-hints hint))

  (when dump-file
    (fprintf dump-file "(eval f ~a)\n" args-str))

  (define res-ptr (rival_eval machine args-str hints-str))
  (define res-str (cast res-ptr _pointer _string/utf-8))
  (define res-list (read (open-input-string res-str)))
  (rival_free_string res-ptr)

  (define-values (status value)
    (match res-list
      [(list 'valid vals ...)
       ;; Drop the first value (precondition)
       (define vals* (cdr vals))
       (define parsed-vals (map (lambda (s) (bf s)) vals*))
       (values 'valid parsed-vals)]
      [(list 'invalid) (values 'invalid #f)]
      [(list 'unsamplable) (values 'exit #f)]
      [_ (error 'real-apply "Unknown result from rival: ~a" res-list)]))

  (timeline-push!/unsafe 'outcomes
                         (- (current-inexact-milliseconds) start)
                         1 ; iterations (dummy)
                         (symbol->string status)
                         1)
  (values status value))

(define (real-compiler-clear! compiler)
  (void))

(define (real-compiler-analyze compiler input-ranges [hint #f])
  (match-define (real-compiler _ vars var-reprs _ _ machine dump-file _) compiler)

  (define args-str
    (string-join (for/list ([iv (in-vector input-ranges)])
                   (format "~a ~a" (bigfloat->string (ival-lo iv)) (bigfloat->string (ival-hi iv))))
                 " "))

  (define hints-str (serialize-hints hint))

  (define res-ptr (rival_analyze machine args-str hints-str))
  (define res-str (cast res-ptr _pointer _string/utf-8))
  (define res-list (read (open-input-string res-str)))
  (rival_free_string res-ptr)

  (match res-list
    [(list lo-str hi-str converged hints)
     (define lo (bf lo-str))
     (define hi (bf hi-str))
     (define status (ival (not (bfzero? lo)) (not (bfzero? hi))))
     (define next-hint hints) ; hints is already a list of symbols/lists from read
     (define conv (equal? converged 'true))
     (list status next-hint conv)]
    [_ (error 'real-compiler-analyze "Unknown result: ~a" res-list)]))
