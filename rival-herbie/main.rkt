#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path
         math/bigfloat
         "ops.rkt")

(provide rival-compile
         rival-apply
         rival-analyze-with-hints
         rival-profile
         (struct-out exn:rival)
         (struct-out exn:rival:invalid)
         (struct-out exn:rival:unsamplable)
         (struct-out execution)
         (struct-out discretization)
         (struct-out ival)
         boolean-discretization
         rival-machine?
         *rival-max-precision*
         *rival-max-iterations*
         *rival-profile-executions*
         (all-from-out "ops.rkt"))

;; Structs
(struct exn:rival exn:fail ())
(struct exn:rival:invalid exn:rival (pt))
(struct exn:rival:unsamplable exn:rival (pt))
(struct execution (name number precision time memory iteration) #:prefab)
(struct discretization (target convert distance))
(struct ival (lo hi) #:transparent)

(define boolean-discretization (discretization 53 values (lambda (x y) (if (eq? x y) 0 2))))

;; Parameters
(define *rival-max-precision* (make-parameter 256))
(define *rival-max-iterations* (make-parameter 5))
(define *rival-profile-executions* (make-parameter 1000))

;; FFI
(define-runtime-path librival-path
                     (build-path "target/release"
                                 (string-append (case (system-type)
                                                  [(windows) "rival_herbie"]
                                                  [else "librival_herbie"])
                                                (bytes->string/utf-8 (system-type 'so-suffix)))))

(define-ffi-definer define-rival (ffi-lib librival-path))

(define c-free (get-ffi-obj "free" #f (_fun _pointer -> _void)))
(define (free p)
  (c-free p))

(define _convert_cb (_fun _size _string -> _pointer))
(define _distance_cb (_fun _size _string _string -> _size))
(define _free_cb (_fun _pointer -> _void))

(define-rival rival_compile
              (_fun _string _string _uint32 _convert_cb _distance_cb _free_cb -> _pointer))

(define-rival rival_destroy (_fun _pointer -> _void))

(define-rival rival_apply (_fun _pointer _string _bytes _size _size -> _pointer))

(define-rival rival_analyze_with_hints (_fun _pointer _string _bytes _size -> _pointer))

(define-rival rival_profile (_fun _pointer _string -> _pointer))

(define-rival rival_free_string (_fun _pointer -> _void))

;; Wrapper struct to hold callbacks and pointer
(struct machine-wrapper (ptr convert-cb distance-cb free-cb discs)
  #:property prop:cpointer
  (struct-field-index ptr))

(define rival-machine? machine-wrapper?)

(define (string->c-pointer s)
  (define b (string->bytes/utf-8 s))
  (define n (bytes-length b))
  (define p (malloc (add1 n) 'raw))
  (memcpy p b n)
  (ptr-set! p _byte n 0)
  p)

(define (bf->string-for-rust x)
  (cond
    [(bigfloat? x)
     (define s (bigfloat->string x))
     (cond
       [(or (string-contains? s "nan") (string-contains? s "NaN")) "NaN"]
       [(or (string-contains? s "inf") (string-contains? s "Inf"))
        (if (string-prefix? s "-") "-inf" "inf")]
       [else (string-replace s ".bf" "")])]
    [(real? x)
     (cond
       [(nan? x) "NaN"]
       [(infinite? x) (if (positive? x) "inf" "-inf")]
       [else (number->string x)])]
    [(boolean? x) (if x "1.0" "0.0")]
    [else "NaN"]))

(define (rival-compile exprs vars discs)
  (define exprs-str (format "~a" exprs))
  (define vars-str (string-join (map symbol->string vars) " "))

  ;; Callbacks
  (define (convert-cb idx val-str)
    (with-handlers ([exn:fail? (lambda (e)
                                 (displayln (format "Error in convert-cb: ~a" (exn-message e)))
                                 (string->c-pointer "NaN"))])
      (define disc (list-ref discs idx))
      (define val (bf val-str))
      (define res ((discretization-convert disc) val))
      (define res-str (bf->string-for-rust res))
      (string->c-pointer res-str)))
  (define (distance-cb idx lo-str hi-str)
    (with-handlers ([exn:fail? (lambda (e)
                                 (displayln (format "Error in distance-cb: ~a" (exn-message e)))
                                 0)])
      (define disc (list-ref discs idx))
      (define lo (bf lo-str))
      (define hi (bf hi-str))

      (cond
        [(eq? disc boolean-discretization)
         ;; Special handling for boolean discretization
         ;; Convert bigfloats to booleans (0.0 -> #f, anything else -> #t)
         (define lo-bool (not (bfzero? lo)))
         (define hi-bool (not (bfzero? hi)))
         ((discretization-distance disc) lo-bool hi-bool)]
        [else
         ;; Standard handling
         (define lo-conv ((discretization-convert disc) lo))
         (define hi-conv ((discretization-convert disc) hi))
         ((discretization-distance disc) lo-conv hi-conv)])))
  (define (free-cb ptr)
    (free ptr))

  (define target
    (if (null? discs)
        0
        (discretization-target (car discs))))

  (define ptr (rival_compile exprs-str vars-str target convert-cb distance-cb free-cb))

  (if (not ptr)
      (error "rival-compile failed")
      (begin
        (register-finalizer ptr rival_destroy)
        (machine-wrapper ptr convert-cb distance-cb free-cb discs))))

(define (serialize-hints-binary hints)
  (let ([out (open-output-bytes)])
    (let loop ([h hints])
      (match h
        ['execute (write-byte 0 out)]
        ['skip (write-byte 1 out)]
        [(list 'alias n)
         (write-byte 2 out)
         (write-byte n out)]
        [(list 'known-bool b)
         (write-byte 3 out)
         (write-byte (if b 1 0) out)]
        [(list sub-hints ...) (for-each loop sub-hints)]
        [(vector sub-hints ...) (for-each loop sub-hints)]
        [#f (void)]
        [_ (error "Unknown hint" h)]))
    (get-output-bytes out)))

(define (rival-apply machine pt [hint #f])
  (define pt-str (string-join (map bigfloat->string (vector->list pt)) " "))
  (define hint-bytes (serialize-hints-binary hint))
  (define hint-len (bytes-length hint-bytes))
  (define max-iters (*rival-max-iterations*))
  (define discs (machine-wrapper-discs machine))

  (define res-ptr (rival_apply machine pt-str hint-bytes hint-len max-iters))
  (define res-str (cast res-ptr _pointer _string))
  (rival_free_string res-ptr)

  (define res (read (open-input-string res-str)))
  (match res
    [(list 'valid vals ...)
     (list->vector (map (lambda (x disc)
                          (define val-bf
                            (if (string? x)
                                (bf x)
                                (bf (format "~a" x))))
                          ((discretization-convert disc) val-bf))
                        vals
                        discs))]
    ['(invalid) (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
    ['(unsamplable)
     (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
    [_ (error "rival-apply: unexpected result" res)]))

(define (rival-analyze-with-hints machine rect [hint #f])
  (define rect-str
    (string-join (for/list ([i (in-vector rect)])
                   (format "~a ~a" (bigfloat->string (ival-lo i)) (bigfloat->string (ival-hi i))))
                 " "))
  (define hint-bytes (serialize-hints-binary hint))
  (define hint-len (bytes-length hint-bytes))

  (define res-ptr (rival_analyze_with_hints machine rect-str hint-bytes hint-len))
  (define res-str (cast res-ptr _pointer _string))
  (rival_free_string res-ptr)

  (define res (read (open-input-string res-str)))
  ;; (status-lo status-hi converged (hint ...))
  (match res
    [(list lo hi converged hints)
     (define lo-bf (bf lo))
     (define hi-bf (bf hi))
     (define lo-bool (not (bfzero? lo-bf)))
     (define hi-bool (not (bfzero? hi-bf)))
     (list (ival lo-bool hi-bool) (list->vector hints) (equal? converged 'true))]
    [_ (error "rival-analyze-with-hints: unexpected result" res)]))

(define (rival-profile machine param)
  (define param-str (symbol->string param))
  (define res-ptr (rival_profile machine param-str))
  (define res-str (cast res-ptr _pointer _string))
  (rival_free_string res-ptr)

  (define res (read (open-input-string res-str)))
  (match param
    ['executions
     (list->vector (map (lambda (x)
                          (match x
                            [(list name number precision time memory iteration)
                             (execution name number precision time memory iteration)]))
                        res))]
    [_ res]))
