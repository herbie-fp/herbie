#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path
         math/bigfloat
         (only-in math/private/bigfloat/mpfr _mpfr-pointer)
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
(struct discretization (target convert distance type))
(struct ival (lo hi) #:transparent)

(define boolean-discretization (discretization 53 values (lambda (x y) (if (eq? x y) 0 2)) 0))

(define _profile-field (_enum '(instructions = 0 iterations = 1 bumps = 2 executions = 3) _uint32))

;; Parameters
(define *rival-max-precision* (make-parameter 256))
(define *rival-max-iterations* (make-parameter 5))
(define *rival-profile-executions* (make-parameter 1000))

;; Execution record layout:
;; - name_ptr: pointer (8 bytes)
;; - name_len: size_t (8 bytes)
;; - number: i32 (4 bytes)
;; - precision: u32 (4 bytes)
;; - time_ms: f64 (8 bytes)
;; - iteration: size_t (8 bytes)
(define execution-record-size 40)

;; Profile data layout:
;; - instructions_len: size_t (8 bytes)
;; - iterations: size_t (8 bytes)
;; - bumps: size_t (8 bytes)
;; - executions_ptr: pointer (8 bytes)
;; - executions_len: size_t (8 bytes)
(define _profile-data (_list-struct _size _size _size _pointer _size))

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

(define-rival rival_compile
              (_fun _string _string _uint32 _pointer _uint32 _uint32 _uint32 -> _pointer))

(define-rival rival_destroy (_fun _pointer -> _void))

(define-rival rival_apply
              (_fun _pointer _pointer _size _pointer _size _pointer _size _size _uint32 -> _int32))

(define (rival-apply machine pt [hints #f] [max-iterations (*rival-max-iterations*)])
  (define n-args (vector-length pt))
  (define arg-ptrs (machine-wrapper-arg-buf machine))
  (for ([i (in-range n-args)]
        [bf (in-vector pt)])
    (ptr-set! arg-ptrs _pointer i bf))

  (define n-outs (length (machine-wrapper-discs machine)))
  (define outs (build-vector n-outs (lambda (_) (bf 0.0))))
  (define out-ptrs (machine-wrapper-out-buf machine))
  (for ([i (in-range n-outs)]
        [bf (in-vector outs)])
    (ptr-set! out-ptrs _pointer i bf))

  (define hints-bytes
    (if hints
        (serialize-hints-binary hints)
        #""))
  (define res-code
    (rival_apply (machine-wrapper-ptr machine)
                 arg-ptrs
                 n-args
                 out-ptrs
                 n-outs
                 hints-bytes
                 (bytes-length hints-bytes)
                 max-iterations
                 (*rival-max-precision*)))

  (match res-code
    [0
     (define discs (machine-wrapper-discs machine))
     (list->vector (for/list ([bf (in-vector outs)]
                              [disc (in-list discs)])
                     ((discretization-convert disc) bf)))]
    [-1 (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
    [-2 (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
    [-99 (error 'rival-apply "Rival panic")]
    [else (error 'rival-apply "Unknown result code: ~a" res-code)]))

(define-rival rival_analyze_with_hints (_fun _pointer _string _bytes _size -> _pointer))

(define-rival rival_profile (_fun _pointer _profile-field -> _profile-data))

(define-rival rival_free_string (_fun _pointer -> _void))

;; Wrapper struct to hold callbacks and pointer
(struct machine-wrapper (ptr discs arg-buf out-buf) #:property prop:cpointer (struct-field-index ptr))

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

(define (machine-destroy wrapper)
  (rival_destroy (machine-wrapper-ptr wrapper))
  (free (machine-wrapper-arg-buf wrapper))
  (free (machine-wrapper-out-buf wrapper)))

(define (rival-compile exprs vars discs)
  (define exprs-str (format "~a" exprs))
  (define vars-str (string-join (map symbol->string vars) " "))

  (define target
    (if (null? discs)
        0
        (discretization-target (car discs))))

  (define num-types (length discs))
  (define types-ptr (malloc _uint32 num-types 'raw))
  (for ([i (in-naturals)]
        [disc (in-list discs)])
    (ptr-set! types-ptr _uint32 i (discretization-type disc)))

  (define ptr
    (rival_compile exprs-str
                   vars-str
                   target
                   types-ptr
                   num-types
                   (*rival-max-precision*)
                   (*rival-profile-executions*)))

  (free types-ptr)

  (if (not ptr)
      (error "rival-compile failed")
      (begin
        (let ([n-args (length vars)]
              [n-outs (length discs)])
          (define arg-buf (malloc _pointer n-args 'raw))
          (define out-buf (malloc _pointer n-outs 'raw))
          (define wrapper (machine-wrapper ptr discs arg-buf out-buf))
          (register-finalizer wrapper machine-destroy)
          wrapper))))

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

;; Read execution record directly to avoid cast operation (much more efficient)
(define (read-execution-at base-ptr i)
  (define p (ptr-add base-ptr (* i execution-record-size)))
  (define name-ptr (ptr-ref p _pointer))
  (define name-len (ptr-ref (ptr-add p 8) _size))
  (define name-bytes (make-bytes name-len))
  (memcpy name-bytes name-ptr name-len)
  (execution (string->symbol (bytes->string/utf-8 name-bytes))
             (ptr-ref (ptr-add p 16) _int32) ; number
             (ptr-ref (ptr-add p 20) _uint32) ; precision
             (ptr-ref (ptr-add p 24) _double) ; time_ms
             0 ; memory
             (ptr-ref (ptr-add p 32) _size))) ; iteration

(define (rival-profile machine param)
  (match-define (list instructions-len iterations bumps executions-ptr executions-len)
    (rival_profile machine param))
  (match param
    ['executions
     (for/vector #:length executions-len
                 ([i (in-range executions-len)])
       (read-execution-at executions-ptr i))]
    ['instructions instructions-len]
    ['iterations iterations]
    ['bumps bumps]))
