#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path
         math/bigfloat
         (only-in math/private/bigfloat/mpfr _mpfr-pointer)
         "ops.rkt")

(provide rival-compile
         rival-apply
         rival-apply-batch
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

(define-rival rival_compile (_fun _string _string _uint32 _pointer _uint32 -> _pointer))

(define-rival rival_destroy (_fun _pointer -> _void))

(define-rival rival_apply
              (_fun _pointer _pointer _size _pointer _size _pointer _size _size -> _int32))

(define-rival rival_apply_batch
              (_fun _pointer _size _pointer _size _pointer _size _pointer _pointer _size _pointer -> _int32))

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
                 max-iterations))

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

;; Ensure batch buffers are allocated for the given size
(define (ensure-batch-buffers! machine batch-size n-args n-outs)
  (when (or (not (machine-wrapper-batch-args-buf machine))
            (> batch-size (machine-wrapper-batch-size machine)))
    ;; Free old buffers if any
    (when (machine-wrapper-batch-args-buf machine)
      (free (machine-wrapper-batch-args-buf machine))
      (free (machine-wrapper-batch-outs-buf machine))
      (free (machine-wrapper-batch-hints-buf machine))
      (free (machine-wrapper-batch-lens-buf machine))
      (free (machine-wrapper-batch-status-buf machine)))
    ;; Allocate new buffers
    (set-machine-wrapper-batch-args-buf! machine (malloc _pointer (* batch-size n-args) 'raw))
    (set-machine-wrapper-batch-outs-buf! machine (malloc _pointer (* batch-size n-outs) 'raw))
    (set-machine-wrapper-batch-hints-buf! machine (malloc _pointer batch-size 'raw))
    (set-machine-wrapper-batch-lens-buf! machine (malloc _size batch-size 'raw))
    (set-machine-wrapper-batch-status-buf! machine (malloc _int32 batch-size 'raw))
    (set-machine-wrapper-batch-size! machine batch-size)))

(define (ensure-analyze-buffers! machine rect-len)
  (when (or (not (machine-wrapper-analyze-lo-buf machine))
            (not (= rect-len (machine-wrapper-analyze-size machine))))
    (when (machine-wrapper-analyze-lo-buf machine)
      (free (machine-wrapper-analyze-lo-buf machine))
      (free (machine-wrapper-analyze-hi-buf machine)))
    (set-machine-wrapper-analyze-lo-buf! machine (malloc _pointer rect-len 'raw))
    (set-machine-wrapper-analyze-hi-buf! machine (malloc _pointer rect-len 'raw))
    (set-machine-wrapper-analyze-size! machine rect-len))
  (when (not (machine-wrapper-analyze-status-lo machine))
    (set-machine-wrapper-analyze-status-lo! machine (bf 0.0))
    (set-machine-wrapper-analyze-status-hi! machine (bf 0.0))
    (set-machine-wrapper-analyze-converged-buf! machine (malloc _byte 1 'raw))
    (set-machine-wrapper-analyze-hints-len-buf! machine (malloc _size 1 'raw)))
  (when (not (machine-wrapper-analyze-hints-buf machine))
    (define init-capacity (max 64 (* 4 rect-len)))
    (set-machine-wrapper-analyze-hints-buf! machine (malloc _byte init-capacity 'raw))
    (set-machine-wrapper-analyze-hints-capacity! machine init-capacity)))

(define (rival-apply-batch machine pts-and-hints [max-iterations (*rival-max-iterations*)])
  (define batch-size (vector-length pts-and-hints))
  (when (zero? batch-size)
    (vector))

  (define n-args (vector-length (car (vector-ref pts-and-hints 0))))
  (define n-outs (length (machine-wrapper-discs machine)))
  (define discs (machine-wrapper-discs machine))

  ;; Ensure pre-allocated buffers are ready
  (ensure-batch-buffers! machine batch-size n-args n-outs)
  (define all-args-ptr (machine-wrapper-batch-args-buf machine))
  (define all-outs-ptr (machine-wrapper-batch-outs-buf machine))
  (define hints-ptrs (machine-wrapper-batch-hints-buf machine))
  (define hints-lens (machine-wrapper-batch-lens-buf machine))
  (define status-out (machine-wrapper-batch-status-buf machine))

  ;; Allocate output bigfloats (must be fresh each call to hold results)
  (define all-outs (build-vector (* batch-size n-outs) (lambda (_) (bf 0.0))))

  ;; Allocate malloc'd buffers for hint bytes (will be freed after FFI call)
  ;; NOTE: We must copy hint bytes to malloc'd memory because Racket's GC can move bytes objects,
  ;; invalidating the pointers we pass to Rust!
  (define hints-malloc-ptrs (make-vector batch-size #f))

  ;; Fill in the argument and hints buffers
  (for ([i (in-range batch-size)])
    (define pt-hint (vector-ref pts-and-hints i))
    (define pt (car pt-hint))
    (define hint (cdr pt-hint))

    ;; Fill argument pointers
    (for ([j (in-range n-args)]
          [bf-val (in-vector pt)])
      (ptr-set! all-args-ptr _pointer (+ (* i n-args) j) bf-val))

    ;; Fill output pointers
    (for ([j (in-range n-outs)])
      (ptr-set! all-outs-ptr _pointer (+ (* i n-outs) j) (vector-ref all-outs (+ (* i n-outs) j))))

    ;; Serialize hints and copy to malloc'd memory
    (define hb (if hint (serialize-hints-binary hint) #""))
    (define hb-len (bytes-length hb))
    (cond
      [(> hb-len 0)
       (define malloc-ptr (malloc _byte hb-len 'raw))
       (memcpy malloc-ptr hb hb-len)
       (vector-set! hints-malloc-ptrs i malloc-ptr)
       (ptr-set! hints-ptrs _pointer i malloc-ptr)
       (ptr-set! hints-lens _size i hb-len)]
      [else
       (ptr-set! hints-ptrs _pointer i #f)
       (ptr-set! hints-lens _size i 0)]))

  ;; Call the batch FFI function
  (define res-code
    (rival_apply_batch (machine-wrapper-ptr machine)
                       batch-size
                       all-args-ptr
                       n-args
                       all-outs-ptr
                       n-outs
                       hints-ptrs
                       hints-lens
                       max-iterations
                       status-out))

  ;; Free malloc'd hint buffers
  (for ([ptr (in-vector hints-malloc-ptrs)])
    (when ptr (free ptr)))

  ;; If panic occurred, return #f to signal caller to fall back
  (cond
    [(= res-code -99) #f]
    [else
     ;; Read results
     (for/vector #:length batch-size
                 ([i (in-range batch-size)])
       (define status (ptr-ref status-out _int32 i))
       (cons status
             (if (= status 0)
                 (list->vector
                  (for/list ([j (in-range n-outs)]
                             [disc (in-list discs)])
                    ((discretization-convert disc) (vector-ref all-outs (+ (* i n-outs) j)))))
                 #f)))]))

(define-rival rival_analyze_with_hints
  (_fun _pointer _pointer _pointer _size _bytes _size _pointer _pointer _pointer _pointer _size _pointer -> _int32))

(define-rival rival_profile (_fun _pointer _string -> _pointer))

(define-rival rival_free_string (_fun _pointer -> _void))

;; Wrapper struct to hold callbacks and pointer
;; Includes batch buffers for efficient batched evaluation
(struct machine-wrapper (ptr discs arg-buf out-buf
                         [batch-args-buf #:mutable]
                         [batch-outs-buf #:mutable]
                         [batch-hints-buf #:mutable]
                         [batch-lens-buf #:mutable]
                         [batch-status-buf #:mutable]
                         [batch-size #:mutable]
                         [analyze-lo-buf #:mutable]
                         [analyze-hi-buf #:mutable]
                         [analyze-status-lo #:mutable]
                         [analyze-status-hi #:mutable]
                         [analyze-converged-buf #:mutable]
                         [analyze-hints-buf #:mutable]
                         [analyze-hints-len-buf #:mutable]
                         [analyze-hints-capacity #:mutable]
                         [analyze-size #:mutable])
  #:property prop:cpointer (struct-field-index ptr))

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
  (free (machine-wrapper-out-buf wrapper))
  ;; Free batch buffers if allocated
  (when (machine-wrapper-batch-args-buf wrapper)
    (free (machine-wrapper-batch-args-buf wrapper)))
  (when (machine-wrapper-batch-outs-buf wrapper)
    (free (machine-wrapper-batch-outs-buf wrapper)))
  (when (machine-wrapper-batch-hints-buf wrapper)
    (free (machine-wrapper-batch-hints-buf wrapper)))
  (when (machine-wrapper-batch-lens-buf wrapper)
    (free (machine-wrapper-batch-lens-buf wrapper)))
  (when (machine-wrapper-batch-status-buf wrapper)
    (free (machine-wrapper-batch-status-buf wrapper)))
  (when (machine-wrapper-analyze-lo-buf wrapper)
    (free (machine-wrapper-analyze-lo-buf wrapper)))
  (when (machine-wrapper-analyze-hi-buf wrapper)
    (free (machine-wrapper-analyze-hi-buf wrapper)))
  (when (machine-wrapper-analyze-converged-buf wrapper)
    (free (machine-wrapper-analyze-converged-buf wrapper)))
  (when (machine-wrapper-analyze-hints-buf wrapper)
    (free (machine-wrapper-analyze-hints-buf wrapper)))
  (when (machine-wrapper-analyze-hints-len-buf wrapper)
    (free (machine-wrapper-analyze-hints-len-buf wrapper))))

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

  (define ptr (rival_compile exprs-str vars-str target types-ptr num-types))

  (free types-ptr)

  (if (not ptr)
      (error "rival-compile failed")
      (begin
        (let ([n-args (length vars)]
              [n-outs (length discs)])
          (define arg-buf (malloc _pointer n-args 'raw))
          (define out-buf (malloc _pointer n-outs 'raw))
          (define wrapper (machine-wrapper ptr discs arg-buf out-buf
                                           #f #f #f #f #f 0
                                           #f #f #f #f #f #f #f 0 0))
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

(define (deserialize-hints len ref)
  (let loop ([idx 0] [acc '()])
    (if (>= idx len)
        (list->vector (reverse acc))
        (match (ref idx)
          [0 (loop (add1 idx) (cons 'execute acc))]
          [1 (loop (add1 idx) (cons 'skip acc))]
          [2 (if (>= (+ idx 1) len)
                 (error 'deserialize-hints "Incomplete alias")
                 (loop (+ idx 2) (cons (list 'alias (ref (add1 idx))) acc)))]
          [3 (if (>= (+ idx 1) len)
                 (error 'deserialize-hints "Incomplete known-bool")
                 (loop (+ idx 2) (cons (list 'known-bool (not (zero? (ref (add1 idx))))) acc)))]
          [_ (error 'deserialize-hints (format "Unknown hint tag ~a" (ref idx)))]))))

(define (deserialize-hints-binary b len)
  (deserialize-hints len (lambda (i) (bytes-ref b i))))

(define (deserialize-hints-pointer p len)
  (deserialize-hints len (lambda (i) (ptr-ref p _byte i))))

(define (rival-analyze-with-hints machine rect [hint #f])
  (define rect-len (vector-length rect))
  (ensure-analyze-buffers! machine rect-len)

  (define lo-buf (machine-wrapper-analyze-lo-buf machine))
  (define hi-buf (machine-wrapper-analyze-hi-buf machine))
  (for ([i (in-range rect-len)]
        [ival (in-vector rect)])
    (ptr-set! lo-buf _pointer i (ival-lo ival))
    (ptr-set! hi-buf _pointer i (ival-hi ival)))

  (define hint-bytes (serialize-hints-binary hint))
  (define hint-len (bytes-length hint-bytes))

  (define status-lo (machine-wrapper-analyze-status-lo machine))
  (define status-hi (machine-wrapper-analyze-status-hi machine))
  (define converged-buf (machine-wrapper-analyze-converged-buf machine))
  (define hints-buf (machine-wrapper-analyze-hints-buf machine))
  (define hints-cap (machine-wrapper-analyze-hints-capacity machine))
  (define hints-len-buf (machine-wrapper-analyze-hints-len-buf machine))

  (define (call-analyze)
    (rival_analyze_with_hints (machine-wrapper-ptr machine)
                              lo-buf
                              hi-buf
                              rect-len
                              hint-bytes
                              hint-len
                              status-lo
                              status-hi
                              converged-buf
                              hints-buf
                              hints-cap
                              hints-len-buf))

  (define res-code (call-analyze))
  (when (= res-code 1)
    (define needed (ptr-ref hints-len-buf _size 0))
    (when (> needed hints-cap)
      (free hints-buf)
      (set-machine-wrapper-analyze-hints-buf! machine (malloc _byte needed 'raw))
      (set-machine-wrapper-analyze-hints-capacity! machine needed)
      (set! hints-buf (machine-wrapper-analyze-hints-buf machine))
      (set! hints-cap needed))
    (set! res-code (call-analyze)))

  (cond
    [(= res-code 0)
     (define hint-size (ptr-ref hints-len-buf _size 0))
     (define next-hints (deserialize-hints-pointer hints-buf hint-size))
     (define lo-bool (not (bfzero? status-lo)))
     (define hi-bool (not (bfzero? status-hi)))
     (define converged? (not (zero? (ptr-ref converged-buf _byte 0))))
     (list (ival lo-bool hi-bool) next-hints converged?)]
    [(= res-code -99) (error 'rival-analyze-with-hints "Rival panic")]
    [else (error 'rival-analyze-with-hints (format "Unknown result code: ~a" res-code))]))

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
