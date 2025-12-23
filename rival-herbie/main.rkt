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
         rival-hints?
         *rival-max-precision*
         *rival-max-iterations*
         *rival-profile-executions*
         *batch-size*
         rival-clear-batch-cache!
         (all-from-out "ops.rkt"))

;; Structs
(struct exn:rival exn:fail ())
(struct exn:rival:invalid exn:rival (pt aggregated-profile))
(struct exn:rival:unsamplable exn:rival (pt aggregated-profile))
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

;; Analyze result layout: status_code, is_error, maybe_error, converged, hints
(define _analyze-result (_list-struct _int32 _stdbool _stdbool _stdbool _pointer))

;; Apply result layout:
;; - status_code: i32 (4 bytes)
;; - bumps: size_t (8 bytes)
;; - iterations: size_t (8 bytes)
;; - profile_ptr: pointer (8 bytes)
;; - profile_len: size_t (8 bytes)
(define _apply-result (_list-struct _int32 _size _size _pointer _size))

;; Aggregated profile entry layout:
;; - instruction_index: i32 (4 bytes)
;; - precision_bucket: u32 (4 bytes)
;; - total_time_ms: f64 (8 bytes)
;; - total_memory: size_t (8 bytes)
(define aggregated-entry-size 24)

;; Batch apply result layout
(define _batch-apply-result
  (_list-struct _size _pointer _size _size _pointer _size))

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

(define-rival rival_hints_destroy (_fun _pointer -> _void))

(define-rival rival_apply
              (_fun _pointer _pointer _size _pointer _size _pointer _size _uint32 -> _apply-result))

(define-rival rival_apply_batch
              (_fun _pointer _size _pointer _size _pointer _size _pointer _size _uint32
                    -> _batch-apply-result))

(define-rival rival_analyze_with_hints (_fun _pointer _pointer _size _pointer -> _analyze-result))

(define-rival rival_profile (_fun _pointer _profile-field -> _profile-data))

(define-rival rival_instruction_names
              (_fun _pointer (len : (_ptr o _size)) -> (ptr : _pointer) -> (values ptr len)))

(define *batch-size* (make-parameter 32))

(struct machine-wrapper (ptr discs n-args n-outs name-table
                         arg-buf-box out-buf-box rect-buf-box
                         batch-arg-buf-box batch-out-buf-box batch-hints-buf-box
                         [batch-capacity #:mutable]
                         [batch-outs-cache #:mutable])
  #:property prop:cpointer (struct-field-index ptr))

;; Wrapper struct for hints
(struct hints-wrapper (ptr) #:property prop:cpointer (struct-field-index ptr))

(define rival-machine? machine-wrapper?)
(define rival-hints? hints-wrapper?)

(define (machine-wrapper-arg-buf m)
  (define box (machine-wrapper-arg-buf-box m))
  (or (unbox box)
      (let ([buf (malloc _pointer (machine-wrapper-n-args m) 'raw)])
        (set-box! box buf)
        buf)))

(define (machine-wrapper-out-buf m)
  (define box (machine-wrapper-out-buf-box m))
  (or (unbox box)
      (let ([buf (malloc _pointer (machine-wrapper-n-outs m) 'raw)])
        (set-box! box buf)
        buf)))

(define (machine-wrapper-rect-buf m)
  (define box (machine-wrapper-rect-buf-box m))
  (or (unbox box)
      (let ([buf (malloc _pointer (* 2 (machine-wrapper-n-args m)) 'raw)])
        (set-box! box buf)
        buf)))

(define (ensure-batch-buffers! m batch-size)
  (define current-cap (machine-wrapper-batch-capacity m))
  (when (or (not current-cap) (> batch-size current-cap))
    (define n-args (machine-wrapper-n-args m))
    (define n-outs (machine-wrapper-n-outs m))
    (define arg-box (machine-wrapper-batch-arg-buf-box m))
    (define out-box (machine-wrapper-batch-out-buf-box m))
    (define hints-box (machine-wrapper-batch-hints-buf-box m))
    (when (unbox arg-box) (free (unbox arg-box)))
    (when (unbox out-box) (free (unbox out-box)))
    (when (unbox hints-box) (free (unbox hints-box)))
    (set-box! arg-box (malloc _pointer (* batch-size n-args) 'raw))
    (set-box! out-box (malloc _pointer (* batch-size n-outs) 'raw))
    (set-box! hints-box (malloc _pointer batch-size 'raw))
    (set-machine-wrapper-batch-capacity! m batch-size)
    (set-machine-wrapper-batch-outs-cache! m #f)))

(define (ensure-batch-outs-cache! m batch-size)
  (define cache (machine-wrapper-batch-outs-cache m))
  (define n-outs (machine-wrapper-n-outs m))
  (cond
    [(and cache (>= (vector-length cache) batch-size)) cache]
    [else
     (define new-cache
       (for/vector #:length batch-size ([_ (in-range batch-size)])
         (for/vector #:length n-outs ([_ (in-range n-outs)])
           (bf 0.0))))
     (set-machine-wrapper-batch-outs-cache! m new-cache)
     new-cache]))

;; Clears the batch outputs cache to release bigfloat memory (GC run?)
(define (rival-clear-batch-cache! machine)
  (set-machine-wrapper-batch-outs-cache! machine #f))

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
      (let ([n-args (length vars)]
            [n-outs (length discs)])
        ;; Build name table (always needed)
        (define-values (names-ptr names-len) (rival_instruction_names ptr))
        (define names-bytes (make-bytes names-len))
        (memcpy names-bytes names-ptr names-len)
        (define name-table
          (list->vector
            (map string->symbol
                 (string-split (bytes->string/utf-8 names-bytes) "\0"))))
        ;; Create wrapper with lazy buffer boxes (all start as #f)
        (define wrapper (machine-wrapper ptr discs n-args n-outs name-table
                                         (box #f) (box #f) (box #f)
                                         (box #f) (box #f) (box #f)
                                         #f #f))
        (register-finalizer wrapper machine-destroy)
        wrapper)))

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

  (define hints-ptr
    (if hints
        (hints-wrapper-ptr hints)
        #f))

  (match-define (list status-code bumps iterations profile-ptr profile-len)
    (rival_apply (machine-wrapper-ptr machine)
                 arg-ptrs
                 n-args
                 out-ptrs
                 n-outs
                 hints-ptr
                 max-iterations
                 (*rival-max-precision*)))

  ;; Don't let the gc go to town
  (void (vector-length pt))
  (void hints)

  (define name-table (machine-wrapper-name-table machine))
  (define aggregated-profile
    (read-aggregated-profile-entries profile-ptr profile-len name-table))

  (match status-code
    [0
     (define discs (machine-wrapper-discs machine))
     (define result
       (list->vector (for/list ([bf (in-vector outs)]
                                [disc (in-list discs)])
                       ((discretization-convert disc) bf))))
     (values result bumps iterations aggregated-profile)]
    [-1 (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt aggregated-profile))]
    [-2 (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt aggregated-profile))]
    [-99 (error 'rival-apply "Rival panic")]
    [else (error 'rival-apply "Unknown result code: ~a" status-code)]))

(define (rival-apply-batch machine pts hints-vec [max-iterations (*rival-max-iterations*)])
  (define batch-size (vector-length pts))
  (define n-args (if (> batch-size 0) (vector-length (vector-ref pts 0)) 0))
  (define discs (machine-wrapper-discs machine))
  (define n-outs (length discs))

  (ensure-batch-buffers! machine batch-size)
  (define outs-vec (ensure-batch-outs-cache! machine batch-size))

  (define batch-arg-buf (unbox (machine-wrapper-batch-arg-buf-box machine)))
  (for ([pt-idx (in-range batch-size)])
    (define pt (vector-ref pts pt-idx))
    (for ([arg-idx (in-range n-args)])
      (ptr-set! batch-arg-buf _pointer (+ (* pt-idx n-args) arg-idx) (vector-ref pt arg-idx))))

  (define batch-out-buf (unbox (machine-wrapper-batch-out-buf-box machine)))
  (for ([pt-idx (in-range batch-size)])
    (define outs (vector-ref outs-vec pt-idx))
    (for ([out-idx (in-range n-outs)])
      (ptr-set! batch-out-buf _pointer (+ (* pt-idx n-outs) out-idx) (vector-ref outs out-idx))))

  (define batch-hints-buf (unbox (machine-wrapper-batch-hints-buf-box machine)))
  (for ([pt-idx (in-range batch-size)])
    (define h (and hints-vec (vector-ref hints-vec pt-idx)))
    (ptr-set! batch-hints-buf _pointer pt-idx (if h (hints-wrapper-ptr h) #f)))

  (match-define (list num-results status-ptr total-bumps total-iters profile-ptr profile-len)
    (rival_apply_batch (machine-wrapper-ptr machine)
                       batch-size
                       batch-arg-buf n-args
                       batch-out-buf n-outs
                       batch-hints-buf
                       max-iterations
                       (*rival-max-precision*)))

  ;; Don't let the gc go to town
  (void (vector-length pts))
  (void (and hints-vec (vector-length hints-vec)))

  (define statuses
    (for/vector #:length batch-size ([i (in-range batch-size)])
      (ptr-ref status-ptr _int32 i)))

  (define name-table (machine-wrapper-name-table machine))
  (define aggregated-profile
    (read-aggregated-profile-entries profile-ptr profile-len name-table))

  (define results
    (for/vector #:length batch-size ([pt-idx (in-range batch-size)])
      (define status (vector-ref statuses pt-idx))
      (define outs-bf (vector-ref outs-vec pt-idx))
      (cond
        [(= status 0)
         (define vals
           (for/list ([bf (in-vector outs-bf)]
                      [disc (in-list discs)])
             ((discretization-convert disc) bf)))
         (cons 'valid vals)]
        [(= status -1) (cons 'invalid #f)]
        [(= status -2) (cons 'exit #f)]
        [else (cons 'panic #f)])))

  (values results total-bumps total-iters aggregated-profile))

(define (rival-analyze-with-hints machine rect [hint #f])
  (define n-args (vector-length rect))
  (define rect-ptrs (machine-wrapper-rect-buf machine))
  (for ([i (in-range n-args)]
        [iv (in-vector rect)])
    (ptr-set! rect-ptrs _pointer (* 2 i) (ival-lo iv))
    (ptr-set! rect-ptrs _pointer (+ (* 2 i) 1) (ival-hi iv)))

  (define hint-ptr
    (if hint
        (hints-wrapper-ptr hint)
        #f))

  (match-define (list status-code is-error maybe-error converged hints-ptr)
    (rival_analyze_with_hints machine rect-ptrs n-args hint-ptr))

  (when (= status-code -1)
    (error 'rival-analyze-with-hints "Rival panic"))

  ;; Wrap the hints pointer so it can be passed back later
  (define new-hints
    (if hints-ptr
        (let ([wrapper (hints-wrapper hints-ptr)])
          (register-finalizer wrapper hints-destroy)
          wrapper)
        #f))

  (list (ival is-error maybe-error) new-hints converged))

(define (rival-profile machine param)
  (match-define (list instructions-len iterations bumps executions-ptr executions-len)
    (rival_profile machine param))
  (match param
    ['executions
     (define name-table (machine-wrapper-name-table machine))
     (for/vector #:length executions-len
                 ([i (in-range executions-len)])
       (read-execution-at executions-ptr i name-table))]
    ['instructions instructions-len]
    ['iterations iterations]
    ['bumps bumps]))

;; Read execution record using pre-cached name table (avoids FFI string overhead)
(define (read-execution-at base-ptr i name-table)
  (define p (ptr-add base-ptr (* i execution-record-size)))
  (define number (ptr-ref (ptr-add p 16) _int32))
  ;; For regular instructions (number >= 0), use cached name table
  ;; For special records (number = -1), fall back to reading name from FFI
  (define name
    (if (>= number 0)
        (vector-ref name-table number) ; Fast vector lookup!
        'adjust))
  (execution name
             number
             (ptr-ref (ptr-add p 20) _uint32) ; precision
             (ptr-ref (ptr-add p 24) _double) ; time_ms
             0 ; memory
             (ptr-ref (ptr-add p 32) _size))) ; iteration

;; Returns vector of (list name precision time memory)
(define (read-aggregated-profile-entries base-ptr len name-table)
  (if (or (not base-ptr) (= len 0))
      (vector)
      (for/vector #:length len ([i (in-range len)])
        (define p (ptr-add base-ptr (* i aggregated-entry-size)))
        (define idx (ptr-ref p _int32))
        (define name (if (>= idx 0) (vector-ref name-table idx) 'adjust))
        (list name
              (ptr-ref (ptr-add p 4) _uint32)   ; precision_bucket
              (ptr-ref (ptr-add p 8) _double)   ; total_time_ms
              (ptr-ref (ptr-add p 16) _size))))) ; total_memory

(define (hints-destroy wrapper)
  (when (hints-wrapper-ptr wrapper)
    (rival_hints_destroy (hints-wrapper-ptr wrapper))))

(define (machine-destroy wrapper)
  (rival_destroy (machine-wrapper-ptr wrapper))
  ;; Only free buffers that were actually allocated (non-#f in box)
  (define (free-if-allocated box)
    (define buf (unbox box))
    (when buf (free buf)))
  (free-if-allocated (machine-wrapper-arg-buf-box wrapper))
  (free-if-allocated (machine-wrapper-out-buf-box wrapper))
  (free-if-allocated (machine-wrapper-rect-buf-box wrapper))
  (free-if-allocated (machine-wrapper-batch-arg-buf-box wrapper))
  (free-if-allocated (machine-wrapper-batch-out-buf-box wrapper))
  (free-if-allocated (machine-wrapper-batch-hints-buf-box wrapper)))
