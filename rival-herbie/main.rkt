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
         rival-hints?
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

;; Analyze result layout: status_code, is_error, maybe_error, converged, hints
(define _analyze-result (_list-struct _int32 _stdbool _stdbool _stdbool _pointer))

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
              (_fun _pointer _pointer _size _pointer _size _pointer _size _uint32 -> _int32))

(define-rival rival_analyze_with_hints (_fun _pointer _pointer _size _pointer -> _analyze-result))

(define-rival rival_profile (_fun _pointer _profile-field -> _profile-data))

(define-rival rival_instruction_names
              (_fun _pointer (len : (_ptr o _size)) -> (ptr : _pointer) -> (values ptr len)))

;; Wrapper struct for machine
(struct machine-wrapper (ptr discs arg-buf out-buf rect-buf name-table)
  #:property prop:cpointer (struct-field-index ptr))

;; Wrapper struct for hints
(struct hints-wrapper (ptr) #:property prop:cpointer (struct-field-index ptr))

(define rival-machine? machine-wrapper?)
(define rival-hints? hints-wrapper?)

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
        (define arg-buf (malloc _pointer n-args 'raw))
        (define out-buf (malloc _pointer n-outs 'raw))
        (define rect-buf (malloc _pointer (* 2 n-args) 'raw))
        ;; Fetch instruction names once and cache as vector of symbols
        (define-values (names-ptr names-len) (rival_instruction_names ptr))
        (define names-bytes (make-bytes names-len))
        (memcpy names-bytes names-ptr names-len)
        (define name-table
          (list->vector
            (map string->symbol
                 (string-split (bytes->string/utf-8 names-bytes) "\0"))))
        (define wrapper (machine-wrapper ptr discs arg-buf out-buf rect-buf name-table))
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

  (define res-code
    (rival_apply (machine-wrapper-ptr machine)
                 arg-ptrs
                 n-args
                 out-ptrs
                 n-outs
                 hints-ptr
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

(define (hints-destroy wrapper)
  (when (hints-wrapper-ptr wrapper)
    (rival_hints_destroy (hints-wrapper-ptr wrapper))))

(define (machine-destroy wrapper)
  (rival_destroy (machine-wrapper-ptr wrapper))
  (free (machine-wrapper-arg-buf wrapper))
  (free (machine-wrapper-out-buf wrapper))
  (free (machine-wrapper-rect-buf wrapper)))
