#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
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
              (_fun _pointer ; machine ptr
                    _size ; batch_size
                    _f64vector ; args_data (flattened f64 array)
                    _size ; args_len (per point)
                    _f64vector ; out_data (flattened f64 array)
                    _size ; out_len (per point)
                    _bytes ; hints_data
                    (_vector i _size) ; hints_offsets
                    (_vector i _size) ; hints_lens
                    _s32vector ; result_codes
                    _size ; max_iterations
                    ->
                    _int32))

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

(define (rival-apply-batch machine pts hints-list [max-iterations (*rival-max-iterations*)])
  (define batch-size (vector-length pts))
  (when (= batch-size 0)
    (error 'rival-apply-batch "Empty batch"))

  (define n-args (vector-length (vector-ref pts 0)))
  (define n-outs (length (machine-wrapper-discs machine)))

  ;; Prepare arguments as flattened f64vector (zero allocation beyond the array itself)
  (define args-data (make-f64vector (* batch-size n-args)))
  (for ([i (in-range batch-size)])
    (define pt (vector-ref pts i))
    (for ([j (in-range n-args)])
      (f64vector-set! args-data (+ (* i n-args) j) (vector-ref pt j))))

  ;; Prepare output buffer as flattened f64vector
  (define out-data (make-f64vector (* batch-size n-outs)))

  ;; Prepare hints (flattened into single bytes buffer)
  (define hints-bytes-list
    (for/list ([h (in-list hints-list)])
      (if h (serialize-hints-binary h) #"")))
  (define total-hints-len (apply + (map bytes-length hints-bytes-list)))
  (define hints-data (make-bytes total-hints-len))
  (define hints-offsets (make-vector batch-size))
  (define hints-lens (make-vector batch-size))
  (let loop ([i 0] [offset 0] [hbs hints-bytes-list])
    (unless (null? hbs)
      (define hb (car hbs))
      (define len (bytes-length hb))
      (bytes-copy! hints-data offset hb)
      (vector-set! hints-offsets i offset)
      (vector-set! hints-lens i len)
      (loop (add1 i) (+ offset len) (cdr hbs))))

  ;; Allocate result codes
  (define result-codes (make-s32vector batch-size))

  ;; Call batch function
  (define overall-status
    (rival_apply_batch (machine-wrapper-ptr machine)
                       batch-size
                       args-data
                       n-args
                       out-data
                       n-outs
                       hints-data
                       hints-offsets
                       hints-lens
                       result-codes
                       max-iterations))

  (when (= overall-status -99)
    (error 'rival-apply-batch "Rival panic"))

  ;; Extract results - outputs are already f64, just read them
  (define discs (machine-wrapper-discs machine))
  (for/vector ([i (in-range batch-size)])
    (define code (s32vector-ref result-codes i))
    (cond
      [(= code 0) ; Valid
       ;; Read outputs directly from f64vector, skip first (precondition)
       (define base-idx (* i n-outs))
       (define outs
         (for/vector ([j (in-range 1 n-outs)]) ; skip index 0 (precondition)
           (define raw-val (f64vector-ref out-data (+ base-idx j)))
           (define disc (list-ref discs j))
           ;; Apply discretization convert for booleans
           (if (= (discretization-type disc) 0) ; bool type
               (not (zero? raw-val))
               raw-val)))
       (cons 'valid outs)]
      [(= code -1) (cons 'invalid #f)]
      [(= code -2) (cons 'exit #f)]
      [else (cons 'unknown #f)])))

(define-rival rival_analyze_with_hints (_fun _pointer _string _bytes _size -> _pointer))

(define-rival rival_profile (_fun _pointer _string -> _pointer))

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

  (define ptr (rival_compile exprs-str vars-str target types-ptr num-types))

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
