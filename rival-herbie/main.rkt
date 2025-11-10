#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         math/bigfloat
         racket/runtime-path)

(provide
  rival-compile
  rival-apply
  rival-profile
  rival-analyze-with-hints
  (struct-out execution)
  (struct-out discretization)
  boolean-discretization
  *rival-max-precision*
  *rival-max-iterations*
  *rival-profile-executions*
  exn:rival:invalid?
  exn:rival:unsamplable?)

(define-runtime-path librival-path
  (build-path "target/release"
              (string-append (case (system-type)
                               [(windows) "rival_herbie"]
                               [else "librival_herbie"])
                             (bytes->string/utf-8 (system-type 'so-suffix)))))

(define (running-on-rosetta?)
  (and (equal? (system-type 'os) 'macosx)
       (equal? (system-type 'arch) 'x86_64)
       (equal? (with-output-to-string (lambda () (system "sysctl -n sysctl.proc_translated")))
               "1\n")))

(define fallback-message
  (string-append "Error: unable to load the 'rival_herbie' library\n"
                 "Please file a bug at https://github.com/herbie-fp/herbie/issues"))

(define rosetta-message
  (string-append "Error: You are running the 'x86' version of Racket via 'Rosetta' emulation.\n"
                 "  Please use the 'Apple Silicon' version of Racket instead.\n"
                 "You can install it from https://download.racket-lang.org"))

(define (handle-rival-import-failure)
  (raise-user-error (if (running-on-rosetta?) rosetta-message fallback-message)))

(define-ffi-definer define-rival (ffi-lib librival-path #:fail handle-rival-import-failure))

;; CString helpers
(define-rival destroy_string (_fun _pointer -> _void))
(define-rival string_length (_fun _pointer -> _uint32))

(define (_rust/string->string p)
  (define len (string_length p))
  (define bstr (make-bytes len))
  (memcpy bstr p len)
  (destroy_string p)
  (bytes->string/utf-8 bstr))

(define _rust/string
  (make-ctype _pointer
              (lambda (x) (and x (string->bytes/utf-8 (~a x))))
              (lambda (x) (and x (_rust/string->string x)))))

;; Machine pointer type that is GC-aware; auto-destroy on finalization
(define _rival-pointer
  (_cpointer 'rival-machine
             #f
             #f
             (lambda (p)
               (register-finalizer p rival_destroy)
               p)))

(define-rival rival_compile (_fun _string      ;; vars sexpr (NUL-terminated UTF-8)
                                  _string      ;; exprs sexpr (list of exprs)
                                  _uint32      ;; precision (53 for binary64)
                                  ->
                                  _rival-pointer))
(define-rival rival_destroy (_fun _rival-pointer -> _void))
(define-rival rival_instruction_count (_fun _rival-pointer -> _uint32))
(define-rival rival_bumps (_fun _rival-pointer -> _uint32))
(define-rival rival_apply (_fun _rival-pointer
                                    _f64vector ;; point data
                                    _uint32    ;; length
                                    _uint32    ;; max iterations
                                    (out-len : (_ptr o _uint32))
                                    ->
                                    (out-ptr : _pointer)
                                    ->
                                    (values out-ptr out-len)))
(define-rival destroy_f64_array (_fun _pointer _uint32 -> _void))
(define-rival rival_last_iterations (_fun _rival-pointer -> _uint32))

;; Parameters to control Rival (mirrors usage in src/core/rival.rkt)
(define *rival-max-precision* (make-parameter 10000))
(define *rival-max-iterations* (make-parameter 5))
(define *rival-profile-executions* (make-parameter 1024))

;; Discretization compatibility shim (Racket-side only, not used by FFI)
(struct discretization (bits to-repr distance) #:prefab)
(define boolean-discretization (discretization 1 values (lambda (_1 _2) 0)))

;; Minimal profiling struct for compatibility
(struct execution (name precision time memory) #:prefab)

;; Exceptions (not raised by this shim, but defined for handlers)
(define-values (exn:rival:invalid? make-exn:rival:invalid)
  (let ()
    (define-struct (exn:rival:invalid exn:fail) ())
    (values exn:rival:invalid? (lambda (msg) (exn:rival:invalid msg (current-continuation-marks))))))
(define-values (exn:rival:unsamplable? make-exn:rival:unsamplable)
  (let ()
    (define-struct (exn:rival:unsamplable exn:fail) ())
    (values exn:rival:unsamplable? (lambda (msg) (exn:rival:unsamplable msg (current-continuation-marks))))))

;; Public API

(define (rival-compile exprs vars discs)
;   (eprintf "[rival-herbie] compile ~a vars ~a\n" exprs vars)
  (define precision (if (and (list? discs)
                             (pair? discs)
                             (discretization? (car discs))
                             (>= (discretization-bits (car discs)) 53))
                        53
                        53))
  ;; Convert exact Racket numbers to inexact f64
  (define (inexactify x)
    (cond
      [(number? x)
       (cond
         [(exact? x) (real->double-flonum x)]
         [else x])]
      [(pair? x) (cons (inexactify (car x)) (inexactify (cdr x)))]
      [(vector? x) (list->vector (map inexactify (vector->list x)))]
      [else x]))
  (define vars-str (~a `(,@vars)))
  (define exprs-str (~a `(,@(map inexactify exprs))))
  (rival_compile vars-str exprs-str precision))

(define (rival-apply machine pt* [hint #f])
;   (eprintf "[rival-herbie] apply on ~a\n" (vector->list pt*))
  ;; Convert bigfloats to f64 for FFI; this is a best-effort discretization
  (define f64s (list->f64vector
                (for/list ([v (in-vector pt*)])
                  (cond
                    [(bigfloat? v) (real->double-flonum (bigfloat->real v))]
                    [(real? v) (real->double-flonum v)]
                    [else 0.0]))))
  (define max-iters (*rival-max-iterations*))
  (define-values (out-ptr out-len)
    (rival_apply machine f64s (f64vector-length f64s) max-iters))
  (define len out-len)
  (define arr (make-f64vector len))
  (for ([i (in-range len)])
    (f64vector-set! arr i (ptr-ref out-ptr _double i)))
  (destroy_f64_array out-ptr out-len)
  ;; Return vector: first element corresponds to precondition result included in exprs
  (list->vector (f64vector->list arr)))

(define (rival-profile machine what)
  (case what
    [(instructions) (rival_instruction_count machine)]
    [(bumps) (rival_bumps machine)]
    [(executions) (vector)]
    [(iterations) (rival_last_iterations machine)]
    [else 0]))

;; Analyzer stub: returns a 4-value list matching Herbie's expectations
(define (rival-analyze-with-hints machine input-ranges [hint #f])
  ;; Return shape expected by search: (list (list ival err err?) hint* converged?)
  ;; Conservative defaults: no error, not converged, no useful hint
  (list (make-prefab-struct 'ival (list 'no #f)) #f #f))
