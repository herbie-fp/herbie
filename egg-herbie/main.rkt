#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide egraph_create egraph_destroy egraph_add_expr
         egraph_run egraph_run_with_iter_limit
         egraph_get_stop_reason
         egraph_get_simplest egraph_get_variants
         _EGraphIter destroy_egraphiters egraph_get_cost
         egraph_is_unsound_detected egraph_get_times_applied
         egraph_get_proof destroy_string egraph_is_equal
         (struct-out EGraphIter)
         (struct-out FFIRule))

(define-runtime-path libeggmath-path
  (build-path "target/release"
              (string-append
               (case (system-type)
                 [(windows) "egg_math"]
                 [else "libegg_math"])
               (bytes->string/utf-8 (system-type 'so-suffix)))))

; Checks if Racket is being run in emulation via rosetta
(define (running-on-rosetta?)
  (and (equal? (system-type 'os) 'macosx)
       (equal? (system-type 'arch) 'x86_64)
       (equal? (with-output-to-string (lambda () ; returns true if running in emulation
                                        (system "sysctl -n sysctl.proc_translated"))) "1\n")))

; Note, this message should not be reached.
(define fallback-message
  (string-join
   `("Error: unable to load the 'egg-math' library"
     "Please file a bug at https://github.com/herbie-fp/herbie/issues")
   "\n"))

; Note, refering to ARM as Apple Silicon to match Racket download page.
(define rosetta-message
  (string-join
   '("Error: You are running the 'x86' version of Racket via 'Rosetta'"
     "emulation. To run Herbie, you need to use the 'Apple Silicon'"
     "version of Racket."
     "You can install it here: https://download.racket-lang.org")
   "\n"))

(define (handle-eggmath-import-failure)
  (define error-message
    (if (running-on-rosetta?)
        rosetta-message
        fallback-message))
  (raise-user-error error-message))

; Checks for a condition on MacOS if x86 Racket is being used on an ARM mac.
(define-ffi-definer define-eggmath
  (ffi-lib  libeggmath-path #:fail handle-eggmath-import-failure))

; GC'able egraph
; If Racket GC can prove unreachable, `egraph_destroy` will be called
(define _egraph-pointer
  (_cpointer 'egraph #f #f
             (lambda (p)
               (register-finalizer p egraph_destroy)
               p)))

; Egraph iteration data
; Not managed by Racket GC.
; Must call `destroy_egraphiters` to free.
(define-cstruct _EGraphIter
  ([numnodes _uint]
   [numeclasses _uint]
   [time _double])
  #:malloc-mode 'raw)

; Rewrite rule
; Not managed by Racket GC.
; Must call `free` on struct and fields
(define-cstruct _FFIRule
  ([name _pointer]
   [left _pointer]
   [right _pointer])
  #:malloc-mode 'raw)

;;  -> a pointer to an egraph
(define-eggmath egraph_create (_fun -> _egraph-pointer))

(define-eggmath egraph_destroy (_fun _egraph-pointer -> _void))

(define-eggmath destroy_string (_fun _pointer -> _void))

;; egraph pointer, s-expr string -> node number
(define-eggmath egraph_add_expr (_fun _egraph-pointer _string/utf-8 -> _uint))

(define-eggmath destroy_egraphiters (_fun _pointer -> _void))

(define-eggmath egraph_is_unsound_detected (_fun _egraph-pointer -> _bool))

(define-eggmath egraph_run_with_iter_limit
  (_fun _egraph-pointer                           ;; egraph
        (ffi-rules : (_list i _FFIRule-pointer))  ;; ffi rules
        (_uint = (length ffi-rules))              ;; number of rules
        (iterations-length : (_ptr o _uint))      ;; pointer to length of resulting array
        (iterations-ptr : (_ptr o _pointer))      ;; pointer to array allocation, caller frees
        _uint                                     ;; iter limit
        _uint                                     ;; node limit
        _bool                                     ;; constant folding enabled?
        -> (iterations : _EGraphIter-pointer)
        -> (values iterations iterations-length iterations-ptr)))

(define-eggmath egraph_run
  (_fun _egraph-pointer                           ;; egraph
        (ffi-rules : (_list i _FFIRule-pointer))  ;; ffi rules
        (_uint = (length ffi-rules))              ;; number of rules
        (iterations-length : (_ptr o _uint))      ;; pointer to size of resulting array
        (iterations-ptr : (_ptr o _pointer))      ;; pointer to array allocation, caller frees
        _uint                                     ;; node limit
        _bool                                     ;; constant folding enabled?
        -> (iterations : _EGraphIter-pointer)
        -> (values iterations iterations-length iterations-ptr)))

;; gets the stop reason as an integer
(define-eggmath egraph_get_stop_reason (_fun _egraph-pointer -> _uint))

;; node number -> s-expr string
(define-eggmath egraph_get_simplest (_fun _egraph-pointer
                                          _uint ;; node id
                                          _uint ;; iteration
                                          -> _pointer))

(define-eggmath egraph_get_proof (_fun _egraph-pointer
                                       _string/utf-8
                                       _string/utf-8
                                       -> _pointer))

(define-eggmath egraph_is_equal (_fun _egraph-pointer
                                      _string/utf-8
                                      _string/utf-8
                                      -> _bool))

;; node number -> (s-expr string) string
(define-eggmath egraph_get_variants (_fun _egraph-pointer
                                          _uint           ;; node id
                                          _string/utf-8   ;; original expr
                                          -> _pointer))   ;; string pointer

(define-eggmath egraph_get_cost (_fun _egraph-pointer
                                      _uint ;; node id
                                      _uint ;; iteration
                                      -> _uint))

(define-eggmath egraph_get_times_applied (_fun _egraph-pointer
                                               _pointer ;; name of the rule
                                               -> _uint))

