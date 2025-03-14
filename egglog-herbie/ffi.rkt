#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide egraph_create
         egraph_free
         egraph_run)

(define-runtime-path libegglog-path
  (build-path "target/release"
             (string-append
               (case (system-type)
                 [(windows) "egglog_herbie"]
                [else "libegglog_herbie"])
        (bytes->string/utf-8 (system-type 'so-suffix)))))

(define (handle-egglog-import-failure)
  (error 'eggmath "Failed to import eggmath"))

(define-ffi-definer define-eggmath
  (ffi-lib libegglog-path
           #:fail handle-egglog-import-failure))

;; Converts a Racket string to a C-style string.
(define (string->_rust/string s #:raw? [raw? #f])
  (define bstr (string->bytes/utf-8 s))
  (define n (bytes-length bstr))
  (define p (malloc (if raw? 'raw 'atomic) (add1 n)))
  (memcpy p bstr n)
  (ptr-set! p _byte n 0)
  p)

;; Converts a non-NULL, Rust-allocated C-string to a Racket string,
;; freeing the Rust string.
(define (_rust/string->string p)
  (define len (string_length p))
  (define bstr (make-bytes len))
  (memcpy bstr p len)
  (string_free p)
  (bytes->string/utf-8 bstr))

;; FFI type that converts Rust-allocated C-style strings
;; to Racket strings, automatically freeing the Rust-side allocation.
(define _rust/string
  (make-ctype _pointer
              (lambda (x) (and x (string->_rust/string x)))
              (lambda (x) (and x (_rust/string->string x)))))

;; E-graph instance managed by the Racket GC.
;; The GC will invoke the finalizer once it is no longer reachable.
(define _egraph-pointer
  (_cpointer 'egraph
             #f
             #f
             (lambda (p)
               (register-finalizer p egraph_free)
               p)))

;; Constructs an e-graph instances.
(define-eggmath egraph_create (_fun -> _egraph-pointer))

;; Frees an e-graph instance.
(define-eggmath egraph_free (_fun _egraph-pointer -> _void))

;; Runs an egglog program on an e-graph instance.
(define-eggmath egraph_run (_fun _egraph-pointer _rust/string -> _rust/string))

;; Gets the length of a Rust-allocated C-string in bytes,
;; excluding the nul terminator.
(define-eggmath string_length (_fun _pointer -> _uint32))

;; Frees a Rust-allocated C-string
(define-eggmath string_free (_fun _pointer -> _void))
