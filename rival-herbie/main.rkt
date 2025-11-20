#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide rival_make_context
         rival_destroy
         rival_eval
         rival_analyze
         rival_free_string
         _convert_cb
         _distance_cb
         _free_cb
         c-free
         string->c-pointer
         (struct-out ival))

(struct ival (lo hi) #:transparent)

(define c-free (get-ffi-obj "free" #f (_fun _pointer -> _void)))

(define (string->c-pointer s)
  (define b (string->bytes/utf-8 s))
  (define n (bytes-length b))
  (define p (malloc (add1 n) 'raw))
  (memcpy p b n)
  (ptr-set! p _byte n 0)
  p)

(define-runtime-path librival-path
                     (build-path "target/release"
                                 (string-append (case (system-type)
                                                  [(windows) "rival_herbie"]
                                                  [else "librival_herbie"])
                                                (bytes->string/utf-8 (system-type 'so-suffix)))))

(define-ffi-definer define-rival (ffi-lib librival-path))

;; Define callbacks
(define _convert_cb (_fun _size _string -> _pointer)) ; Returns pointer to string that must be freed
(define _distance_cb (_fun _size _string _string -> _size))
(define _free_cb (_fun _pointer -> _void))

(define-rival rival_make_context
              (_fun _string _string _bool _uint32 _convert_cb _distance_cb _free_cb -> _pointer))

(define-rival rival_destroy (_fun _pointer -> _void))

(define-rival rival_eval (_fun _pointer _string _string -> _pointer))

(define-rival rival_analyze (_fun _pointer _string _string -> _pointer))

(define-rival rival_free_string (_fun _pointer -> _void))
