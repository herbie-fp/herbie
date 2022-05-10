#lang racket

(require ffi/unsafe
         ffi/unsafe/define)
(require racket/runtime-path)

(provide egraph_create egraph_destroy egraph_add_expr
         egraph_addresult_destroy egraph_run egraph_get_simplest
         _EGraphIter destroy_egraphiters egraph_get_cost
         egraph_is_unsound_detected egraph_get_times_applied
         destroy_string
         (struct-out EGraphAddResult)
         (struct-out EGraphIter)
         (struct-out FFIRule))

(define-runtime-path libeggmath-path
  (build-path "target/release"
              (string-append
                (case (system-type)
                 [(windows) "egg_math"]
                 [else "libegg_math"])
                (bytes->string/utf-8 (system-type 'so-suffix)))))

(define-ffi-definer define-eggmath (ffi-lib libeggmath-path))

(define _egraph-pointer (_cpointer 'egraph))

(define-cstruct _EGraphAddResult
  ([id _uint]
   [successp _bool]))

(define-cstruct _EGraphIter
  ([numnodes _uint]
   [numeclasses _uint]
   [time _double]))

(define-cstruct _FFIRule ; The pointers are pointers to strings, but types hidden for allocation
  ([name _pointer]
   [left _pointer]
   [right _pointer])
  #:malloc-mode 'raw)


;;  -> a pointer to an egraph
(define-eggmath egraph_create (_fun -> _egraph-pointer))

(define-eggmath egraph_destroy (_fun _egraph-pointer -> _void))

(define-eggmath destroy_string (_fun _pointer -> _void))

;; egraph pointer, s-expr string -> node number
(define-eggmath egraph_add_expr (_fun _egraph-pointer _string/utf-8 -> _EGraphAddResult-pointer))

(define-eggmath egraph_addresult_destroy (_fun _EGraphAddResult-pointer -> _void))

(define-eggmath destroy_egraphiters (_fun _uint _EGraphIter-pointer -> _void))


(define-eggmath egraph_is_unsound_detected (_fun _egraph-pointer -> _bool))

(define-eggmath egraph_run (_fun _egraph-pointer
                                 (len : (_ptr o _uint)) ;; pointer to size of resulting array
                                 _uint ;; node limit
                                (ffi-rules : (_list i _FFIRule-pointer)) ;; ffi rules
                                _bool ;; constant folding enabled?
                                (_uint = (length ffi-rules))
                                -> (iters : _EGraphIter-pointer)
                                -> (values iters len)))

;; node number -> s-expr string
(define-eggmath egraph_get_simplest (_fun _egraph-pointer
                                         _uint ;; node id
                                         _uint ;; iteration
                                         -> _pointer)) ;; string pointer

(define-eggmath egraph_get_cost (_fun _egraph-pointer
                                     _uint ;; node id
                                     _uint ;; iteration
                                     -> _uint))

(define-eggmath egraph_get_times_applied (_fun _egraph-pointer
                                               _pointer ;; name of the rule
                                               -> _uint))
                                               