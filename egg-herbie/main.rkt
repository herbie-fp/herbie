#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         racket/runtime-path)

(provide egraph_create
         egraph_destroy
         egraph_add_expr
         egraph_add_node
         egraph_run
         egraph_copy
         egraph_get_stop_reason
         egraph_find
         egraph_serialize
         egraph_get_eclasses
         in-egraph-enodes
         egraph_get_simplest
         egraph_get_variants
         egraph_get_cost
         egraph_is_unsound_detected
         egraph_get_times_applied
         egraph_get_proof
         (struct-out iteration-data)
         (struct-out FFIRule)
         make-ffi-rule)

(define-runtime-path libeggmath-path
                     (build-path "target/release"
                                 (string-append (case (system-type)
                                                  [(windows) "egg_math"]
                                                  [else "libegg_math"])
                                                (bytes->string/utf-8 (system-type 'so-suffix)))))

; Checks if Racket is being run in emulation via rosetta
(define (running-on-rosetta?)
  (and (equal? (system-type 'os) 'macosx)
       (equal? (system-type 'arch) 'x86_64)
       (equal? (with-output-to-string (lambda () ; returns true if running in emulation
                                        (system "sysctl -n sysctl.proc_translated")))
               "1\n")))

; Note, this message should not be reached.
(define fallback-message
  (string-join `("Error: unable to load the 'egg-math' library"
                 "Please file a bug at https://github.com/herbie-fp/herbie/issues")
               "\n"))

; Note, refering to ARM as Apple Silicon to match Racket download page.
(define rosetta-message
  (string-join '("Error: You are running the 'x86' version of Racket via 'Rosetta'"
                 "emulation. To run Herbie, you need to use the 'Apple Silicon'"
                 "version of Racket."
                 "You can install it here: https://download.racket-lang.org")
               "\n"))

(define (handle-eggmath-import-failure)
  (define error-message (if (running-on-rosetta?) rosetta-message fallback-message))
  (raise-user-error error-message))

; Checks for a condition on MacOS if x86 Racket is being used on an ARM mac.
(define-ffi-definer define-eggmath (ffi-lib libeggmath-path #:fail handle-eggmath-import-failure))

;; Frees a Rust-allocated C-string
(define-eggmath destroy_string (_fun _pointer -> _void))

;; Gets the length of a Rust-allocated C-string in bytes,
;; excluding the nul terminator.
(define-eggmath string_length (_fun _pointer -> _uint32))

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
  (destroy_string p)
  (bytes->string/utf-8 bstr))

;; Converts a non-NULL, Rust-allocated C-string to a Racket datum
;; by repeatedly reading the string. The underlying Rust string
;; is automatically freed.
(define (_rust/string->data p)
  (define len (string_length p))
  (define bstr (make-bytes len))
  (memcpy bstr p len)
  (destroy_string p)
  (for/list ([datum (in-port read (open-input-bytes bstr))])
    datum))

;; FFI type that converts Rust-allocated C-style strings
;; to Racket strings, automatically freeing the Rust-side allocation.
(define _rust/string
  (make-ctype _pointer
              (lambda (x) (and x (string->_rust/string x)))
              (lambda (x) (and x (_rust/string->string x)))))

;; FFI type that converts Rust-allocated C-style strings
;; to a Racket datum via `read`, automatically freeing the Rust-side allocation.
(define _rust/datum
  (make-ctype _pointer
              (lambda (x) (and x (string->_rust/string (~a x))))
              (lambda (x) (and x (first (_rust/string->data x))))))

;; FFI type that converts Rust-allocated C-style strings
;; to multiple Racket datum via reapeated use of `read`,
;; automatically freeing the Rust-side allocation.
(define _rust/data
  (make-ctype _pointer
              (lambda (_) (error '_rust/data "cannot be used as an input type"))
              (lambda (x) (and x (_rust/string->data x)))))

; Egraph iteration data
; Not managed by Racket GC.
; Must call `destroy_egraphiters` to free.
(define-cstruct _EGraphIter ([numnodes _uint] [numeclasses _uint] [time _double]) #:malloc-mode 'raw)

;; Frees an array of _EgraphIter structs
(define-eggmath destroy_egraphiters (_fun _pointer -> _void))

;; Racket representation of `_EGraphIter`
(struct iteration-data (num-nodes num-eclasses time))

;; Rewrite rule that can be passed over the FFI boundary.
;; Must be manually freed.
(define-cstruct _FFIRule ([name _pointer] [left _pointer] [right _pointer]) #:malloc-mode 'raw)

;; Constructs for `_FFIRule` struct.
(define (make-ffi-rule name lhs rhs)
  (define name* (string->_rust/string (~a name) #:raw? #t))
  (define lhs* (string->_rust/string (~a lhs) #:raw? #t))
  (define rhs* (string->_rust/string (~a rhs) #:raw? #t))
  (define p (make-FFIRule name* lhs* rhs*))
  (register-finalizer p free-ffi-rule)
  p)

;; Frees a `_FFIRule` struct.
(define (free-ffi-rule rule)
  (free (FFIRule-name rule))
  (free (FFIRule-left rule))
  (free (FFIRule-right rule))
  (free rule))

; GC'able egraph
; If Racket GC can prove unreachable, `egraph_destroy` will be called
(define _egraph-pointer
  (_cpointer 'egraph
             #f
             #f
             (lambda (p)
               (register-finalizer p egraph_destroy)
               p)))

;; Constructs an e-graph instances.
(define-eggmath egraph_create (_fun -> _egraph-pointer))

;; Frees an e-graph instance.
(define-eggmath egraph_destroy (_fun _egraph-pointer -> _void))

;; Copies an e-graph instance.
(define-eggmath egraph_copy (_fun _egraph-pointer -> _egraph-pointer))

;; Adds an expression to the e-graph.
;; egraph -> expr -> id
(define-eggmath egraph_add_expr (_fun _egraph-pointer _rust/datum -> _uint))

; egraph -> string -> ids -> bool -> id
(define-eggmath egraph_add_node
                (_fun [p : _egraph-pointer] ; egraph
                      [f : _rust/string] ; enode op
                      [v : _u32vector] ; id vector
                      [_uint = (u32vector-length v)] ; id vector length
                      [is_root : _stdbool] ; root node?
                      ->
                      _uint))

(define-eggmath egraph_is_unsound_detected (_fun _egraph-pointer -> _stdbool))

;; Runs the egraph with a set of rules, returning the statistics of the run.
(define-eggmath egraph_run
                (_fun _egraph-pointer ;; egraph
                      (ffi-rules : (_list i _FFIRule-pointer)) ;; ffi rules
                      (_uint = (length ffi-rules)) ;; number of rules
                      (iterations-length : (_ptr o _uint)) ;; pointer to length of resulting array
                      (iterations-ptr :
                                      (_ptr o _pointer)) ;; pointer to array allocation, caller frees
                      _uint ;; iter limit
                      _uint ;; node limit
                      _stdbool ;; simple scheduler?
                      _stdbool ;; constant folding enabled?
                      ->
                      (iterations : _EGraphIter-pointer) ;; array of _EgraphIter structs
                      ->
                      (begin
                        (define iter-data
                          (for/list ([i (in-range iterations-length)])
                            (define ptr (ptr-add iterations i _EGraphIter))
                            (iteration-data (EGraphIter-numnodes ptr)
                                            (EGraphIter-numeclasses ptr)
                                            (EGraphIter-time ptr))))
                        (destroy_egraphiters iterations-ptr)
                        iter-data)))

;; gets the stop reason as an integer
(define-eggmath egraph_get_stop_reason (_fun _egraph-pointer -> _uint))

;; egraph -> string
(define-eggmath egraph_serialize (_fun _egraph-pointer -> _rust/datum))

;; egraph -> uint
(define-eggmath egraph_size (_fun _egraph-pointer -> _uint))

;; egraph -> id -> uint
(define-eggmath egraph_eclass_size (_fun _egraph-pointer _uint -> _uint))

;; egraph -> id -> idx -> uint
(define-eggmath egraph_enode_size (_fun _egraph-pointer _uint _uint -> _uint))

;; egraph -> u32vector
(define-eggmath
 egraph_get_eclasses
 (_fun [e : _egraph-pointer] [v : _u32vector = (make-u32vector (egraph_size e))] -> _void -> v))

;; egraph -> id -> u32 -> (or symbol? number? (cons symbol u32vector))
;; UNSAFE: `v` must be large enough to contain the child ids
(define-eggmath egraph_get_node
                (_fun [e : _egraph-pointer]
                      [id : _uint32]
                      [idx : _uint32]
                      [v : _u32vector]
                      ->
                      [f : _rust/string]
                      ->
                      (values (if (zero? (u32vector-length v))
                                  (or (string->number f) (string->symbol f))
                                  (string->symbol f))
                              v)))

;; This fairly long and ugly method is the core of our egg FFI.
;; It iterates over all enodes in the egraph, returning for each:
;;
;; - The eclass id. This is normalize to range over 1..n
;; - The operator name, which is a symbol or a number
;; - A u32vector of child eclass ids, which are also normalized.
;;
;; If an enode is an atom, like a symbol or a number, there aren't any
;; children. The same holds if it's a zero-argument operator; these
;; cases need to be disambiguated by the caller.
;;
;; This method is also extremely performance sensitive, so it is written
;; to maximize performance. This leads to a key constraint:
;;
;; - The u32vector of child eclass ids cannot escape the caller; it is
;;   invalidated with each iteration.
;;
;; The caller must be careful!

(define (in-egraph-enodes egg-ptr)
  (define eclass-ids (egraph_get_eclasses egg-ptr))
  (define max-id
    (for/fold ([current-max 0]) ([idx (in-range (u32vector-length eclass-ids))])
      (define egg-id (u32vector-ref eclass-ids idx))
      (max current-max egg-id)))
  (define egg-id->idx (make-u32vector (+ max-id 1)))
  (for ([idx (in-range (u32vector-length eclass-ids))])
    (u32vector-set! egg-id->idx (u32vector-ref eclass-ids idx) idx))
  (define num-eclasses (u32vector-length eclass-ids))

  (cond
    [(= num-eclasses 0) (in-range 0)]
    [else
     (define first-eclass (u32vector-ref eclass-ids 0))
     (define first-eclass-size (egraph_eclass_size egg-ptr first-eclass))

     (define 0-vec (make-u32vector 0))
     (define 1-vec (make-u32vector 1))
     (define 2-vec (make-u32vector 2))
     (define 3-vec (make-u32vector 3))

     (make-do-sequence
      (lambda ()
        (values (lambda (i)
                  (match-define (vector eclass-idx eclass-id eclass-size enode-idx) i)
                  (define node-size (egraph_enode_size egg-ptr eclass-id enode-idx))
                  (define vec
                    (match node-size
                      [0 0-vec]
                      [1 1-vec]
                      [2 2-vec]
                      [3 3-vec]
                      [n (make-u32vector n)]))
                  (define-values (op args) (egraph_get_node egg-ptr eclass-id enode-idx vec))
                  (for ([i (in-range (u32vector-length args))])
                    (define sub-eclass-id (u32vector-ref args i))
                    (u32vector-set! args i (u32vector-ref egg-id->idx sub-eclass-id)))
                  (values eclass-idx op args))
                (lambda (i)
                  (match-define (vector eclass-idx eclass-id eclass-size enode-idx) i)
                  (define enode-idx* (add1 enode-idx))
                  (cond
                    [(< enode-idx* eclass-size) (vector-set! i 3 enode-idx*)]
                    [else
                     (define eclass-idx* (add1 eclass-idx))
                     (vector-set! i 0 eclass-idx*)
                     (when (< eclass-idx* num-eclasses)
                       (define eclass-id* (u32vector-ref eclass-ids eclass-idx*))
                       (vector-set! i 1 eclass-id*)
                       (define eclass-size* (egraph_eclass_size egg-ptr eclass-id*))
                       (vector-set! i 2 eclass-size*)
                       (vector-set! i 3 0))])
                  i)
                (vector 0 first-eclass first-eclass-size 0)
                (lambda (i)
                  (match-define (vector eclass-idx eclass-id eclass-size enode-idx) i)
                  (< eclass-idx num-eclasses))
                #f
                #f)))]))

;; egraph -> id -> id
(define-eggmath egraph_find (_fun _egraph-pointer _uint -> _uint))

;; egraph -> id -> (listof expr)
(define-eggmath egraph_get_simplest
                (_fun _egraph-pointer
                      _uint ;; node id
                      _uint ;; iteration
                      ->
                      _rust/datum)) ;; expr

;; egraph -> id -> string -> (listof expr)
(define-eggmath egraph_get_variants
                (_fun _egraph-pointer
                      _uint ;; node id
                      _rust/datum ;; original expr
                      ->
                      _rust/data)) ;; listof expr

;; egraph -> string -> string -> string
;; TODO: in Herbie, we bail on converting the proof
;; if the string is too big. It would be more efficient
;; to bail here instead.
(define-eggmath egraph_get_proof
                (_fun _egraph-pointer ;; egraph
                      _rust/datum ;; expr1
                      _rust/datum ;; expr2
                      ->
                      _rust/string)) ;; string

(define-eggmath egraph_get_cost
                (_fun _egraph-pointer
                      _uint ;; node id
                      _uint ;; iteration
                      ->
                      _uint))

(define-eggmath egraph_get_times_applied
                (_fun _egraph-pointer
                      _pointer ;; name of the rule
                      ->
                      _uint))
