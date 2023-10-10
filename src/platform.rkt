#lang racket

(require "syntax/types.rkt" "syntax/syntax.rkt")
(provide *active-platform* activate-platform!)

(module+ internals
  (provide define-platform register-platform!))

;;; Platforms specify what representations, operators, and constants Herbie
;;; is allowed to produce in its output. Platforms provide no implementation
;;; of floating-point operations and will verify that implementations
;;; are loaded. Unlike plugins, only one platform may be active at any
;;; given time and platforms may be activated or deactivated.
(struct platform (reprs ops))

;; Platform table, mapping name to platform
(define platforms (make-hash))

;; Active platform
(define *active-platform* (make-parameter 'default))

;; Loads a platform.
(define (activate-platform! name)
  (unless (hash-has-key? platforms name)
    (error 'register-platform! "unknown platform ~a, found ~a" name
           (string-join (map ~a (hash-keys platforms)) ", ")))
  (define pform (hash-ref platforms name))
  (printf "Activating platform `~a`\n" name)

  (clear-active-operator-impls!)
  (for ([impl (in-list (platform-ops pform))])
    (activate-operator-impl! impl)))

;; Registers a platform: `repr-data` is a dictionary
;; mapping a representation name to a list of operators
;; and their types signatures (argument representations)
(define (register-platform! name repr-data)
  (when (hash-has-key? platforms name)
    (error 'register-platform! "platform already registered ~a" name))
  (define reprs
    (for/list ([(repr-name _) (in-dict repr-data)])
       (get-representation repr-name)))
  (define ops
    (for/fold ([ops '()]) ([(repr-name op-data) (in-dict repr-data)])
      (define repr (get-representation repr-name))
      (for/fold ([ops ops]) ([op (in-list op-data)])
        (match op
          [(list name)
           (define impl (get-parametric-constant name repr #:all? #t))
           (cons impl ops)]
          [(list name itypes ...)
           (define ireprs (map get-representation itypes))
           (define impl (apply get-parametric-operator name ireprs #:all? #t))
           (cons impl ops)]))))
  (define pform (platform reprs ops))
  (hash-set! platforms name pform))

;; Macro version of `register-platform!`.
(define-syntax define-platform
  (syntax-rules ()
    [(_ name [repr-name (conv-targets ...) [op itypes ...] ...] ...)
     (begin
       (define repr-data
         (for/list ([repr '(repr-name ...)]
                    [convs-to '((conv-targets ...) ...)]
                    [op-data '(((op itypes ...) ...) ...)])
           (define conversions
             (for/list ([conv-to convs-to])
               (list 'cast conv-to)))
           (cons repr (append conversions op-data))))
       (register-platform! 'name repr-data))]))
