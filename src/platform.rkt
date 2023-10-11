#lang racket

(require (for-syntax racket/match))
(require "common.rkt" "syntax/types.rkt" "syntax/syntax.rkt"
         (submod "syntax/syntax.rkt" internals))

(provide platform? platform-reprs platform-operators
         (rename-out [platform-ops platform-operator-impls])
         platform-conversions get-platform
         *active-platform* activate-platform!)

(module+ internals
  (provide define-platform register-platform!))

;; Representaiton name sanitizer
(define (repr->symbol repr)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (string->symbol (string-replace* (~a (representation-name repr)) replace-table)))

;;; Platforms specify what representations, operators, and constants Herbie
;;; is allowed to produce in its output. Platforms provide no implementation
;;; of floating-point operations and will verify that implementations
;;; are loaded. Unlike plugins, only one platform may be active at any
;;; given time and platforms may be activated or deactivated.
(struct platform (name reprs ops))

;; Platform table, mapping name to platform
(define platforms (make-hash))

;; Active platform
(define *active-platform* (make-parameter #f))

;; Looks up a platform by identifier.
;; Panics if no platform is found.
(define (get-platform name)
  (hash-ref platforms name
            (λ ()
              (error 'get-platform "unknown platform ~a, found ~a"
                     name (string-join (map ~a (hash-keys platforms)) ", ")))))

;; Loads a platform.
(define/contract (activate-platform! pform)
  (-> platform? void?)
  (printf "Activating platform `~a`\n" (platform-name pform))

  ; replace the active operator table
  (clear-active-operator-impls!)
  (for ([impl (in-list (platform-ops pform))])
    (activate-operator-impl! impl)))

;; Real operators in a platform.
(define/contract (platform-operators pform)
  (-> platform? (listof symbol?))
  (define ops (mutable-set))
  (for ([impl (in-list (platform-ops pform))])
    (set-add! ops (impl->operator impl)))
  (set->list ops))

;; Representation conversions in a platform.
(define/contract (platform-conversions pform)
  (-> platform? (listof any/c))
  (reap [sow]
    (for ([impl (in-list (platform-ops pform))])
      (when (eq? (impl->operator impl) 'cast)
        (sow impl)))))

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
          [(list 'cast itype)
           (define irepr (get-representation itype)) 
           (define impl
             (or (get-repr-conv irepr repr)
                 (generate-conversion-impl irepr repr)))
           (unless impl
             (error 'register-platform! "could not generate conversion ~a => ~a"
                    (format "<~a>" (representation-name irepr))
                    (format "<~a>" (representation-name repr))))
           (unless (get-rewrite-operator repr)
             ; need to make a "precision rewrite" operator
             ; (only if we did not generate it before)
             (define rewrite-name (sym-append '<- (repr->symbol repr)))
             (register-operator-impl! 'convert rewrite-name (list repr) repr
                (list (cons 'fl identity))))
           (cons impl ops)]
          [(list name itypes ...)
           (define ireprs (map get-representation itypes))
           (define impl (apply get-parametric-operator name ireprs #:all? #t))
           (cons impl ops)]))))
  (define pform (platform name reprs ops))
  (hash-set! platforms name pform))

;; Macro version of `register-platform!`
;; 
;; Example usage:
;; ```
;; (define-platform default
;;   [bool ()                   ; no conversions
;;    #:const [TRUE]            ; keyword declaration
;;    [FALSE]                   ; non-keyword declaration
;;    #:1ary [not]              ; keyword declaration (e.g., not : bool -> bool)
;;    #:2ary [and or]           ; keyword declaration (e.g., and : bool x bool -> bool)
;;    #:2ary binary64 [== > <]  ; keyword declaration (e.g., == : binary64 x binary64 -> bool)
;;    [>= binary64 binary64]    ; non-keyword declaration
;;    [<= binary64 binary64]    ; non-keyword declaration
;;   ]
;;   ...
;; )
;; ```
(define-syntax (define-platform stx)
  (syntax-case stx ()
    [(_ name e1 es ...)
     (let loop ([entries (syntax->list #'(e1 es ...))]
                [repr-names '()]
                [convs/repr '()]
                [ops/repr '()])
       (match entries
         [(list)
          (with-syntax ([(repr-names ...) repr-names]
                        [(convs/repr ...) convs/repr]
                        [(ops/repr ...) ops/repr])
          #'(begin
              (define repr-data
                (for/list ([repr '(repr-names ...)]
                           [convs-to '(convs/repr ...)]
                           [op-data '(ops/repr ...)])
                  (define conversions
                    (for/list ([conv-to convs-to])
                      (list 'cast conv-to)))
                  (cons repr (append conversions op-data))))
              (register-platform! 'name repr-data)))]
         [(list entry rest ...)
          (syntax-case entry ()
            [(name (conv-targets ...) op-clauses ...)
             (begin
               (define targets (syntax->list #'(conv-targets ...)))
               (define ops
                 (let loop ([clauses #'(op-clauses ...)] [done '()])
                   (syntax-case clauses ()
                     [() done]
                     [(#:const [ops ...] rest ...)
                      (loop #'(rest ...) (append (syntax->list #'((ops) ...)) done))]
                     [(#:1ary [ops ...] rest ...)
                      (loop #'(#:1ary name [ops ...] rest ...) done)]
                     [(#:2ary [ops ...] rest ...)
                      (loop #'(#:2ary name [ops ...] rest ...) done)]
                     [(#:3ary [ops ...] rest ...)
                      (loop #'(#:3ary name [ops ...] rest ...) done)]
                     [(#:4ary [ops ...] rest ...)
                      (loop #'(#:4ary name [ops ...] rest ...) done)]
                     [(#:1ary itype [ops ...] rest ...)
                      (with-syntax ([(itypes ...) (build-list 1 (λ (_) #'itype))])
                        (loop #'(rest ...) (append (syntax->list #'((ops itypes ...) ...)) done)))]
                     [(#:2ary itype [ops ...] rest ...)
                      (with-syntax ([(itypes ...) (build-list 2 (λ (_) #'itype))])
                        (loop #'(rest ...) (append (syntax->list #'((ops itypes ...) ...)) done)))]
                     [(#:3ary itype [ops ...] rest ...)
                      (with-syntax ([(itypes ...) (build-list 3 (λ (_) #'itype))])
                        (loop #'(rest ...) (append (syntax->list #'((ops itypes ...) ...)) done)))]
                     [(#:4ary itype [ops ...] rest ...)
                      (with-syntax ([(itypes ...) (build-list 4 (λ (_) #'itype))])
                        (loop #'(rest ...) (append (syntax->list #'((ops itypes ...) ...)) done)))]
                     [([op itypes ...] rest ...)
                      (loop #'(rest ...) (cons #'(op itypes ...) done))]
                     [_
                      (raise-syntax-error 'define-platform "malformed operator" stx clauses)])))
               (loop rest
                     (cons (syntax->datum #'name) repr-names)
                     (cons targets convs/repr)
                     (cons ops ops/repr)))]
            [_
             (raise-syntax-error 'define-platform "malformed entry" stx entry)])]))]
    [(_ _)
     (raise-syntax-error 'define-platform "empty platform" stx)]))

;; This is a simpler implementation of `define-platform` that
;; does not support keywords
;; Macro version of `register-platform!`.
; (define-syntax define-platform
;   (syntax-rules ()
;     [(_ name [repr-name (conv-targets ...) [op itypes ...] ...] ...)
;      (begin
;        (define repr-data
;          (for/list ([repr '(repr-name ...)]
;                     [convs-to '((conv-targets ...) ...)]
;                     [op-data '(((op itypes ...) ...) ...)])
;            (define conversions
;              (for/list ([conv-to convs-to])
;                (list 'cast conv-to)))
;            (cons repr (append conversions op-data))))
;        (register-platform! 'name repr-data))]))
