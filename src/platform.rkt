#lang racket

(require (for-syntax racket/match))
(require "common.rkt" "errors.rkt"
         "syntax/rules.rkt" "syntax/syntax.rkt" "syntax/types.rkt"
         (submod "syntax/syntax.rkt" internals))

(provide
  get-platform *active-platform* activate-platform!
  ;;; Platform API
  (contract-out
    [platform? (-> any/c boolean?)]
    [platform-reprs (-> platform? (listof representation?))]
    [platform-operators (-> platform? (listof symbol?))]
    [rename platform-impls platform-operator-impls (-> platform? (listof symbol?))]
    [platform-conversions (-> platform? (listof symbol?))]
    [platform-reprchange-rules (-> platform? (listof rule?))]
    [platform-union (-> platform? platform? ... platform?)]
    [platform-intersect (-> platform? platform? ... platform?)]
    [platform-subtract (-> platform? platform? ... platform?)]
    [make-platform-product (-> (listof (cons/c symbol? representation?))
                           (listof symbol?)
                           platform?)]))

(module+ internals
  (provide platform make-platform register-platform!
           platform-product make-platform-product
           platform-union platform-intersect platform-subtract))

;;; Platforms describe a set of representations, operator, and constants
;;; Herbie should use during its improvement loop. Platforms are just
;;; a "type signature" - they provide no implementations of floating-point
;;; operations (see plugins). During runtime, platforms will verify if
;;; every listed feature is actually loaded by Herbie and will panic if
;;; implemenations are missing. nlike plugins, only one platform may be
;;; active at any given time and platforms may be activated or deactivated.
;;;
;;; A small API is provided for platforms for querying the supported
;;; operators, operator implementations, and representation conversions.
(struct platform (name reprs impls)
        #:name $platform
        #:constructor-name create-platform
        #:methods gen:custom-write
        [(define (write-proc p port mode)
           (if (platform-name p)
               (fprintf port "#<platform:~a>" (platform-name p))
               (fprintf port "#<platform>")))])

;; Platform table, mapping name to platform
(define platforms (make-hash))

;; Active platform
(define *active-platform* (make-parameter #f))

;; Looks up a platform by identifier.
;; Panics if no platform is found.
(define (get-platform name)
  (or (hash-ref platforms name #f)
      (raise-herbie-error "unknown platform `~a`, found (~a)" name
                          (string-join (map ~a (hash-keys platforms)) ", "))))

;; Loads a platform.
(define (activate-platform! pform)
  (printf "Activating platform `~a`\n" (platform-name pform))
  ; replace the active operator table
  (clear-active-operator-impls!)
  (for ([impl (in-list (platform-impls pform))])
    (activate-operator-impl! impl)))

;; Registers a platform under identifier `name`.
(define (register-platform! name pform)
  (when (hash-has-key? platforms name)
    (error 'register-platform!
           "platform already registered ~a"
           name))
  (hash-set! platforms name
            (struct-copy $platform pform [name name])))

;; Representation name sanitizer
(define (repr->symbol repr)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define repr-name (representation-name repr))
  (string->symbol (string-replace* (~a repr-name) replace-table)))

;; Loading conversion-related implementations for `repr1 => repr2`.
(define (get-conversion-impls irepr orepr)
  (define impl
    (or (get-repr-conv irepr orepr #:all? #t)
        (generate-conversion-impl irepr orepr)))
  (unless impl
    (error 'load-conversion-impls! "could not generate conversion ~a => ~a"
           (format "<~a>" (representation-name irepr))
           (format "<~a>" (representation-name orepr))))
  (define rw-impl (get-rewrite-operator orepr #:all? #t))
  (unless rw-impl
    ; need to make a "precision rewrite" operator
    ; (only if we did not generate it before)
    (define rewrite-name (sym-append '<- (repr->symbol orepr)))
    (register-operator-impl! 'convert rewrite-name
      (list orepr) orepr (list (cons 'fl identity)))
    (set! rw-impl (get-rewrite-operator orepr #:all? #t)))
  (list impl rw-impl))

;; Constructor procedure for platforms.
;; Verifies that a platform is well-formed and
;; the platform will be supported by Herbie's runtime.
(define (make-platform repr-names convs-dict ops-data)
  ; load the representations
  (define reprs
    (for/list ([repr-name (in-list repr-names)])
      (get-representation repr-name)))
  ; load the conversions
  (define convs
    (for/fold ([convs '()]) ([(in out) (in-dict convs-dict)])
      (define irepr (get-representation in))
      (define orepr (get-representation out))
      (append (get-conversion-impls irepr orepr)
              (get-conversion-impls orepr irepr)
              convs)))
  ; load the operators
  (define ops
    (for/list ([op-data (in-list ops-data)])
      (match op-data
        [(list name otype)
         ; special case: constants
         (define orepr (get-representation otype))
         (get-parametric-constant name orepr #:all? #t)]
        [(list name _ itypes ...)
         (define ireprs (map get-representation itypes))
         (apply get-parametric-operator name ireprs #:all? #t)])))
  ; make the platform
  (create-platform #f reprs (append convs ops)))

;; Macro version of `make-platform`
;; 
;; Example usage:
;; ```
;; (define default
;    (platform
;;     ([binary64 binary32] ...)  ; conversions
;;     (bool
;;       #:const [TRUE]            ; keyword declaration
;;       [FALSE]                   ; non-keyword declaration
;;       #:1ary [not]              ; keyword declaration (e.g., not : bool -> bool)
;;       #:2ary [and or]           ; keyword declaration (e.g., and : bool x bool -> bool)
;;       #:2ary binary64 [== > <]  ; keyword declaration (e.g., == : binary64 x binary64 -> bool)
;;       [>= binary64 binary64]    ; non-keyword declaration
;;       [<= binary64 binary64]    ; non-keyword declaration
;;      )
;;     ...
;;   ))
;; ```
(define-syntax (platform stx)
  (syntax-case stx ()
    [(_ (cs ...) e1 es ...)
     (begin
       ;; iterate over `cs ...` to get conversions
       (define convs
         (let loop ([clauses (syntax->list #'(cs ...))] [convs '()])
           (match clauses
             [(list) convs]
             [(list entry rest ...)
              (syntax-case entry ()
                [(in out)
                 (loop rest (cons (cons #'in #'out) convs))]
                [_
                 (raise-syntax-error 'define-platform
                                     "malformed conversion clause"
                                     stx
                                     entry)])])))
       ;; iterate over `e1 es ...` to get operators
       (define-values (reprs ops)
         (let loop ([entries (syntax->list #'(e1 es ...))]
                    [reprs '()]
                    [ops/repr '()])
           (match entries
             [(list) (values reprs ops/repr)]
             [(list entry rest ...)
              (syntax-case entry ()
                [(name op-clauses ...)
                 (begin
                   (define ops
                     (let loop ([clauses #'(op-clauses ...)] [done '()])
                       (syntax-case clauses ()
                         [() done]
                         [(#:const [ops ...] rest ...)
                          (loop #'(rest ...) (append (syntax->list #'((ops name) ...)) done))]
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
                            (loop #'(rest ...) (append (syntax->list #'((ops name itypes ...) ...)) done)))]
                         [(#:2ary itype [ops ...] rest ...)
                          (with-syntax ([(itypes ...) (build-list 2 (λ (_) #'itype))])
                            (loop #'(rest ...) (append (syntax->list #'((ops name itypes ...) ...)) done)))]
                         [(#:3ary itype [ops ...] rest ...)
                          (with-syntax ([(itypes ...) (build-list 3 (λ (_) #'itype))])
                            (loop #'(rest ...) (append (syntax->list #'((ops name itypes ...) ...)) done)))]
                         [(#:4ary itype [ops ...] rest ...)
                          (with-syntax ([(itypes ...) (build-list 4 (λ (_) #'itype))])
                            (loop #'(rest ...) (append (syntax->list #'((ops name itypes ...) ...)) done)))]
                         [([op itypes ...] rest ...)
                          (loop #'(rest ...) (cons #'(op itypes ...) done))]
                         [_
                          (raise-syntax-error 'define-platform
                                              "malformed operator entry"
                                              stx clauses)])))
                   (loop rest
                         (cons (syntax->datum #'name) reprs)
                         (append ops ops/repr)))])]
                [_
                 (raise-syntax-error 'define-platform
                                     "malformed representation entry"
                                     stx #'entry)])))
       ;; compose everything into a `make-platform` call
       (with-syntax ([(repr-names ...) reprs]   
                     [(convs ...) convs]
                     [(ops ...) ops])
         #'(make-platform '(repr-names ...) '(convs ...) '(ops ...))))]
    [(_ _ _ _ ...)
     (raise-syntax-error 'define-platform "malformed conversions clause" stx)]
    [(_ _)
     (raise-syntax-error 'define-platform "missing conversions clause" stx)]
    [_
     (raise-syntax-error 'define-platform "bad syntax" stx)]))

;; Real operators in a platform.
(define (platform-operators pform)
  (define ops (mutable-set))
  (for ([impl (in-list (platform-impls pform))])
    (set-add! ops (impl->operator impl)))
  (set->list ops))

;; Representation conversions in a platform.
(define (platform-conversions pform)
  (reap [sow]
    (for ([impl (in-list (platform-impls pform))])
      (when (eq? (impl->operator impl) 'cast)
        (sow impl)))))

;; The "precision change" rules valid for a platform
(define (platform-reprchange-rules pform)
  ; by directionality
  (define unidir (mutable-set))
  (define bidir (mutable-set))

  ; conversion signatures
  (for ([impl (platform-conversions pform)])
    (match-define (list irepr) (operator-info impl 'itype))
    (define orepr (operator-info impl 'otype))
    (define sig (cons irepr orepr))
    (set-add! unidir sig)
    
    (define rev-sig (cons orepr irepr))
    (when (set-member? unidir rev-sig)
      (set-add! bidir rev-sig)))

  ; for each conversion, enable a rewrite
  (define rules
    (for/list ([(irepr orepr) (in-dict (set->list unidir))])
      ; names
      (define irepr-sym (repr->symbol irepr))
      (define orepr-sym (repr->symbol orepr))
      (define conv (sym-append irepr-sym '-> orepr-sym))
      (define change (sym-append '<- irepr-sym))
      (define rewrite-name (sym-append 'rewrite- orepr-sym '/ irepr-sym))
      ; rules
      (rule rewrite-name 'a `(,conv (,change a)) `((a . ,irepr)) orepr)))

  ; for each bidirectional conversion, enable a pair of simplification rewrites
  (for/fold ([rules rules]) ([(irepr orepr) (in-dict (set->list unidir))])
    ; names
    (define irepr-sym (repr->symbol irepr))
    (define orepr-sym (repr->symbol orepr))
    (define conv1 (sym-append irepr-sym '-> orepr-sym))
    (define conv2 (sym-append orepr-sym '-> irepr-sym))
    (define rw-name1 (sym-append 'rewrite- orepr-sym '/ irepr-sym '-simplify))
    (define rw-name2 (sym-append 'rewrite- irepr-sym '/ orepr-sym '-simplify))
    ; rules
    (define simplify1 (rule rw-name1 'a `(,conv1 (,conv2 a)) `((a . ,orepr)) orepr))
    (define simplify2 (rule rw-name2 'a `(,conv2 (,conv1 a)) `((a . ,irepr)) irepr))
    (append (list simplify1 simplify2) rules)))

;; Applier for set operations on platforms
(define ((make-set-applier set-fn) p1 . ps)
  (define reprs
    (apply set-fn
      (for/list ([p (in-list (cons p1 ps))])
        (list->set (platform-reprs p)))))
  (define impls
    (apply set-fn
      (for/list ([p (in-list (cons p1 ps))])
        (list->set (platform-impls p)))))
  (create-platform #f (set->list reprs) (set->list impls)))
  
;; Set operations on platforms
(define platform-union (make-set-applier set-union))
(define platform-intersect (make-set-applier set-intersect))
(define platform-subtract (make-set-applier set-subtract))

;; Also in <herbie>/core/egg-herbie.rkt
(define (type-combinations types type-dict)
  (reap [sow]
    (let loop ([types types] [assigns '()])
      (match types
        [(list) (sow assigns)]
        [(list type rest ...)
         (for ([repr (dict-ref type-dict type)])
           (loop rest (cons (cons type repr) assigns)))]))))

;; Specialized "product" construction of a platform.
;; Given a map from type to representations, instantiate an operator implementation
;; for each valid assignment of representations.
(define (make-platform-product type-dict ops)
  (define reprs
    (for/fold ([reprs* '()]) ([(_ reprs) (in-dict type-dict)])
      (append reprs reprs*)))
  (define impls
    (for/fold ([impls '()]) ([op (in-list ops)])
      (define otype (real-operator-info op 'otype))
      (define itypes (real-operator-info op 'itype))
      (define types (remove-duplicates (cons otype itypes)))
      (for/fold ([impls impls]) ([assigns (type-combinations types type-dict)])
        (define orepr (dict-ref assigns otype))
        (define ireprs (map (curry dict-ref assigns) itypes))
        (cons (list* op orepr ireprs) impls))))
  (make-platform reprs '() impls))

;; Macro version of `make-platform-product`
(define-syntax (platform-product stx)
  (syntax-case stx ()
    [(_ cs ...)
     (begin
       (define-values (type-dict op-names)
         (let loop ([clauses (syntax->list #'(cs ...))]
                    [type-dict '()]
                    [op-names '()])
          (syntax-case clauses ()
            [() (values type-dict op-names)]
            [(#:type type [reprs ...] rest ...)
             (loop #'(rest ...)
                   (cons (cons #'type (syntax->list #'(reprs ...))) type-dict)
                   op-names)]
            [(#:operators [ops ...] rest ...)
             (loop #'(rest ...)
                   type-dict
                   (append (syntax->list #'(ops ...)) op-names))]
            [(#:type _ _)
             (raise-syntax-error 'define-platform-product
                                 "representation list malformed"
                                 stx
                                 clauses)]
            [(#:type _)
             (raise-syntax-error 'define-platform-product
                                 "missing representations"
                                 stx
                                 clauses)]
            [(#:type)
             (raise-syntax-error 'define-platform-product
                                 "missing type and representations"
                                 stx
                                 clauses)]
            [(#:operators _)
             (raise-syntax-error 'define-platform-product
                                 "operator list malformed"
                                 stx
                                 clauses)]
            [(#:operators)
             (raise-syntax-error 'define-platform-product
                                 "missing operators"
                                 stx
                                 clauses)])))
       (with-syntax ([(type-dict ...) type-dict]
                     [(op-names ...) op-names])
         #'(make-platform-product '(type-dict ...) '(op-names ...))))]
    [_
     (raise-syntax-error 'define-platform-product
                         "bad syntax"
                         stx)]))
