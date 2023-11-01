#lang racket

(require (for-syntax racket/list racket/match))
(require "common.rkt" "errors.rkt"
         "syntax/rules.rkt" "syntax/syntax.rkt" "syntax/types.rkt"
         (submod "syntax/syntax.rkt" internals))

(provide
  platform get-platform *active-platform*
  activate-platform! operator-set operator-set?
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
                           operator-set?
                           platform?)]))

(module+ internals
  (provide platform get-platform register-platform!
           platform-product platform-union platform-intersect
           platform-subtract platform-filter operator-set))

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
        [(list name itypes ... _)
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
;;     #:conversions ([binary64 binary32] ...)  ; conversions
;;     [(bool) (TRUE FALSE)]                    ; constant (0-ary functions)
;;     [(bool bool) (not)]                      ; 1-ary function: bool -> bool
;;     [(bool bool bool) (and or)]              ; 2-ary function: bool -> bool -> bool
;;     [(binary64 binary64 bool)                ; 2-ary function: binary64 -> binary64 -> bool
;;      (== > < >= <=)]
;;     ...
;; ))
;; ```
(define-syntax (platform stx)
  (define (oops! why [sub-stx #f])
    (if sub-stx
        (raise-syntax-error 'platform why stx sub-stx)
        (raise-syntax-error 'platform why stx)))
  (syntax-case stx ()
    [(_ #:conversions (cs ...) e1 es ...)
     (begin
       ;; iterate over `cs ...` to get conversions
       (define convs
         (let loop ([clauses (syntax->list #'(cs ...))] [convs '()])
           (match clauses
             [(list) convs]
             [(list entry rest ...)
              (syntax-case entry ()
                [(in out) (loop rest (cons (cons #'in #'out) convs))]
                [_ (oops! "malformed conversion clause" entry)])])))
       ;; iterate over `e1 es ...` to get operators
       (define-values (reprs ops)
         (let loop ([clauses (syntax->list #'(e1 es ...))] [reprs '()] [ops '()])
           (cond
             [(null? clauses) (values reprs ops)]
             [else
              (syntax-case (car clauses) ()
                [((itype ... otype) (op ...))
                 (begin
                   (define tsig (syntax->datum #'(itype ... otype)))
                   (define ops* (syntax->datum #'(op ...)))
                   (unless (andmap symbol? ops*)
                     (oops! "expected a list of identifiers" #'(op ...)))
                   (loop (cdr clauses)
                         (remove-duplicates (append tsig reprs))
                         (append (map (λ (o) (cons o tsig)) ops*) ops)))]
                [((itype ... otype) bad)
                 (oops! "expected a list of operators" #'bad)]
                [(bad (op ...))
                 (oops! "expected a type signature" #'bad)]
                [(_ _)
                 (oops! "malformed entry" (car clauses))]
                [_
                 (oops! "expected [<signature> <ops>]" (car clauses))])])))
       ;; compose everything into a `make-platform` call
       (with-syntax ([(repr-names ...) reprs]   
                     [(convs ...) convs]
                     [(ops ...) ops])
         #'(make-platform '(repr-names ...) '(convs ...) '(ops ...))))]
    [(_ e1 es ...)
     #'(platform #:conversions () e1 es ...)]
    [_
     (oops! "bad syntax")]))

;; Real operators in a platform.
(define (platform-operators pform)
  (define ops (mutable-set))
  (reap [sow]
    (for ([impl (in-list (platform-impls pform))])
      (define op (impl->operator impl))
      (unless (set-member? ops op)
        (set-add! ops op)
        (sow op)))))

;; Representation conversions in a platform.
(define (platform-conversions pform)
  (reap [sow]
    (for ([impl (in-list (platform-impls pform))])
      (when (eq? (impl->operator impl) 'cast)
        (sow impl)))))

;; The "precision change" rules valid for a platform
(define (platform-reprchange-rules pform)
  ; sort by representation index
  (define reprs (platform-reprs pform))

  ; conversion signatures
  (define as-set (mutable-set))
  (define convs
    (reap [sow]
      (for ([impl (platform-conversions pform)])
        (match-define (list irepr) (operator-info impl 'itype))
        (define orepr (operator-info impl 'otype))
        (unless (or (set-member? as-set (cons irepr orepr))
                     (set-member? as-set (cons orepr irepr)))
          (set-add! as-set (cons irepr orepr))
          (sow (cons irepr orepr))))))

  ; precision rule generator
  (define (make-precision-rewrite irepr orepr)
    (define irepr-sym (repr->symbol irepr))
    (define orepr-sym (repr->symbol orepr))
    (define conv (sym-append irepr-sym '-> orepr-sym))
    (define change (sym-append '<- irepr-sym))
    (define rewrite-name (sym-append 'rewrite- orepr-sym '/ irepr-sym))
    (rule rewrite-name 'a `(,conv (,change a)) `((a . ,irepr)) orepr))

  ; for each conversion, enable precision rewrite
  (define prec-rules
    (for/fold ([rules '()]) ([(irepr orepr) (in-dict convs)])
      (list* (make-precision-rewrite irepr orepr)
             (make-precision-rewrite orepr irepr)
             rules)))

  ; for each conversion, enable a precision simplification
  (define prec-simplifiers
    (for/fold ([rules '()]) ([(irepr orepr) (in-dict convs)])
      (define irepr-sym (repr->symbol irepr))
      (define orepr-sym (repr->symbol orepr))
      (define conv1 (sym-append irepr-sym '-> orepr-sym))
      (define conv2 (sym-append orepr-sym '-> irepr-sym))
      (define rw-name1 (sym-append 'rewrite- orepr-sym '/ irepr-sym '-simplify))
      (define rw-name2 (sym-append 'rewrite- irepr-sym '/ orepr-sym '-simplify))
      ; rules
      (define simplify1 (rule rw-name1 'a `(,conv1 (,conv2 a)) `((a . ,orepr)) orepr))
      (define simplify2 (rule rw-name2 'a `(,conv2 (,conv1 a)) `((a . ,irepr)) irepr))
      (list* simplify1 simplify2 rules)))

  (append prec-rules prec-simplifiers))

;; Set operations on platforms
(define ((make-set-operation merge) p1 . ps)
  (define (combine accessor)
    (for/fold ([s (accessor p1)]) ([p (in-list ps)])
      (merge s (accessor p))))
  (create-platform #f
                   (combine platform-reprs)
                   (combine platform-impls)))

;; Set union for platforms.
;; Use list operations for deterministic ordering.
(define platform-union
  (make-set-operation
    (λ (s1 s2) (remove-duplicates (append s1 s2)))))

;; Set intersection for platforms.
;; Use list operations for deterministic ordering.
(define platform-intersect
  (make-set-operation
    (λ (s1 s2) (filter (curry set-member? (list->set s2)) s1))))

;; Set subtract for platforms.
;; Use list operations for deterministic ordering.       
(define platform-subtract
  (make-set-operation
    (λ (s1 s2) (filter-not (curry set-member? (list->set s2)) s1))))

;; Coarse-grained filters on platforms.
(define ((make-platform-filter repr-supported? op-supported?) pform)
  (define reprs* (filter repr-supported? (platform-reprs pform)))
  (define impls*
    (filter
      (λ (impl)
        (and (op-supported? (impl->operator impl))
             (repr-supported? (operator-info impl 'otype))
             (andmap repr-supported? (operator-info impl 'itype))))
      (platform-impls pform)))
  (create-platform #f reprs* impls*))

;; Macro version of `make-platform-filter`.
(define-syntax (platform-filter stx)
  (define (oops! why [sub-stx #f])
    (if sub-stx
        (raise-syntax-error 'platform why stx sub-stx)
        (raise-syntax-error 'platform why stx)))
  (syntax-case stx ()
    [(_ cs ... pform)
     (let loop ([clauses (syntax->list #'(cs ...))]
                [repr-filter #f]
                [op-filter #f])
       (syntax-case clauses ()
         [()
          (with-syntax ([repr-filter repr-filter] [op-filter op-filter])
            #'((make-platform-filter (or repr-filter (const #t))
                                     (or op-filter (const #t)))
                pform))]
         [(#:representations [reprs ...] rest ...)
          (let ([reprs* (syntax->list #'(reprs ...))])
            (when repr-filter
              (oops! "cannot set both #:representations and #:not-representations"))
            (loop #'(rest ...)
                  #'(lambda (r)
                      (define rs (map get-representation '(reprs ...)))
                      (set-member? (list->set rs) r))
                  op-filter))]
         [(#:not-representations [reprs ...] rest ...)
          (let ([reprs* (syntax->list #'(reprs ...))])
            (when repr-filter
              (oops! "cannot set both #:representations and #:not-representations"))
            (loop #'(rest ...)
                  #'(lambda (r)
                      (define rs (map get-representation '(reprs ...)))
                      (not (set-member? (list->set rs) r)))
                  op-filter))]
         [(#:operators [ops ...] rest ...)
          (let ([ops* (syntax->list #'(ops ...))])
            (when op-filter
              (oops! "cannot set both #:operators and #:not-operators"))
            (loop #'(rest ...)
                  repr-filter
                  #'(lambda (r)
                      (define ops* '(ops ...))
                      (set-member? (list->set ops*) r))))]
         [(#:not-operators [ops ...] rest ...)
          (let ([ops* (syntax->list #'(ops ...))])
            (when op-filter
              (oops! "cannot set both #:operators and #:not-operators"))
            (loop #'(rest ...)
                  repr-filter
                  #'(lambda (r) 
                      (define ops* '(ops ...))
                      (not (set-member? (list->set '(ops ...)) r)))))]
         [_
          (oops! "bad syntax")]))]
    [_
     (oops! "bad syntax" stx)]))

;; Set of operators: operators are just names with a type signature.
;; Platforms may be instantiated from operator sets using `platform-product`.
(struct operator-set (ops)
        #:name $operator-set
        #:constructor-name make-operator-set)

;; Constructs an operator set.
(define-syntax (operator-set stx)
  (define (oops! why [sub-stx #f])
    (if sub-stx
        (raise-syntax-error 'operator-set why stx sub-stx)
        (raise-syntax-error 'operator-set why stx)))
  (syntax-case stx ()
    [(_ es ...)
     (let loop ([clauses (syntax->list #'(es ...))] [op-data '()])
       (cond
         [(null? clauses)
          (with-syntax ([op-data op-data])
            #'(make-operator-set 'op-data))]
         [else
          (syntax-case (car clauses) ()
            [((itype ... otype) (op ...))
             (let ([tsig (syntax->datum #'(itype ... otype))]
                   [ops* (syntax->datum #'(op ...))])
               (unless (andmap symbol? ops*)
                 (oops! "expected a list of identifiers" #'(op ...)))
               (loop (cdr clauses)
                     (for/fold ([op-data op-data]) ([o (in-list ops*)])
                       (cons (cons o tsig) op-data))))]
            [((itype ... otype) bad)
             (oops! "expected a list of operators" #'bad)]
            [(bad (op ...))
             (oops! "expected a type signature" #'bad)]
            [(_ _)
             (oops! "malformed entry" (car clauses))]
            [_
             (oops! "expected [<signature> <ops>]" (car clauses))])]))]
    [_
     (oops! "bad syntax" stx)]))

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
(define (make-platform-product type-dict op-set)
  (define reprs
    (for/fold ([reprs* '()]) ([(_ reprs) (in-dict type-dict)])
      (append reprs reprs*)))
  (define impls
    (for/fold ([impls '()]) ([entry (in-list (operator-set-ops op-set))])
      (match-define (list op itypes ... otype) entry)
      (define types (remove-duplicates (cons otype itypes)))
      (for/fold ([impls impls]) ([assigns (type-combinations types type-dict)])
        (define orepr (dict-ref assigns otype))
        (define ireprs (map (curry dict-ref assigns) itypes))
        (cons `(,op ,@ireprs ,orepr) impls))))
  (make-platform reprs '() impls))

;; Macro version of `make-platform-product`
(define-syntax (platform-product stx)
  (define (oops! why [sub-stx #f])
    (if sub-stx
        (raise-syntax-error 'platform-product why stx sub-stx)
        (raise-syntax-error 'platform-product why stx)))
  (syntax-case stx ()
    [(_ cs ... os)
     (let loop ([clauses (syntax->list #'(cs ...))] [type-dict '()])
      (cond
        [(null? clauses)
         (with-syntax ([type-dict type-dict])
           #'(make-platform-product 'type-dict os))]
        [else
         (syntax-case (car clauses) ()
           [(type (repr-names ...))
            (let ([ty (syntax->datum #'type)]
                  [rn (syntax->datum #'(repr-names ...))])
              (loop (cdr clauses) (cons (cons ty rn) type-dict)))]
           [(type _)
            (oops! "expected a list of representations" #'bad)]
           [_
            (oops! "expected [<type> (<repr> ...)]" (car clauses))])]))]))
