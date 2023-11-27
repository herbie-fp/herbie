#lang racket

(require (for-syntax racket/list racket/match))
(require "common.rkt" "errors.rkt"
         "syntax/rules.rkt" "syntax/syntax.rkt" "syntax/types.rkt"
         (submod "syntax/syntax.rkt" internals))

(provide
  platform get-platform *active-platform* activate-platform!
  ;; Platform API
  (contract-out
    ;; Operator sets
    [operator-set? (-> any/c boolean?)]
    [operator-set-operators (-> operator-set? (listof symbol?))]
    ;; Platforms
    [platform? (-> any/c boolean?)]
    [platform-reprs (-> platform? (listof representation?))]
    [platform-impls (-> platform? (listof symbol?))]
    [platform-conversions (-> platform? (listof symbol?))]
    [platform-reprchange-rules (-> platform? (listof rule?))]
    [platform-union (-> platform? platform? ... platform?)]
    [platform-intersect (-> platform? platform? ... platform?)]
    [platform-subtract (-> platform? platform? ... platform?)]
    [platform-operator-set (-> platform? operator-set?)]))

(module+ internals
  (provide platform get-platform register-platform!
           platform-product platform-union platform-intersect
           platform-subtract platform-filter operator-set
           platform-operator-set))

;;; Platforms describe a set of representations, operator, and constants
;;; Herbie should use during its improvement loop. Platforms are just
;;; a "type signature" - they provide no implementations of floating-point
;;; operations (see plugins). During runtime, platforms will verify if
;;; every listed feature is actually loaded by Herbie and will panic if
;;; implemenations are missing. Unlike plugins, only one platform may be
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

;; Optional error handler based on a value `optional?`.
(define-syntax-rule (with-cond-handlers optional? ([pred handle] ...) body ...)
  (if optional?
      (with-handlers ([pred handle] ...) body ...)
      (begin body ...)))

;; Constructor procedure for platforms.
;; The platform is described by a list of operator implementations.
;;
;;  [<name> (<itype> ... <otype>) <cost>]
;;
;; Verifies that a platform is well-formed and the platform will be
;; supported by Herbie's runtime. A platform specified by `optional?`
;; may discard any implementations it fails to find in the Racket runtime.
(define (make-platform pform #:optional? [optional? #f])
  ; load the representations
  (define unique-reprs (mutable-set))
  (define reprs
    (reap [sow]
      (for ([impl-sig (in-list pform)])
        (match-define (list _ tsig _) impl-sig)
        (for ([repr-name (in-list tsig)])
          (unless (set-member? unique-reprs repr-name)
            (set-add! unique-reprs repr-name)
            (sow (get-representation repr-name)))))))
  ; load the conversions
  (define missing (mutable-set))
  (define convs
    (reap [sow]
      (for ([impl-sig (in-list pform)])
        (match-define (list op tsig _) impl-sig)
        (match* (op tsig)
          [('cast `(,itype ,otype))
           (define irepr (get-representation itype))
           (define orepr (get-representation otype))
           (with-cond-handlers optional?
                               ([exn:fail:user:herbie:missing?
                                 (λ (_) (set-add! missing (list 'cast itype otype)))])
             (for ([impl (in-list (get-conversion-impls irepr orepr))])
               (sow impl)))]
          [('cast _)
           (error 'make-platform "unexpected type signature for `cast` ~a" tsig)]
          [(_ _) (void)]))))
  ; load the operator implementations
  (define impls
    (reap [sow]
      (for ([impl-sig (in-list pform)])
      (match-define (list name tsig _) impl-sig)
        (match* (name tsig)
          [('cast _) (void)] ; casts
          [(_ `(,otype)) ; constants
           (define orepr (get-representation otype))
           (with-cond-handlers optional?
                               ([exn:fail:user:herbie:missing?
                                 (λ (_) (set-add! missing (list name otype)))])
             (sow (get-parametric-constant name orepr #:all? #t)))]
          [(_ `(,itypes ... ,otype)) ; operators
           (define ireprs (map get-representation itypes))
           (with-cond-handlers optional?
                               ([exn:fail:user:herbie:missing?
                                 (λ (_) (set-add! missing `(op ,name ,@itypes , otype)))])
             (sow (apply get-parametric-operator name ireprs #:all? #t)))]))))
  ; emit warnings if need be
  (unless (set-empty? missing)
    (warn 'platform
          "platform has missing optional implementations: ~a"
          (string-join
            (for/list ([m (in-set missing)])
              (match-define (list name itypes ... otype) m)
              (format "(~a ~a)" name `(,@itypes ,otype)))
            " ")))
  ; make the platform
  (create-platform #f reprs (append convs impls)))

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
  (define (go cs es #:optional? [optional? #f])
    ;; iterate over `es ...` to get implementation signatures
    (define sigs
      (let loop ([clauses es] [sigs '()])
        (cond
          [(null? clauses) sigs]
          [else
           (syntax-case (car clauses) ()
             [((itype ... otype) (op ...))
              (begin
                (define tsig (syntax->datum #'(itype ... otype)))
                (define ops* (syntax->list #'(op ...)))
                (unless (andmap identifier? ops*)
                  (oops! "expected a list of identifiers" #'(op ...)))
                (loop (cdr clauses)
                      (for/fold ([sigs sigs]) ([name (in-list ops*)])
                        (cons (list name tsig 1) sigs))))]
             [((_ ... _) bad)
              (oops! "expected a list of operators" #'bad)]
             [(bad (_ ...))
              (oops! "expected a type signature" #'bad)]
             [(_ _)
              (oops! "malformed entry" (car clauses))]
             [_
              (oops! "expected [<signature> <ops>]" (car clauses))])])))
    (with-syntax ([(sigs ...) sigs] [optional? optional?])
      #'(make-platform '(sigs ...) #:optional? optional?)))
  (syntax-case stx ()
    [(_ #:optional #:conversions (cs ...) es ...)
     (go (syntax->list #'(cs ...)) (syntax->list #'(es ...)) #:optional? #t)]
    [(_ #:conversions (cs ...) es ...)
     (go (syntax->list #'(cs ...)) (syntax->list #'(es ...)))]
    [(_ es ...)
     #'(platform #:conversions () es ...)]
    [_
     (oops! "bad syntax")]))

;; Representation conversions in a platform.
(define (platform-conversions pform)
  (reap [sow]
    (for ([impl (in-list (platform-impls pform))])
      (when (eq? (impl->operator impl) 'cast)
        (sow impl)))))

;; The "precision change" rules valid for a platform
(define (platform-reprchange-rules pform)
  ; conversion signatures
  (define as-set (mutable-set))
  (define convs
    (reap [sow]
      (for ([impl (platform-conversions pform)])
        (match-define (list irepr) (impl-info impl 'itype))
        (define orepr (impl-info impl 'otype))
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
             (repr-supported? (impl-info impl 'otype))
             (andmap repr-supported? (impl-info impl 'itype))))
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

;; Projection of a platform to an operator set.
(define (platform-operator-set pform)
  (define unique (mutable-set))
  (make-operator-set
    (for/fold ([ops '()]) ([impl (platform-impls pform)])
      (define op (impl->operator impl))
      (cond
        [(set-member? unique op)
         ops]
        [else
         (define itypes (operator-info op 'itype))
         (define otype (operator-info op 'otype))
         (set-add! unique op)
         (cons `(,op ,@itypes ,otype) ops)]))))

;; List of operator names in an operator set.
(define (operator-set-operators oset)
  (for/list ([op (operator-set-ops oset)])
    (match-define (list name _ ...) op)
    name))

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
(define (make-platform-product type-dict op-set #:optional? [optional? #f])
  (define impls
    (for/fold ([impls '()]) ([entry (in-list (operator-set-ops op-set))])
      (match-define (list op itypes ... otype) entry)
      (define types (remove-duplicates (cons otype itypes)))
      (for/fold ([impls impls]) ([assigns (type-combinations types type-dict)])
        (define orepr (dict-ref assigns otype))
        (define ireprs (map (curry dict-ref assigns) itypes))
        (cons `(,op (,@ireprs ,orepr) 1) impls))))
  (make-platform impls #:optional? optional?))

;; Macro version of `make-platform-product`
(define-syntax (platform-product stx)
  (define (oops! why [sub-stx #f])
    (if sub-stx
        (raise-syntax-error 'platform-product why stx sub-stx)
        (raise-syntax-error 'platform-product why stx)))
  (define (go cs os #:optional? [optional? #f])
    (let loop ([clauses cs] [type-dict '()])
      (cond
        [(null? clauses)
         (with-syntax ([type-dict type-dict] [os os])
           (if optional?
               #'(make-platform-product 'type-dict os #:optional? #t)
               #'(make-platform-product 'type-dict os)))]
        [else
         (syntax-case (car clauses) ()
           [(type (repr-names ...))
            (let ([ty (syntax->datum #'type)]
                  [rn (syntax->datum #'(repr-names ...))])
              (loop (cdr clauses) (cons (cons ty rn) type-dict)))]
           [(type _)
            (oops! "expected a list of representations" #'bad)]
           [_
            (oops! "expected [<type> (<repr> ...)]" (car clauses))])])))
  (syntax-case stx ()
    [(_ #:optional cs ... os)
     (go (syntax->list #'(cs ...)) #'os #:optional? #t)]
    [(_ cs ... os)
     (go (syntax->list #'(cs ...)) #'os)]))
