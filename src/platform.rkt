#lang racket

(require (for-syntax racket/match))

(require "common.rkt"
         "errors.rkt"
         "syntax/rules.rkt"
         "syntax/syntax.rkt"
         "syntax/types.rkt"
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
    [platform-operator-set (-> platform? operator-set?)]
    ; Cost model
    [platform-cost-proc (-> platform? procedure?)]))

(module+ internals
  (provide platform get-platform register-platform!
           platform-product platform-union platform-intersect
           platform-subtract platform-filter
           operator-set platform-operator-set
           with-terminal-cost cost-map))

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
(struct platform (name reprs impls impl-costs repr-costs)
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
      (with-handlers ([pred handle] ...) (begin body ...))
      (begin body ...)))

;; Constructor procedure for platforms.
;; The platform is described by a list of operator implementations.
;;
;;  [<name> (<itype> ... <otype>) <cost>]
;;
;; Verifies that a platform is well-formed and the platform will be
;; supported by Herbie's runtime. A platform specified by `optional?`
;; may discard any implementations it fails to find in the Racket runtime.
(define (make-platform pform
                       #:optional? [optional? #f]
                       #:if-cost [if-cost #f])
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
  (define costs (make-hash))
  (define convs
    (reap [sow]
      (for ([impl-sig (in-list pform)])
        (match-define (list op tsig cost) impl-sig)
        (match* (op tsig)
          [('cast `(,itype ,otype))
           (define irepr (get-representation itype))
           (define orepr (get-representation otype))
           (with-cond-handlers optional?
                               ([exn:fail:user:herbie:missing?
                                 (λ (_) (set-add! missing (list 'cast itype otype)))])
             (for ([impl (in-list (get-conversion-impls irepr orepr))])
               (hash-set! costs impl cost)
               (sow impl)))]
          [('cast _)
           (error 'make-platform "unexpected type signature for `cast` ~a" tsig)]
          [(_ _) (void)]))))
  ; load the operator implementations
  (define impls
    (reap [sow]
      (for ([impl-sig (in-list pform)])
      (match-define (list name tsig cost) impl-sig)
        (match* (name tsig)
          [('cast _) (void)] ; casts
          [(_ `(,otype)) ; constants
           (define orepr (get-representation otype))
           (with-cond-handlers optional?
                               ([exn:fail:user:herbie:missing?
                                 (λ (_) (set-add! missing (list name otype)))])
             (let ([impl (get-parametric-constant name orepr #:all? #t)])
               (hash-set! costs impl cost)
               (sow impl)))]
          [(_ `(,itypes ... ,otype)) ; operators
           (define ireprs (map get-representation itypes))
           (with-cond-handlers optional?
                               ([exn:fail:user:herbie:missing?
                                 (λ (_) (set-add! missing `(op ,name ,@itypes , otype)))])
             (let ([impl (apply get-parametric-operator name ireprs #:all? #t)])
               (hash-set! costs impl cost)
               (sow impl)))]))))
  ; set cost of `if`
  (when if-cost (hash-set! costs 'if if-cost))
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
  (create-platform #f
                   reprs
                   (append convs impls)
                   (make-immutable-hash (hash->list costs))
                   (hash)))

;; Macro version of `make-platform`
;; 
;; Example usage:
;; ```
;; (define default
;;   (platform
;;     #:conversions ([binary64 binary32] ...)  ; conversions
;;     #:default-cost 1                         ; default cost per impl
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
    (raise-syntax-error 'platform why stx sub-stx))
  (define (go cs es optional? default-cost if-cost)
    ;; iterate over `cs ...` to get conversion signatures
    (define convs-sigs
      (let loop ([clauses cs] [convs '()])
        (match clauses
          [(list) convs]
          [(list entry rest ...)
           (syntax-case entry ()
             [(in out)
              (let ([in* (syntax->datum #'in)]
                    [out* (syntax->datum #'out)])
                (unless default-cost
                  (oops! "#:default-cost required with #:conversions" cs))
                (loop rest
                      (list* (list 'cast (list in* out*) default-cost)
                             (list 'cast (list out* in*) default-cost)
                             convs)))]
             [_ (oops! "malformed conversion clause" entry)])])))
    ;; iterate over `es ...` to get implementation signatures
    (define impl-sigs
      (let loop ([clauses es] [impl-sigs '()])
        (cond
          [(null? clauses)
           impl-sigs]
          [else
           (syntax-case (car clauses) ()
             [((itype ... otype) (op ...) cost)
              ; multiple implementations with same type sig and cost
              (loop (for/fold ([clauses (cdr clauses)])
                              ([o (in-list (syntax->datum #'(op ...)))])
                      (define cl (with-syntax ([op o]) #'((itype ... otype) op cost)))
                      (cons cl clauses))
                    impl-sigs)]
             [((itype ... otype) (op ...))
              ; multiple implementations with same type sig and default cost
              (loop (for/fold ([clauses (cdr clauses)])
                              ([o (in-list (syntax->datum #'(op ...)))])
                      (define cl (with-syntax ([op o]) #'((itype ... otype) op))) 
                      (cons cl clauses))
                    impl-sigs)]
             [((itype ... otype) op cost)
              ; single implementation with cost
              (begin
                (unless (identifier? #'op)
                  (oops! "expected an identifier" #'op))
                (loop (cdr clauses) (cons #'(op (itype ... otype) (unquote cost)) impl-sigs)))]
             [((itype ... otype) op)
              ; single implementation with default cost
              (with-syntax ([cost default-cost])
                (unless default-cost
                  (oops! "#:default-cost required" (car clauses)))
                (loop (cons #'((itype ... otype) op cost) (cdr clauses)) impl-sigs))]
             [((_ ... _) bad)
              (oops! "expected a list of operators" #'bad)]
             [(bad (_ ...))
              (oops! "expected a type signature" #'bad)]
             [(_ _)
              (oops! "malformed entry" (car clauses))]
             [_
              (oops! "expected [<signature> <ops>]" (car clauses))])])))
    (with-syntax ([(impl-sigs ...) (append convs-sigs impl-sigs)]
                  [optional? optional?]
                  [if-cost if-cost])
      #'(make-platform `(impl-sigs ...) #:optional? optional? #:if-cost if-cost)))
  (syntax-case stx ()
    [(_ cs ...)
     (begin
       (define default-cost #f)
       (define if-cost #f)
       (define optional? #f)
       (define conv-sigs '())
       (define impl-sigs '())
       (let loop ([clauses #'(cs ...)])
         (syntax-case clauses ()
           [(#:optional rest ...)
            (set! optional? #t)
            (loop #'(rest ...))]
           [(#:default-cost cost rest ...)
            (set! default-cost (eval-syntax #'cost))
            (loop #'(rest ...))]
           [(#:default-cost)
            (oops! "expected cost after keyword" clauses)]
           [(#:if-cost cost rest ...)
            (set! if-cost (eval-syntax #'cost))
            (loop #'(rest ...))]
           [(#:if-cost)
            (oops! "expected cost after keyword" clauses)]
           [(#:conversions (cs ...) rest ...)
            (set! conv-sigs (append (syntax->list #'(cs ...)) conv-sigs))
            (loop #'(rest ...))]
           [(#:conversions)
            (oops! "expected conversions after keyword" clauses)]
           [(sig rest ...)
            (set! impl-sigs (cons #'sig impl-sigs))
            (loop #'(rest ...))]
           [()
            (go conv-sigs impl-sigs optional? default-cost if-cost)])))]
    [_ (oops! "bad syntax")]))

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

;; Merger for costs.
(define (merge-cost pform-costs key #:optional? [optional? #f])
  (define costs
    (for/list ([pform-cost (in-list pform-costs)])
      (hash-ref pform-cost key #f)))
  (match (remove-duplicates (filter identity costs))
    [(list c1 c2 cs ...)
     (error 'merge-costs
            "mismatch when combining cost model ~a ~a"
            key (list* c1 c2 cs))]
    [(list c1)
     c1]
    [(list)
     (unless optional?
       (error 'merge-costs
              "cannot find cost for implementation ~a"
              key))
     #f]))

;; Set operations on platforms.
(define ((make-set-operation merge-impls) p1 . ps)
  ; apply set operation on impls
  (define impls (apply merge-impls (map platform-impls (cons p1 ps))))
  ; valid representations are based on impls
  (define reprs
    (remove-duplicates
      (for/fold ([reprs '()]) ([impl (in-list impls)])
        (append (cons (impl-info impl 'otype)
                      (impl-info impl 'itype))
                reprs))))
  ; impl costs are based on impls
  (define pform-impl-costs (map platform-impl-costs (cons p1 ps)))
  (define impl-costs
    (for/hash ([impl (in-list impls)])
      (values impl (merge-cost pform-impl-costs impl))))
  ; special case for `if`
  (define if-cost (merge-cost pform-impl-costs 'if #:optional? #t))
  (when if-cost
    (set! impl-costs (hash-set impl-costs 'if if-cost)))
  ; repr costs are based on reprs (may be missing)
  (define pform-repr-costs (map platform-repr-costs (cons p1 ps)))
  (define repr-costs (hash))
  (for/list ([repr (in-list reprs)])
    (define repr-cost (merge-cost pform-repr-costs repr #:optional? #t))
    (when repr-cost
      (set! repr-costs (hash-set repr-costs repr repr-cost))))
  (create-platform #f reprs impls impl-costs repr-costs))

;; Set union for platforms.
;; Use list operations for deterministic ordering.
(define platform-union
  (make-set-operation
    (λ (rs . rss) (remove-duplicates (apply append rs rss)))))

;; Set intersection for platforms.
;; Use list operations for deterministic ordering.
(define platform-intersect
  (make-set-operation
    (λ (rs . rss)
      (for/fold ([rs rs]) ([rs0 (in-list rss)])
        (filter (curry set-member? (list->set rs0)) rs)))))

;; Set subtract for platforms.
;; Use list operations for deterministic ordering.       
(define platform-subtract
  (make-set-operation
    (λ (rs . rss)
      (for/fold ([rs rs]) ([rs0 (in-list rss)])
        (filter-not (curry set-member? (list->set rs0)) rs)))))

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
    (raise-syntax-error 'platform why stx sub-stx))
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
          (begin
            (when repr-filter
              (oops! "cannot set both #:representations and #:not-representations"))
            (loop #'(rest ...)
                  #'(lambda (r)
                      (define rs (map get-representation '(reprs ...)))
                      (set-member? (list->set rs) r))
                  op-filter))]
         [(#:not-representations [reprs ...] rest ...)
          (begin
            (when repr-filter
              (oops! "cannot set both #:representations and #:not-representations"))
            (loop #'(rest ...)
                  #'(lambda (r)
                      (define rs (map get-representation '(reprs ...)))
                      (not (set-member? (list->set rs) r)))
                  op-filter))]
         [(#:operators [ops ...] rest ...)
          (begin
            (when op-filter
              (oops! "cannot set both #:operators and #:not-operators"))
            (loop #'(rest ...)
                  repr-filter
                  #'(lambda (r)
                      (define ops* '(ops ...))
                      (set-member? (list->set ops*) r))))]
         [(#:not-operators [ops ...] rest ...)
          (begin
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
    (raise-syntax-error 'operator-set why stx sub-stx))
  (define (go clauses)
    (let loop ([clauses clauses] [op-data '()])
      (cond
        [(null? clauses)
         (with-syntax ([op-data op-data])
           #'(make-operator-set 'op-data))]
        [else
         (syntax-case (car clauses) ()
           [((itype ... otype) (op ...))
            ; multiple operators with same type signature and default cost
            (loop (for/fold ([clauses (cdr clauses)])
                            ([o (in-list (syntax->datum #'(op ...)))])
                      (define cl (with-syntax ([op o]) #'((itype ... otype) op)))
                      (cons cl clauses))
                  op-data)]
           [((itype ... otype) op)
            ; single operator
            (let ([tsig (syntax->datum #'(itype ... otype))])
              (unless (identifier? #'op)
                (oops! "expected an identifier" #'op))
              (loop (cdr clauses) (cons (cons #'op tsig) op-data)))]
           [((_ ... _) bad)
            (oops! "expected a list of operators" #'bad)]
           [(bad (_ ...))
            (oops! "expected a type signature" #'bad)]
           [(_ _)
            (oops! "malformed entry" (car clauses))]
           [_
            (oops! "expected [<signature> <ops>]" (car clauses))])])))
  (syntax-case stx ()
    [(_ es ...)
     (go (syntax->list #'(es ...)))]
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

;; Cost map for operators.
(struct cost-map (costs default)
        #:name $cost-map
        #:constructor-name make-cost-map)

;; Constructs a cost map.
;; ```
;; (cost-model
;;   #:default-cost <cost> ; default value for ops not in the map
;;   [(<op> ...) <cost>]   ; multiple ops with the same cost
;;   [<op> <cost>])        ; single op with a cost
;; ```
(define-syntax (cost-map stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'cost-map why stx sub-stx))
  (define (go clauses default-cost)
    (let loop ([clauses clauses] [costs (hash)])
      (cond
        [(null? clauses)
         (with-syntax ([costs costs] [default-cost default-cost])
           #'(make-cost-map costs default-cost))]
        [else
         (syntax-case (car clauses) ()
           [((op ...) cost)
            ; multiple ops with same cost
            (loop (for/fold ([clauses (cdr clauses)]) ([o (syntax->list #'(op ...))])
                    (cons (with-syntax ([o o]) #'(o cost)) clauses))
                  costs)]
           [(op cost)
            ; single op with a cost
            (loop (cdr clauses)
                  (hash-set costs (syntax->datum #'op) (eval-syntax #'cost)))]
           [_ (oops! "malformed clause" (car clauses))])])))
  (syntax-case stx ()
    [(_ #:default-cost cost cl ...)
     (go (syntax->list #'(cl ...)) (eval-syntax #'cost))]
    [(_ #:default-cost)
     (oops! "missing cost after #:default-cost")]
    [(_ cl ...)
     (go (syntax->list #'(cl ...)) #f)]
    [_
     (oops! "bad syntax")]))

;; Procedure layer for `platform-product` macro.
;; Produces the actual platform.
(define (make-platform-product assigns op-set #:optional? [optional? #f])
  (define pforms
    (for/list ([assign (in-list assigns)])
      (match-define (list type-dict costs) assign)
      (define op->cost (cost-map-costs costs))
      (define impls
        (for/list ([entry (in-list (operator-set-ops op-set))])
          (match-define (list op itypes ... otype) entry)
          (define ireprs (map (curry dict-ref type-dict) itypes))
          (define orepr (dict-ref type-dict otype))
          (define cost (or (hash-ref op->cost op #f)
                           (cost-map-default costs)
                           (error 'make-platform-product "unknown cost for `~a`" op)))
          `(,op (,@ireprs ,orepr) ,cost)))
      (make-platform impls #:optional? optional?)))
  (apply platform-union pforms))

;; Specialized "product" construction of a platform.
;; Given an operator set, instantiate a set of platform implementations
;; for each assignment of representations and cost model.
;; ```
;; (platform-product
;;   [([<type> <repr>] ...) <cost-map>]
;;   ...
;;   <operator-set>)
;; ```
(define-syntax (platform-product stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'platform-product why stx sub-stx))
  (define (go clauses oset optional?)
    (let loop ([clauses clauses] [assigns '()])
      (cond
        [(null? clauses)
         (with-syntax ([assigns assigns]
                       [oset oset]
                       [optional? optional?])
           #'(make-platform-product `assigns oset #:optional? optional?))]
        [else
         (syntax-case (car clauses) ()
           [(((type repr) ...) cost-map)
            (loop (cdr clauses)
                  (cons #'(((type . repr) ...) ,cost-map) assigns))]
           [(bad _) (oops! "malformed type assignment" #'bad)]
           [_ (oops! "malformed clause" (car clauses))])])))
  (syntax-case stx ()
    [(_ #:optional cl ... oset) (go (syntax->list #'(cl ...)) #'oset #t)]
    [(_ cl ... oset) (go (syntax->list #'(cl ...)) #'oset #f)]
    [(_) (oops! "missing operator set expression")]
    [_ (oops! "bad syntax")]))

; Updates cost for terminals.
; The cost of a terminal is based on the representation.
(define-syntax with-terminal-cost
  (syntax-rules ()
    [(_ ([reprs costs] ...) pform-expr)
     (for/fold ([pform pform-expr])
               ([repr '(reprs ...)]
                [cost '(costs ...)])
       (define pform-reprs (map representation-name (platform-reprs pform)))
       (unless (set-member? pform-reprs repr)
         (error 'with-terminal-cost
                "repr ~a not found in platform ~a"
                repr
                pform))
       (struct-copy $platform pform
         [repr-costs (hash-set (platform-repr-costs pform)
                               (get-representation repr)
                               cost)]))]))

; Implementation cost in a platform.
(define (impl->cost pform impl)
  (hash-ref (platform-impl-costs pform)
            impl
            (lambda ()
              (error 'impl->cost "no cost for impl '~a" impl))))

; Representation (terminal) cost in a platform.
(define (repr->cost pform repr)
  (hash-ref (platform-repr-costs pform)
            repr
            (lambda ()
              (error 'repr->cost "no cost for repr ~a" repr))))

; Cost model parameterized by a platform.
(define (platform-cost-proc pform)
  (λ (expr repr)
    (let loop ([expr expr] [repr repr])
      (match expr
        [(list 'if cond ift iff)
         (+ (impl->cost pform 'if)
            (loop cond (get-representation 'bool))
            (max (loop ift repr) (loop iff repr)))]
        [(list impl args ...)
         (define itypes (impl-info impl 'itype))
         (apply + (impl->cost pform impl) (map loop args itypes))]
        [_
         (repr->cost repr)]))))
