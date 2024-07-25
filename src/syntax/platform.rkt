#lang racket

(require (for-syntax racket/match))

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../core/programs.rkt"
         "../core/rules.rkt"
         "syntax.rkt"
         "types.rkt")

(provide platform
         get-platform
         *active-platform*
         activate-platform!
         platform-lifting-rules
         platform-lowering-rules
         platform-impl-rules
         ;; Platform API
         ;; Operator sets
         (contract-out [operator-set? (-> any/c boolean?)]
                       [operator-set-operators (-> operator-set? (listof symbol?))]
                       ;; Platforms
                       [platform? (-> any/c boolean?)]
                       [platform-name (-> platform? any/c)]
                       [platform-reprs (-> platform? (listof representation?))]
                       [platform-impls (-> platform? (listof symbol?))]
                       [platform-casts (-> platform? (listof symbol?))]
                       [platform-union (-> platform? platform? ... platform?)]
                       [platform-intersect (-> platform? platform? ... platform?)]
                       [platform-subtract (-> platform? platform? ... platform?)]
                       [platform-operator-set (-> platform? operator-set?)]
                       ; Cost model
                       [platform-impl-cost (-> platform? any/c any/c)]
                       [platform-repr-cost (-> platform? any/c any/c)]
                       [platform-node-cost-proc (-> platform? procedure?)]
                       [platform-cost-proc (-> platform? procedure?)]))

(module+ internals
  (provide platform
           *active-platform*
           get-platform
           register-platform!
           platform-product
           platform-union
           platform-intersect
           platform-subtract
           platform-filter
           operator-set
           platform-operator-set
           with-terminal-cost
           cost-map
           cost-map-scale))

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
      (raise-herbie-error "unknown platform `~a`, found (~a)"
                          name
                          (string-join (map ~a (hash-keys platforms)) ", "))))

;; Loads a platform.
(define (activate-platform! pform)
  ; replace the active operator table
  (clear-active-operator-impls!)
  (for-each activate-operator-impl! (platform-impls pform)))

;; Registers a platform under identifier `name`.
(define (register-platform! name pform)
  (when (hash-has-key? platforms name)
    (error 'register-platform! "platform already registered ~a" name))
  (hash-set! platforms name (struct-copy $platform pform [name name])))

;; Optional error handler based on a value `optional?`.
(define-syntax-rule (with-cond-handlers optional? ([pred handle] ...) body ...)
  (if optional?
      (with-handlers ([pred handle] ...)
        (begin
          body ...))
      (begin
        body ...)))

;; Constructor procedure for platforms.
;; The platform is described by a list of operator implementations.
;;
;;  [<name> (<itype> ... <otype>) <cost>]
;;
;; Verifies that a platform is well-formed and the platform will be
;; supported by Herbie's runtime. A platform specified by `optional?`
;; may discard any implementations it fails to find in the Racket runtime.
(define (make-platform pform #:optional? [optional? #f] #:if-cost [if-cost #f])
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
               (cond
                 [(get-cast-impl irepr orepr)
                  =>
                  (lambda (impl)
                    (hash-set! costs impl cost)
                    (sow impl))]
                 [else (set-add! missing (list 'cast itype otype))])]
              [('cast _) (error 'make-platform "unexpected type signature for `cast` ~a" tsig)]
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
                                     (λ (_) (set-add! missing `(,name ,@itypes ,otype)))])
                                   (let ([impl (apply get-parametric-operator name ireprs #:all? #t)])
                                     (hash-set! costs impl cost)
                                     (sow impl)))]))))
  ; set cost of `if`
  (when if-cost
    (hash-set! costs 'if if-cost))
  ; emit warnings if need be
  (unless (set-empty? missing)
    (warn 'platform
          "platform has missing optional implementations: ~a"
          (string-join (for/list ([m (in-set missing)])
                         (match-define (list name itypes ... otype) m)
                         (format "(~a ~a)" name `(,@itypes ,otype)))
                       " ")))
  ; make the platform
  (create-platform #f reprs (append convs impls) (make-immutable-hash (hash->list costs)) (hash)))

(begin-for-syntax

  ;; Macro-time platform description
  (struct platform-info (optional? default-cost if-cost convs impls))

  ;; Empty platform description
  (define (make-platform-info)
    (platform-info #f #f #f '() '()))

  ;; Parse if cost syntax
  (define (platform/parse-if-cost stx)
    (syntax-case stx (max sum)
      [(max x) #'(list 'max x)]
      [(sum x) #'(list 'sum x)]
      [x #'(list 'max x)]))

  ;; Parse conversion signatures into a list of implementation table
  (define (platform/parse-convs oops! default-cost default-cost-id conv-sigs)
    (for/fold ([impls '()]) ([conv (in-list conv-sigs)])
      (syntax-case conv ()
        [(in out)
         (let ([in #'in] [out #'out])
           (unless default-cost
             (oops! "`#:default-cost` required with `#:conversions`" conv))
           (with-syntax ([default-cost-id default-cost-id] [in in] [out out])
             (list* #'(list 'cast '(in out) default-cost-id)
                    #'(list 'cast '(out in) default-cost-id)
                    impls)))]
        [_ (oops! "malformed conversion clause" conv)])))

  ;; Parse implementation signatures into an implementation table
  (define (platform/parse-impls oops! default-cost default-cost-id impl-sigs)
    (let loop ([impl-sigs impl-sigs] [impls '()])
      (match impl-sigs
        ['() impls]
        [(list impl-sig rest ...)
         (syntax-case impl-sig ()
           [((itype ... otype) (op ...) cost)
            ; multiple implementations with same type sig and cost
            (loop (for/fold ([impl-sigs rest]) ([op (syntax->list #'(op ...))])
                    (let ([impl-sig (with-syntax ([op op])
                                      #'((itype ... otype) op cost))])
                      (cons impl-sig impl-sigs)))
                  impls)]
           [((itype ... otype) (op ...))
            ; multiple implementations with same type sig and cost
            (loop (for/fold ([impl-sigs rest]) ([op (syntax->list #'(op ...))])
                    (let ([impl-sig (with-syntax ([op op])
                                      #'((itype ... otype) op))])
                      (cons impl-sig impl-sigs)))
                  impls)]
           [((itype ... otype) op cost)
            ; single implementation with type sig and cost
            (let ([op #'op])
              (unless (identifier? op)
                (oops! "expected an identifier" op))
              (define impl
                (with-syntax ([op op])
                  #'(list 'op '(itype ... otype) cost)))
              (loop rest (cons impl impls)))]
           [((itype ... otype) op)
            ; single implementation with default cost
            (let ([op #'op])
              (unless (identifier? op)
                (oops! "expected an identifier" op))
              (unless default-cost
                (oops! "#:default-cost required for" impl-sig))
              (define impl
                (with-syntax ([op op] [cost default-cost-id])
                  #'(list 'op '(itype ... otype) cost)))
              (loop rest (cons impl impls)))]
           [_ (oops! "malformed implementation signature" impl-sig)])]))))

;; Macro version of `make-platform`
;;
;; Example usage:
;; ```
;; (define default
;;   (platform
;;     #:conversions ([binary64 binary32] ...)  ; conversions
;;     #:default-cost 1                         ; default cost per impl
;;     #:if-cost 1                              ; cost of an if branch (using max strategy)
;;     #:if-cost (max 1)                        ; cost of an if branch (using max strategy)
;;     #:if-cost (sum 1)                        ; cost of an if branch (using sum strategy)
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
  (syntax-case stx ()
    [(_ cs ...)
     (let loop ([cs #'(cs ...)] [info (make-platform-info)])
       (syntax-case cs ()
         [()
          (let ([default-cost-id (gensym)] [if-cost-id (gensym)])
            (match-define (platform-info optional? default-cost if-cost conv-sigs impl-sigs) info)
            (define cast-impls (platform/parse-convs oops! default-cost default-cost-id conv-sigs))
            (define impls (platform/parse-impls oops! default-cost default-cost-id impl-sigs))
            (with-syntax ([(impl-sigs ...) (append cast-impls impls)]
                          [default-cost-id default-cost-id]
                          [if-cost-id if-cost-id]
                          [default-cost default-cost]
                          [if-cost if-cost]
                          [optional? optional?])
              #'(let ([default-cost-id default-cost] [if-cost-id if-cost])
                  (make-platform (list impl-sigs ...) #:optional? optional? #:if-cost if-cost))))]
         [(#:optional rest ...) (loop #'(rest ...) (struct-copy platform-info info [optional? #t]))]
         [(#:default-cost cost rest ...)
          (loop #'(rest ...) (struct-copy platform-info info [default-cost #'cost]))]
         [(#:default-cost) (oops! "expected value after keyword `#:default-cost`" stx)]
         [(#:if-cost cost rest ...)
          (loop #'(rest ...)
                (struct-copy platform-info info [if-cost (platform/parse-if-cost #'cost)]))]
         [(#:if-cost) (oops! "expected value after keyword `#:if-cost`" stx)]
         [(#:conversions (cs ...) rest ...)
          (loop #'(rest ...)
                (struct-copy platform-info
                             info
                             [convs (append (syntax->list #'(cs ...)) (platform-info-convs info))]))]
         [(#:conversions bad _ ...) (oops! "expected a conversion list" #'bad)]
         [(#:conversions) (oops! "expected conversion list after keyword `#:conversions`" stx)]
         [(impl-sig rest ...)
          (loop
           #'(rest ...)
           (struct-copy platform-info info [impls (cons #'impl-sig (platform-info-impls info))]))]
         [_ (oops! "bad syntax")]))]))

;; Casts between representations in a platform.
(define (platform-casts pform)
  (reap [sow]
        (for ([impl (in-list (platform-impls pform))])
          (when (eq? (impl->operator impl) 'cast)
            (sow impl)))))

;; Merger for costs.
(define (merge-cost pform-costs key #:optional? [optional? #f])
  (define costs (map (lambda (h) (hash-ref h key #f)) pform-costs))
  (match-define (list c0 cs ...) costs)
  (define cost
    (for/fold ([c0 c0]) ([c1 (in-list cs)])
      (match* (c0 c1)
        [(#f _) c1]
        [(_ #f) c0]
        [(c c) c]
        [(_ _) (error 'merge-costs "mismatch when combining cost model ~a ~a" key costs)])))
  (unless (or cost optional?)
    (error 'merge-costs "cannot find cost for implementation ~a" key))
  cost)

;; Set operations on platforms.
(define ((make-set-operation merge-impls) p1 . ps)
  ; apply set operation on impls
  (define impls (apply merge-impls (map platform-impls (cons p1 ps))))
  ; valid representations are based on impls
  (define reprs
    (remove-duplicates (for/fold ([reprs '()]) ([impl (in-list impls)])
                         (append (cons (impl-info impl 'otype) (impl-info impl 'itype)) reprs))))
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
(define platform-union (make-set-operation (λ (rs . rss) (remove-duplicates (apply append rs rss)))))

;; Set intersection for platforms.
;; Use list operations for deterministic ordering.
(define platform-intersect
  (make-set-operation (λ (rs . rss)
                        (for/fold ([rs rs]) ([rs0 (in-list rss)])
                          (filter (curry set-member? (list->set rs0)) rs)))))

;; Set subtract for platforms.
;; Use list operations for deterministic ordering.
(define platform-subtract
  (make-set-operation (λ (rs . rss)
                        (for/fold ([rs rs]) ([rs0 (in-list rss)])
                          (filter-not (curry set-member? (list->set rs0)) rs)))))

;; Coarse-grained filters on platforms.
(define ((make-platform-filter repr-supported? op-supported?) pform)
  (define reprs* (filter repr-supported? (platform-reprs pform)))
  (define impls*
    (filter (λ (impl)
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
     (let loop ([clauses (syntax->list #'(cs ...))] [repr-filter #f] [op-filter #f])
       (syntax-case clauses ()
         [()
          (with-syntax ([repr-filter repr-filter] [op-filter op-filter])
            #'((make-platform-filter (or repr-filter (const #t)) (or op-filter (const #t))) pform))]
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
         [_ (oops! "bad syntax")]))]
    [_ (oops! "bad syntax" stx)]))

;; Set of operators: operators are just names with a type signature.
;; Platforms may be instantiated from operator sets using `platform-product`.
(struct operator-set (ops) #:name $operator-set #:constructor-name make-operator-set)

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
            (loop (for/fold ([clauses (cdr clauses)]) ([o (in-list (syntax->datum #'(op ...)))])
                    (define cl
                      (with-syntax ([op o])
                        #'((itype ... otype) op)))
                    (cons cl clauses))
                  op-data)]
           [((itype ... otype) op)
            ; single operator
            (let ([tsig (syntax->datum #'(itype ... otype))])
              (unless (identifier? #'op)
                (oops! "expected an identifier" #'op))
              (loop (cdr clauses) (cons (cons #'op tsig) op-data)))]
           [((_ ... _) bad) (oops! "expected a list of operators" #'bad)]
           [(bad (_ ...)) (oops! "expected a type signature" #'bad)]
           [(_ _) (oops! "malformed entry" (car clauses))]
           [_ (oops! "expected [<signature> <ops>]" (car clauses))])])))
  (syntax-case stx ()
    [(_ es ...) (go (syntax->list #'(es ...)))]
    [_ (oops! "bad syntax" stx)]))

;; Projection of a platform to an operator set.
(define (platform-operator-set pform)
  (define unique (mutable-set))
  (make-operator-set (for/fold ([ops '()]) ([impl (platform-impls pform)])
                       (define op (impl->operator impl))
                       (cond
                         [(set-member? unique op) ops]
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
(struct cost-map (costs default) #:name $cost-map #:constructor-name make-cost-map)

(begin-for-syntax

  ;; Parses cost-map clauses into a table of costs
  (define (cost-map/parse oops! clauses)
    (let loop ([clauses clauses] [costs '()])
      (match clauses
        ['() costs]
        [(list clause rest ...)
         (syntax-case clause ()
           [((op ...) cost)
            ; multiple ops with same cost
            (loop (for/fold ([clauses rest]) ([op (syntax->list #'(op ...))])
                    (define clause
                      (with-syntax ([op op])
                        #'(op cost)))
                    (cons clause clauses))
                  costs)]
           [(op cost)
            ; single op with cost
            (let ([op #'op])
              (unless (identifier? op)
                (oops! "expected identifier" op))
              (define entry
                (with-syntax ([op op])
                  #'(cons 'op cost)))
              (loop rest (cons entry costs)))]
           [_ (oops! "malformed cost clause" clause)])]))))

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
    (define costs (cost-map/parse oops! clauses))
    (with-syntax ([(costs ...) costs] [default-cost-id (gensym)] [default-cost default-cost])
      #'(let ([default-cost-id default-cost])
          (make-cost-map (for/hash ([(op cost) (in-dict (list costs ...))])
                           (values op cost))
                         default-cost-id))))
  (syntax-case stx ()
    [(_ #:default-cost cost cl ...) (go (syntax->list #'(cl ...)) #'cost)]
    [(_ #:default-cost) (oops! "missing cost after `#:default-cost`")]
    [(_ cl ...) (go (syntax->list #'(cl ...)) #f)]
    [_ (oops! "bad syntax")]))

;; Scales all entries in a cost map by a factor.
(define (cost-map-scale s cm)
  (make-cost-map (for/hash ([(id c) (in-hash (cost-map-costs cm))])
                   (values id (* s c)))
                 (and (cost-map-default cm) (* s (cost-map-default cm)))))

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
          (define cost
            (or (hash-ref op->cost op #f)
                (cost-map-default costs)
                (error 'make-platform-product "unknown cost for `~a`" op)))
          `(,op (,@ireprs ,orepr) ,cost)))
      (make-platform impls #:optional? optional?)))
  (apply platform-union pforms))

(begin-for-syntax

  (define (platform-product/parse oops! clauses)
    (for/list ([clause (in-list clauses)])
      (syntax-case clause ()
        [(((type repr) ...) cost-map) #'(let ([t cost-map]) (list '((type . repr) ...) t))]
        [(bad _) (oops! "malformed type assignment" #'bad)]
        [_ (oops! "malformed clause" clause)]))))

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
    (with-syntax ([(clauses ...) (platform-product/parse oops! clauses)]
                  [operator-set oset]
                  [optional? optional?])
      #'(make-platform-product (list clauses ...) operator-set #:optional? optional?)))
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
     (for/fold ([pform pform-expr]) ([repr (list 'reprs ...)] [cost (list costs ...)])
       (struct-copy $platform
                    pform
                    [repr-costs
                     (hash-set (platform-repr-costs pform) (get-representation repr) cost)]))]))

; Implementation cost in a platform.
(define (platform-impl-cost pform impl)
  (hash-ref (platform-impl-costs pform)
            impl
            (lambda () (error 'platform-impl-cost "no cost for impl '~a" impl))))

; Representation (terminal) cost in a platform.
(define (platform-repr-cost pform repr)
  (hash-ref (platform-repr-costs pform)
            repr
            (lambda () (error 'platform-repr-cost "no cost for repr ~a" repr))))

; Cost model of a single node by a platform.
; Returns a procedure that must be called with the costs of the children.
(define (platform-node-cost-proc pform)
  (λ (expr repr)
    (match expr
      [(? literal?) (lambda () (platform-repr-cost pform repr))]
      [(? symbol?) (lambda () (platform-repr-cost pform repr))]
      [(list 'if _ _ _)
       (define if-cost (platform-impl-cost pform 'if))
       (lambda (cond-cost ift-cost iff-cost)
         (match if-cost
           [`(max ,n) (+ n cond-cost (max ift-cost iff-cost))]
           [`(sum ,n) (+ n cond-cost ift-cost iff-cost)]))]
      [(list impl args ...)
       (define impl-cost (platform-impl-cost pform impl))
       (lambda itype-costs
         (unless (= (length itype-costs) (length args))
           (error 'platform-node-cost-proc "arity mismatch, expected ~a arguments" (length args)))
         (apply + impl-cost itype-costs))])))

; Cost model parameterized by a platform.
(define (platform-cost-proc pform)
  (define bool-repr (get-representation 'bool))
  (define node-cost-proc (platform-node-cost-proc pform))
  (λ (expr repr)
    (let loop ([expr expr] [repr repr])
      (match expr
        [(? literal?) ((node-cost-proc expr repr))]
        [(? symbol?) ((node-cost-proc expr repr))]
        [(list 'if cond ift iff)
         (define cost-proc (node-cost-proc expr repr))
         (cost-proc (loop cond bool-repr) (loop ift repr) (loop iff repr))]
        [(list impl args ...)
         (define cost-proc (node-cost-proc expr repr))
         (define itypes (impl-info impl 'itype))
         (apply cost-proc (map loop args itypes))]))))

;; Rules from impl to spec (fixed for a particular platform)
(define-resetter *lifting-rules* (λ () (make-hash)) (λ () (make-hash)))

;; Rules from spec to impl (fixed for a particular platform)
(define-resetter *lowering-rules* (λ () (make-hash)) (λ () (make-hash)))

;; Synthesizes the LHS and RHS of lifting/lowering rules.
(define (impl->rule-parts impl)
  (define op (impl->operator impl))
  (cond
    [(operator-accelerator? op)
     (define spec (operator-info op 'spec))
     (match-define `(,(or 'lambda 'λ) (,vars ...) ,body) spec)
     (values vars body (cons impl vars))]
    [else
     (define itypes (operator-info op 'itype))
     (define vars (map (lambda (_) (gensym)) itypes))
     (values vars (cons op vars) (cons impl vars))]))

;; Synthesizes lifting rules for a given platform.
(define (platform-lifting-rules [pform (*active-platform*)])
  (define impls (platform-impls pform))
  (for/list ([impl (in-list impls)])
    (hash-ref! (*lifting-rules*)
               (cons impl pform)
               (lambda ()
                 (define name (sym-append 'lift- impl))
                 (define itypes (impl-info impl 'itype))
                 (define otype (impl-info impl 'otype))
                 (define-values (vars spec-expr impl-expr) (impl->rule-parts impl))
                 (rule name impl-expr spec-expr (map cons vars itypes) otype)))))

;; Synthesizes lowering rules for a given platform.
(define (platform-lowering-rules [pform (*active-platform*)])
  (define impls (platform-impls pform))
  (for/list ([impl (in-list impls)])
    (hash-ref! (*lowering-rules*)
               (cons impl pform)
               (lambda ()
                 (define op (impl->operator impl))
                 (define name (sym-append 'lower- impl))
                 (define itypes (operator-info op 'itype))
                 (define otype (operator-info op 'otype))
                 (define-values (vars spec-expr impl-expr) (impl->rule-parts impl))
                 (rule name spec-expr impl-expr (map cons vars itypes) otype)))))

;; All possible assignments of implementations.
(define (impl-combinations ops impls)
  (reap [sow]
        (let loop ([ops ops] [assigns '()])
          (match ops
            [(? null?) (sow assigns)]
            [(list 'if rest ...) (loop rest assigns)]
            [(list (? (curryr assq assigns)) rest ...) (loop rest assigns)]
            [(list op rest ...)
             (for ([impl (operator-all-impls op)])
               (when (set-member? impls impl)
                 (loop rest (cons (cons op impl) assigns))))]))))

;; Attempts to lower a specification to an expression using
;; a precomputed assignment of operator implementations.
;; Fails if the result is not well-typed.
(define (try-lower expr repr op->impl)
  (let/ec k
          (define env '())
          (define expr*
            (let loop ([expr expr] [repr repr])
              (match expr
                [(? symbol? x) ; variable
                 (match (dict-ref env x #f)
                   [#f (set! env (cons (cons x repr) env))]
                   [(? (curry equal? repr)) (k #f env)]
                   [_ (void)])
                 x]
                ; number
                [(? number? n) (literal n (representation-name repr))]
                [(list 'if cond ift iff) ; if expression
                 (list 'if (loop cond (get-representation 'bool)) (loop ift repr) (loop iff repr))]
                [(list op args ...) ; application
                 (define impl (dict-ref op->impl op))
                 (unless (equal? (impl-info impl 'otype) repr)
                   (k #f env))
                 (cons impl (map loop args (impl-info impl 'itype)))])))
          (define ctx (context (map car env) repr (map cdr env)))
          (values (and (equal? (repr-of expr* ctx) repr) expr*) env)))

;; Merges two variable -> value mappings.
;; If any mapping disagrees, the result is `#f`.
(define (merge-envs env1 env2)
  (let/ec k
          (for/fold ([env env1]) ([(x ty) (in-dict env2)])
            (match (dict-ref env x #f)
              [#f (cons (cons x ty) env)]
              [(? (curry equal? ty)) env]
              [_ (k #f)]))))

;; Synthesizes impl-to-impl rules for a given platform.
;; If a rule is over implementations, filters by supported implementations.
;; If a rule is over real operators, instantiates for every
;; possible implementation assignment.
(define (platform-impl-rules rules [pform (*active-platform*)])
  (define impls (list->seteq (platform-impls pform)))
  (reap [sow]
        (for ([ru (in-list rules)])
          (match-define (rule name input output _ otype) ru)
          (cond
            [(representation? otype) ; rule over representation
             (define ops (append (ops-in-expr input) (ops-in-expr output)))
             (when (andmap (lambda (op) (or (eq? op 'if) (set-member? impls op))) ops)
               (sow ru))]
            [else ; rules over types
             (define ops (append (ops-in-expr input) (ops-in-expr output)))
             (define isubsts (impl-combinations ops impls))
             (for* ([isubst (in-list isubsts)]
                    [repr (in-list (platform-reprs pform))]
                    #:when (equal? (representation-type repr) otype))
               (define-values (input* ienv) (try-lower input repr isubst))
               (define-values (output* oenv) (try-lower output repr isubst))
               (when (and input* output*)
                 (define itypes* (merge-envs ienv oenv))
                 (when itypes*
                   (define name* (sym-append name '_ (repr->symbol repr)))
                   (sow (rule name* input* output* itypes* repr)))))]))))
