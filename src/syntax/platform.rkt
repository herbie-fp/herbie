#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../core/rules.rkt"
         "matcher.rkt"
         "types.rkt" ; representation structure
         (only-in (submod "types.rkt" internals) make-representation)
         "syntax.rkt"
         (submod "syntax.rkt" internals))

(provide *active-platform*
         activate-platform!
         platform-lifting-rules
         platform-lowering-rules
         platform-copy
         repr-exists?
         get-representation
         prog->spec
         impl-exists?
         variable?
         constant-operator?
         impl-info
         get-fpcore-impl
         ;; Platform API
         ;; Operator sets
         (contract-out ;; Platforms
          [platform? (-> any/c boolean?)]
          [platform-name (-> platform? any/c)]
          [platform-if-cost (-> platform? any/c)]
          [platform-reprs (-> platform? (listof representation?))]
          [platform-impls (-> platform? (listof symbol?))]
          ; Cost model
          [platform-repr-cost (-> platform? any/c any/c)]
          [platform-node-cost-proc (-> platform? procedure?)]
          [platform-cost-proc (-> platform? procedure?)]))

(module+ internals
  (provide make-empty-platform
           platform-register-representation!
           platform-register-implementation!
           platform-register-implementations!
           display-platform
           register-platform!
           make-operator-impl
           make-representation))

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
(struct platform (name if-cost default-cost representations implementations)
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

;; Loads a platform.
(define (activate-platform! name)
  (define platform (hash-ref platforms name #f))

  (unless platform
    (raise-herbie-error "unknown platform `~a`, found (~a)"
                        name
                        (string-join (map ~a (hash-keys platforms)) ", ")))

  (*platform-name* name)
  (*active-platform* platform))

(define (platform-copy platform)
  (struct-copy $platform
               platform
               [representations (hash-copy (platform-representations platform))]
               [implementations (hash-copy (platform-implementations platform))]))

;; Registers a platform under identifier `name`.
(define (register-platform! platform)
  (define name (platform-name platform))
  (when (hash-has-key? platforms name)
    (error 'register-platform! "platform already registered ~a" name))
  (hash-set! platforms name (platform-copy platform)))

(define (make-empty-platform name #:if-cost [if-cost #f] #:default-cost [default-cost #f])
  (define reprs (make-hash))
  (define impls (make-hash))
  (when (hash-has-key? platforms name)
    (error 'make-empty-platform "platform with name ~a is already registered" name))
  (unless (or if-cost default-cost)
    (error 'make-empty-platform "Platform ~a is missing cost for if function" name))
  (set! if-cost (platform/parse-if-cost (or if-cost default-cost)))
  (create-platform name if-cost default-cost reprs impls))

(define (platform-register-representation! platform repr)
  (define reprs (platform-representations platform))
  ; Cost check
  (define repr-cost (representation-cost repr))
  (define default-cost (platform-default-cost platform))
  (unless (or repr-cost default-cost)
    (raise-herbie-error "Missing cost for representation ~a in platform ~a"
                        (representation-name repr)
                        (platform-name platform)))
  ; Duplicate check
  (when (hash-has-key? reprs (representation-name repr))
    (raise-herbie-error "Duplicate representation ~a in platform ~a"
                        (representation-name repr)
                        (platform-name platform)))
  ; Update table
  (hash-set! reprs (representation-name repr) repr))

(define (platform-register-implementation! platform impl)
  (unless impl
    (raise-herbie-error "Platform ~a missing implementation" (platform-name platform)))
  ; Reprs check
  (define reprs (platform-representations platform))
  (define otype (context-repr (operator-impl-ctx impl)))
  (define itype (context-var-reprs (operator-impl-ctx impl)))
  (define impl-reprs (map representation-name (remove-duplicates (cons otype itype))))
  (unless (andmap (curry hash-has-key? reprs) impl-reprs)
    (raise-herbie-error "Platform ~a missing representation of ~a implementation"
                        (platform-name platform)
                        (operator-impl-name impl)))
  ; Cost check
  (define impl-cost (operator-impl-cost impl))
  (define default-cost (platform-default-cost platform))
  (unless (or impl-cost default-cost)
    (raise-herbie-error "Missing cost for ~a" (operator-impl-name impl)))
  ; Dupicate check
  (define impls (platform-implementations platform))
  (when (hash-has-key? impls (operator-impl-name impl))
    (raise-herbie-error "Impl ~a is already registered in platform ~a"
                        (operator-impl-name impl)
                        (platform-name platform)))
  ; Update table
  (hash-set! impls (operator-impl-name impl) impl))

(define-syntax (platform-register-implementations! stx)
  (syntax-case stx ()
    [(_ platform ([name ([var : repr] ...) otype spec fl fpcore cost] ...))
     #'(begin
         (platform-register-implementation! platform
                                            (make-operator-impl (name [var : repr] ...)
                                                                otype
                                                                #:spec spec
                                                                #:fl fl
                                                                #:fpcore fpcore
                                                                #:cost cost)) ...)]))

;; Returns the representation associated with `name`
;; attempts to generate the repr if not initially found
(define (get-representation name)
  (define platform (*active-platform*))
  (define reprs (platform-representations platform))
  (or (hash-ref reprs name #f)
      (raise-herbie-error "Could not find support for ~a representation: ~a in a platform ~a"
                          name
                          (string-join (map ~s (hash-keys reprs)) ", ")
                          (platform-name platform))))

(define (repr-exists? name)
  (define platform (*active-platform*))
  (define reprs (platform-representations platform))
  (hash-has-key? reprs name))

;; Expression predicates ;;

(define (impl-exists? op)
  (define platform (*active-platform*))
  (define impls (platform-implementations platform))
  (hash-has-key? impls op))

(define (constant-operator? op)
  (and (symbol? op)
       (or (and (hash-has-key? operators op) (null? (operator-itype (hash-ref operators op))))
           (and (impl-exists? op) (null? (impl-info op 'vars))))))

(define (variable? var)
  (and (symbol? var)
       (or (not (hash-has-key? operators var))
           (not (null? (operator-itype (hash-ref operators var)))))
       (or (not (impl-exists? var)) (not (null? (impl-info var 'vars))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LImpl -> LSpec

;; Translates an LImpl to a LSpec.
(define (prog->spec expr)
  (match expr
    [(? literal?) (literal-value expr)]
    [(? variable?) expr]
    [(approx spec _) spec]
    [`(if ,cond ,ift ,iff)
     `(if ,(prog->spec cond)
          ,(prog->spec ift)
          ,(prog->spec iff))]
    [`(,impl ,args ...)
     (define vars (impl-info impl 'vars))
     (define spec (impl-info impl 'spec))
     (define env (map cons vars (map prog->spec args)))
     (pattern-substitute spec env)]))

(define (platform/parse-if-cost cost)
  (match cost
    [`(max ,x) `(max ,x)]
    [`(sum ,x) `(sum ,x)]
    [x `(max ,x)]))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (impl-info impl-name field)
  (-> symbol? (or/c 'name 'vars 'itype 'otype 'spec 'fpcore 'fl 'cost) any/c)
  (define impls (platform-implementations (*active-platform*)))
  (define impl
    (hash-ref impls
              impl-name
              (lambda ()
                (error 'impl-info
                       "unknown impl '~a in platform ~a"
                       impl-name
                       (platform-name (*active-platform*))))))
  (case field
    [(name) (operator-impl-name impl)]
    [(vars) (context-vars (operator-impl-ctx impl))]
    [(itype) (context-var-reprs (operator-impl-ctx impl))]
    [(otype) (context-repr (operator-impl-ctx impl))]
    [(spec) (operator-impl-spec impl)]
    [(fpcore) (operator-impl-fpcore impl)]
    [(fl) (operator-impl-fl impl)]
    [(cost) (or (operator-impl-cost impl) (platform-default-cost (*active-platform*)))]))

(define (platform-impls platform)
  (hash-keys (platform-implementations platform)))

(define (platform-reprs platform)
  (hash-values (platform-representations platform)))

; Representation (terminal) cost in a platform.
(define (platform-repr-cost platform repr)
  (define default-cost (platform-default-cost platform))
  (define repr-cost (representation-cost repr))
  (or repr-cost default-cost))

; Cost model of a single node by a platform.
; Returns a procedure that must be called with the costs of the children.
(define ((platform-node-cost-proc platform) expr repr)
  (match expr
    [(? literal?) (lambda () (platform-repr-cost platform repr))]
    [(? symbol?) (lambda () (platform-repr-cost platform repr))]
    [(list 'if _ _ _)
     (define if-cost (platform-if-cost platform))
     (lambda (cond-cost ift-cost iff-cost)
       (match if-cost
         [`(max ,n) (+ n cond-cost (max ift-cost iff-cost))]
         [`(sum ,n) (+ n cond-cost ift-cost iff-cost)]))]
    [(list impl args ...)
     (define impl-cost (impl-info impl 'cost))
     (lambda itype-costs
       (unless (= (length itype-costs) (length args))
         (error 'platform-node-cost-proc "arity mismatch, expected ~a arguments" (length args)))
       (apply + impl-cost itype-costs))]))

; Cost model parameterized by a platform.
(define (platform-cost-proc platform)
  (define node-cost-proc (platform-node-cost-proc platform))
  (λ (expr repr)
    (let loop ([expr expr]
               [repr repr])
      (match expr
        [(? literal?) ((node-cost-proc expr repr))]
        [(? symbol?) ((node-cost-proc expr repr))]
        [(approx _ impl) (loop impl repr)]
        [(list 'if cond ift iff)
         (define bool-repr (get-representation 'bool)) ; that's sketchy, bool repr might not exist
         (define cost-proc (node-cost-proc expr repr))
         (cost-proc (loop cond bool-repr) (loop ift repr) (loop iff repr))]
        [(list impl args ...)
         (define cost-proc (node-cost-proc expr repr))
         (define itypes (impl-info impl 'itype))
         (apply cost-proc (map loop args itypes))]))))

;; Rules from impl to spec (fixed for a particular platform)
(define/reset *lifting-rules* (make-hash))

;; Rules from spec to impl (fixed for a particular platform)
(define/reset *lowering-rules* (make-hash))

;; Synthesizes the LHS and RHS of lifting/lowering rules.
(define (impl->rule-parts impl)
  (define vars (impl-info impl 'vars))
  (define spec (impl-info impl 'spec))
  (values vars spec (cons impl vars)))

;; Synthesizes lifting rules for a platform platform.
(define (platform-lifting-rules [platform (*active-platform*)])
  (define impls (platform-impls platform))
  (for/list ([impl (in-list impls)])
    (hash-ref! (*lifting-rules*)
               (cons impl (platform-name platform))
               (lambda ()
                 (define name (sym-append 'lift- impl))
                 (define itypes (impl-info impl 'itype))
                 (define otype (impl-info impl 'otype))
                 (define-values (vars spec-expr impl-expr) (impl->rule-parts impl))
                 (rule name impl-expr spec-expr (map cons vars itypes) otype '(lifting))))))

;; Synthesizes lowering rules for a given platform.
(define (platform-lowering-rules [platform (*active-platform*)])
  (define impls (platform-impls platform))
  (append* (for/list ([impl (in-list impls)])
             (hash-ref!
              (*lowering-rules*)
              (cons impl (platform-name platform))
              (lambda ()
                (define name (sym-append 'lower- impl))
                (define-values (vars spec-expr impl-expr) (impl->rule-parts impl))
                (define itypes (map representation-type (impl-info impl 'itype)))
                (define otype (representation-type (impl-info impl 'otype)))
                (list (rule name spec-expr impl-expr (map cons vars itypes) otype '(lowering))
                      (rule (sym-append 'lower-unsound- impl)
                            (add-unsound spec-expr)
                            impl-expr
                            (map cons vars itypes)
                            otype
                            '(lowering))))))))

;; Extracts the `fpcore` field of an operator implementation
;; as a property dictionary and expression.
(define (impl->fpcore impl)
  (match (impl-info impl 'fpcore)
    [(list '! props ... body) (values (props->dict props) body)]
    [body (values '() body)]))

(define/reset op-hash #f)

;; For a given FPCore operator, rounding context, and input representations,
;; finds the best operator implementation. Panics if none can be found.
(define/contract (get-fpcore-impl op prop-dict ireprs)
  (-> symbol? prop-dict/c (listof representation?) (or/c symbol? #f))
  (unless (op-hash)
    (define h (make-hash))
    (for ([impl (in-list (platform-impls (*active-platform*)))])
      (define-values (_ expr) (impl->fpcore impl))
      (define expr*
        (if (symbol? expr)
            (list expr)
            expr))
      (when (list? expr*)
        (hash-update! h (car expr*) (curry cons impl) '())))
    (op-hash h))

  ; gather all implementations that have the same spec, input representations,
  ; and its FPCore translation has properties that are found in `prop-dict`
  (define impls
    (reap [sow]
          (for ([impl (in-list (hash-ref (op-hash) op '()))]
                #:when (equal? ireprs (impl-info impl 'itype)))
            (define-values (prop-dict* expr) (impl->fpcore impl))
            (define expr*
              (if (symbol? expr)
                  (list expr)
                  expr)) ; Handle named constants
            (define pattern (cons op (map (lambda (_) (gensym)) ireprs)))
            (when (and (subset? prop-dict* prop-dict) (pattern-match pattern expr*))
              (sow impl)))))
  ; check that we have any matching impls
  (cond
    [(null? impls) #f]
    [else
     ; we rank implementations and select the highest scoring one
     (define scores
       (for/list ([impl (in-list impls)])
         (define-values (prop-dict* _) (impl->fpcore impl))
         (define num-matching (count (lambda (prop) (member prop prop-dict*)) prop-dict))
         (cons num-matching (- (length prop-dict) num-matching))))
     ; select the best implementation
     ; sort first by the number of matched properties,
     ; then tie break on the number of extraneous properties
     (match-define (list (cons _ best) _ ...)
       (sort (map cons scores impls)
             (lambda (x y)
               (cond
                 [(> (car x) (car y)) #t]
                 [(< (car x) (car y)) #f]
                 [else (> (cdr x) (cdr y))]))
             #:key car))
     best]))

(define (display-platform platform)
  (define impls (platform-implementations platform))
  (define reprs (platform-representations platform))
  (define if-cost (platform-if-cost platform))
  (define default-cost (platform-default-cost platform))

  (printf "Platform: ~a;\n          if-cost: ~a;\n          default-cost: ~a\n\n"
          (platform-name platform)
          if-cost
          default-cost)

  (printf "Representations:\n")
  (define reprs-data
    (for/list ([(_ repr) (in-hash reprs)]
               [n (in-naturals)])
      (match-define (representation name type _ _ _ _ _ total-bits _ cost) repr)
      (unless cost
        (set! cost (format "~a (default cost)" default-cost)))
      (list n name type total-bits cost)))
  (write-table reprs-data (list "idx" "name" "type" "#bits" "cost"))

  (printf "\nImplementations\n")
  (define impls-data
    (for/list ([(_ impl) (in-hash impls)]
               [n (in-naturals)])
      (define name (operator-impl-name impl))
      (define itype (map representation-name (context-var-reprs (operator-impl-ctx impl))))
      (define otype (representation-name (context-repr (operator-impl-ctx impl))))
      (define spec (operator-impl-spec impl))
      (define cost (operator-impl-cost impl))
      (unless cost
        (set! cost (format "~a (default cost)" default-cost)))
      (list n name itype otype spec cost)))
  (write-table impls-data (list "idx" "name" "itype" "otype" "spec" "cost")))

(define (write-table data headers #:buffer-space [buffer-space 2])
  (define row-length (length (car data)))
  (define cell-widths (make-vector row-length 0))

  ; Measure cell-lengths
  (for ([header (in-list headers)]
        [i (in-naturals)])
    (vector-set! cell-widths
                 i
                 (max (+ (string-length header) buffer-space) (vector-ref cell-widths i))))
  (for ([row (in-list data)])
    (for ([elem row]
          [i (in-naturals)])
      (vector-set! cell-widths
                   i
                   (max (+ (string-length (~a elem)) buffer-space) (vector-ref cell-widths i)))))

  ; Header
  (printf "~a" (~a (list-ref headers 0) #:width (vector-ref cell-widths 0)))
  (for ([i (in-range 1 row-length)])
    (printf "|~a" (~a (list-ref headers i) #:width (vector-ref cell-widths i))))
  (printf "\n")
  (printf "~a" (~a "" #:width (vector-ref cell-widths 0) #:right-pad-string "-"))
  (for ([i (in-range 1 row-length)])
    (printf "+~a" (~a "" #:width (vector-ref cell-widths i) #:right-pad-string "-")))
  (printf "\n")

  ; Content
  (for ([row data])
    (printf "~a" (~a (list-ref row 0) #:width (vector-ref cell-widths 0)))
    (for ([i (in-range 1 row-length)])
      (printf "|~a" (~a (list-ref row i) #:width (vector-ref cell-widths i))))
    (printf "\n")))
