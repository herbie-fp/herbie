#lang racket

(require racket/runtime-path)
(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../config.rkt"
         "matcher.rkt"
         "types.rkt"
         "syntax.rkt"
         "../utils/float.rkt"
         "generators.rkt"
         "batch.rkt")

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
(struct platform (representations implementations representation-costs)
  #:name $platform
  #:constructor-name create-platform
  #:methods gen:custom-write
  [(define (write-proc p port mode)
     (fprintf port "#<platform>"))])

(provide *active-platform*
         platform-copy
         repr-exists?
         get-representation
         impl-exists?
         impl-info
         prog->spec
         batch-to-spec!
         get-fpcore-impl
         (struct-out $platform)
         ;; Platform API
         ;; Operator sets
         (contract-out [platform-reprs (-> platform? (listof representation?))]
                       [platform-impls (-> platform? (listof symbol?))]
                       [platform-repr-cost (-> platform? any/c any/c)]
                       [platform-node-cost-proc (-> platform? procedure?)]
                       [platform-cost-proc (-> platform? procedure?)])
         ; Platform creation
         make-empty-platform
         display-platform
         make-representation
         (all-from-out "generators.rkt"))

;; Active platform
(define *active-platform* (make-parameter #f))

(define (platform-copy platform)
  (struct-copy $platform
               platform
               [representations (hash-copy (platform-representations platform))]
               [implementations (hash-copy (platform-implementations platform))]))

(define (make-empty-platform)
  (define reprs (make-hash))
  (define repr-costs (make-hash))
  (define impls (make-hash))
  (create-platform reprs impls repr-costs))

;; Returns the representation associated with `name`
;; attempts to generate the repr if not initially found
(define (get-representation name)
  (define platform (*active-platform*))
  (define reprs (platform-representations platform))
  (or (hash-ref reprs name #f)
      (raise-herbie-error "Could not find support for ~a representation: ~a in a platform ~a"
                          name
                          (string-join (map ~s (hash-keys reprs)) ", ")
                          (*platform-name*))))

(define (repr-exists? name)
  (define platform (*active-platform*))
  (define reprs (platform-representations platform))
  (hash-has-key? reprs name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LImpl -> LSpec

;; Translates an LImpl to a LSpec.
(define (prog->spec expr)
  (match expr
    [(? literal? lit)
     (define repr (get-representation (literal-precision lit)))
     (match (representation-type repr)
       ['array
        (define elems (literal-value lit))
        (define elems-list
          (if (vector? elems)
              (vector->list elems)
              elems))
        `(array ,@elems-list)]
       [_ (literal-value lit)])]
    [(? symbol?) expr]
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

(define (batch-to-spec! batch brfs)
  (define lower
    (batch-recurse batch
                   (lambda (brf recurse)
                     (define node (deref brf))
                     (match node
                       [(? literal?)
                        (define repr (get-representation (literal-precision node)))
                        (match (representation-type repr)
                          ['array
                           (define elems (literal-value node))
                           (define elems-list
                             (if (vector? elems)
                                 (vector->list elems)
                                 elems))
                           (batch-add! batch (cons 'array elems-list))]
                          [_ (batch-push! batch (literal-value node))])]
                       [(? number?) brf]
                       [(? symbol?) brf]
                       [(hole _ spec) (recurse spec)]
                       [(approx spec _) (recurse spec)]
                       [(list (? impl-exists? impl) args ...)
                        (define vars (impl-info impl 'vars))
                        (define spec (impl-info impl 'spec))
                        (define env (map cons vars (map recurse args)))
                        (batch-add! batch (pattern-substitute spec env))]
                       [(list op args ...)
                        (batch-push! batch (cons op (map (compose batchref-idx recurse) args)))]))))
  (map lower brfs))

;; Expression predicates ;;

(define (impl-exists? op)
  (define platform (*active-platform*))
  (define impls (platform-implementations platform))
  (hash-has-key? impls op))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (impl-info impl-name field)
  (-> symbol? (or/c 'name 'vars 'itype 'otype 'spec 'fpcore 'fl 'cost 'aggregate) any/c)
  (define impls (platform-implementations (*active-platform*)))
  (define impl
    (hash-ref impls
              impl-name
              (lambda ()
                (error 'impl-info "unknown impl '~a in platform ~a" impl-name (*platform-name*)))))
  (case field
    [(name) (operator-impl-name impl)]
    [(vars) (context-vars (operator-impl-ctx impl))]
    [(itype) (context-var-reprs (operator-impl-ctx impl))]
    [(otype) (context-repr (operator-impl-ctx impl))]
    [(spec) (operator-impl-spec impl)]
    [(fpcore) (operator-impl-fpcore impl)]
    [(fl) (operator-impl-fl impl)]
    [(cost) (operator-impl-cost impl)]
    [(aggregate) (operator-impl-aggregate impl)]))

(define (platform-impls platform)
  (hash-keys (platform-implementations platform)))

(define (platform-reprs platform)
  (hash-values (platform-representations platform)))

; Representation (terminal) cost in a platform.
(define (platform-repr-cost platform repr)
  (define repr-costs (platform-representation-costs platform))
  (hash-ref repr-costs (representation-name repr)))

; Cost model of a single node by a platform.
; Returns a procedure that must be called with the costs of the children.
(define ((platform-node-cost-proc platform) expr repr)
  (match expr
    [(? literal?) (lambda () (platform-repr-cost platform repr))]
    [(? symbol?) (lambda () (platform-repr-cost platform repr))]
    [(list impl args ...)
     (define impl-cost (impl-info impl 'cost))
     (define impl-agg (impl-info impl 'aggregate))
     (lambda itype-costs
       (unless (= (length itype-costs) (length args))
         (error 'platform-node-cost-proc "arity mismatch, expected ~a arguments" (length args)))
       (+ impl-cost (apply impl-agg itype-costs)))]))

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
        [(list impl args ...)
         (define cost-proc (node-cost-proc expr repr))
         (define itypes (impl-info impl 'itype))
         (apply cost-proc (map loop args itypes))]))))

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
  (define repr-costs (platform-representation-costs platform))

  (displayln "Representations:")
  (define reprs-data
    (for/list ([repr (in-hash-values reprs)]
               [n (in-naturals)])
      (match-define (representation name type _ _ _ _ total-bits _) repr)
      (define cost (hash-ref repr-costs name))
      (list n name type total-bits cost)))
  (write-table reprs-data (list "idx" "name" "type" "#bits" "cost"))

  (displayln "\nImplementations")
  (define impls-data
    (for/list ([impl (in-hash-values impls)]
               [n (in-naturals)])
      (define name (operator-impl-name impl))
      (define itype (map representation-name (context-var-reprs (operator-impl-ctx impl))))
      (define otype (representation-name (context-repr (operator-impl-ctx impl))))
      (define spec (operator-impl-spec impl))
      (define cost (operator-impl-cost impl))
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
  (newline)
  (printf "~a" (~a "" #:width (vector-ref cell-widths 0) #:right-pad-string "-"))
  (for ([i (in-range 1 row-length)])
    (printf "+~a" (~a "" #:width (vector-ref cell-widths i) #:right-pad-string "-")))
  (newline)

  ; Content
  (for ([row data])
    (printf "~a" (~a (list-ref row 0) #:width (vector-ref cell-widths 0)))
    (for ([i (in-range 1 row-length)])
      (printf "|~a" (~a (list-ref row i) #:width (vector-ref cell-widths i))))
    (newline)))
