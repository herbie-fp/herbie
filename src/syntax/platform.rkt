#lang racket

(require racket/runtime-path)
(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../config.rkt"
         "matcher.rkt"
         "types.rkt"
         "syntax.rkt"
         "../syntax/float.rkt"
         "generators.rkt"
         "block.rkt")

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
         block-to-spec!
         get-fpcore-impl
         impl->fpcore
         reset-fpcore-op-cache!
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
         array-impl-name
         array-ref-impl-name
         ensure-array-impls!
         ensure-array-ref-impl!
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
  (match name
    [(? representation?) name]
    [`(array ,slots ...) (make-array-representation #:slots (map get-representation slots))]
    [_
     (or (hash-ref reprs name #f)
         (raise-herbie-error "Could not find support for ~a representation: ~a in a platform ~a"
                             name
                             (string-join (map ~s (hash-keys reprs)) ", ")
                             (*platform-name*)))]))

(define (repr-exists? name)
  (define platform (*active-platform*))
  (define reprs (platform-representations platform))
  (match name
    [(? representation?) #t]
    [`(array ,slots ...) (and (pair? slots) (andmap repr-exists? slots))]
    [_ (hash-has-key? reprs name)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LImpl -> LSpec

;; Translates an LImpl to a LSpec.
(define (prog->spec expr)
  (match expr
    [(? literal?) (literal-value expr)]
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

(define (block-to-spec! in-block out-block vs)
  (define lower
    (block-recurse
     in-block
     (lambda (v recurse)
       (define node (val-def v))
       (match node
         [(? literal?) (block-add! out-block (literal-value node))]
         [(? number?) (error 'block-to-spec! "unexpected spec node in input block: ~a" node)]
         [(? symbol?) (block-add! out-block node)]
         [(approx spec _) spec]
         [(list (? impl-exists? impl) args ...)
          (define vars (impl-info impl 'vars))
          (define spec (impl-info impl 'spec))
          (define env (map cons vars (map recurse args)))
          (block-add! out-block (pattern-substitute spec env))]
         [(list op args ...)
          (error 'block-to-spec! "unexpected spec node in input block: ~a" node)]))))
  (map lower vs))

(module+ test
  (require rackunit)

  (define test-empty-ctx (context '() #f '()))

  (let* ([in-block (block-empty test-empty-ctx)]
         [out-block (block-empty test-empty-ctx)]
         [x (block-add! in-block 'x)]
         [x* (first (block-to-spec! in-block out-block (list x)))])
    (check-equal? (val-block x*) out-block)
    (check-equal? (val-def x*) 'x))

  (let* ([block (block-empty test-empty-ctx)]
         [spec-block (block-empty test-empty-ctx)]
         [spec (block-add! spec-block 'x)]
         [impl (block-add! block (literal 1 'binary64))]
         [approx-v (block-add! block (approx spec impl))])
    (check-equal? (block-to-spec! block spec-block (list approx-v)) (list spec)))

  (let* ([in-block (block-empty test-empty-ctx)]
         [out-block (block-empty test-empty-ctx)]
         [spec (block-add! out-block 'x)]
         [impl (block-add! in-block (literal 1 'binary64))]
         [approx-v (block-add! in-block (approx spec impl))]
         [spec* (first (block-to-spec! in-block out-block (list approx-v)))])
    (check-equal? (val-block spec*) out-block)
    (check-equal? (val-def spec*) 'x))

  (let* ([in-block (block-empty test-empty-ctx)]
         [out-block (block-empty test-empty-ctx)]
         [num (block-add! in-block 1)]
         [expr (block-add! in-block `(+ ,num ,num))])
    (parameterize ([*active-platform* (make-empty-platform)])
      (check-exn #rx"unexpected spec node" (λ () (block-to-spec! in-block out-block (list num))))
      (check-exn #rx"unexpected spec node" (λ () (block-to-spec! in-block out-block (list expr)))))))

(define (impl-registered? name)
  (hash-has-key? (platform-implementations (*active-platform*)) name))

(define (repr-name->token name)
  (match name
    [(? symbol?) (symbol->string name)]
    [`(array ,slots ...) (format "array<~a>" (string-join (map repr-name->token slots) ":"))]
    [_ (raise-herbie-error "Cannot name representation ~a" name)]))

(define (array-token repr)
  (repr-name->token (representation-name repr)))

(define (array-impl-name repr)
  (string->symbol (array-token repr)))

(define (array-ref-impl-name repr idx)
  (string->symbol (format "ref.~a.~a" idx (array-token repr))))

(define (ensure-array-representation! repr)
  (define pform (*active-platform*))
  (define reprs (platform-representations pform))
  (define name (representation-name repr))
  (unless (hash-has-key? reprs name)
    (hash-set! reprs name repr)
    (hash-set! (platform-representation-costs pform)
               name
               (for/sum ([slot (in-list (array-representation-slots repr))])
                        (platform-repr-cost pform slot)))))

(define (register-array-impl! impl)
  (hash-set! (platform-implementations (*active-platform*)) (operator-impl-name impl) impl)
  (reset-fpcore-op-cache!))

(define (ensure-array-constructor! repr)
  (define name (array-impl-name repr))
  (unless (impl-registered? name)
    (define slots (array-representation-slots repr))
    (define vars
      (for/list ([i (in-range (length slots))])
        (string->symbol (format "x~a" i))))
    (define spec `(array ,@vars))
    (define cost (for/sum ([slot (in-list slots)]) (platform-repr-cost (*active-platform*) slot)))
    (register-array-impl! (operator-impl name
                                         (context vars repr slots)
                                         spec
                                         spec
                                         (procedure-reduce-arity (lambda args (list->vector args))
                                                                 (length vars))
                                         cost
                                         +)))
  name)

(define (ensure-array-accessor! repr idx)
  (define name (array-ref-impl-name repr idx))
  (unless (impl-registered? name)
    (define slots (array-representation-slots repr))
    (define spec `(ref t ,idx))
    (register-array-impl! (operator-impl name
                                         (context '(t) (list-ref slots idx) (list repr))
                                         spec
                                         spec
                                         (lambda (v) (vector-ref v idx))
                                         (platform-repr-cost (*active-platform*) (list-ref slots idx))
                                         +)))
  name)

(define (ensure-array-impls! repr)
  (define ctor-name (array-impl-name repr))
  (cond
    [(impl-registered? ctor-name) ctor-name]
    [else
     (for ([slot (in-list (array-representation-slots repr))]
           #:when (array-representation? slot))
       (ensure-array-impls! slot))
     (ensure-array-representation! repr)
     (ensure-array-constructor! repr)
     (for ([idx (in-range (length (array-representation-slots repr)))])
       (ensure-array-accessor! repr idx))
     ctor-name]))

(define (ensure-array-ref-impl! repr idx)
  (ensure-array-impls! repr)
  (array-ref-impl-name repr idx))

;; Inverse of `repr-name->token`; #f when the token is not a well-formed name.
(define (token->repr-name tok)
  (define (split-slots body)
    (let loop ([i 0]
               [depth 0]
               [start 0]
               [out '()])
      (cond
        [(= i (string-length body)) (and (zero? depth) (reverse (cons (substring body start i) out)))]
        [else
         (define c (string-ref body i))
         (cond
           [(char=? c #\<) (loop (add1 i) (add1 depth) start out)]
           [(char=? c #\>) (and (positive? depth) (loop (add1 i) (sub1 depth) start out))]
           [(and (char=? c #\:) (zero? depth))
            (loop (add1 i) depth (add1 i) (cons (substring body start i) out))]
           [else (loop (add1 i) depth start out)])])))
  (define (wrapped? prefix)
    (and (string-prefix? tok (string-append prefix "<")) (string-suffix? tok ">")))
  (define (body prefix)
    (substring tok (add1 (string-length prefix)) (sub1 (string-length tok))))
  (define (slots-of prefix)
    (define parts (split-slots (body prefix)))
    (and parts
         (pair? parts)
         (not (member "" parts))
         (let ([names (map token->repr-name parts)]) (and (andmap values names) names))))
  (cond
    [(wrapped? "array")
     (define names (slots-of "array"))
     (and names `(array ,@names))]
    [(zero? (string-length tok)) #f]
    [(or (string-contains? tok "<") (string-contains? tok ">") (string-contains? tok ":")) #f]
    [else (string->symbol tok)]))

;; Symbols that cannot name an array impl, so a repeated miss costs one eq lookup.
(define non-array-names (make-weak-hasheq))

;; Splits a name into its array token and, for an accessor, its slot index.
(define (array-impl-name-parts name)
  (cond
    [(not (symbol? name)) (values #f #f)]
    [(hash-ref non-array-names name #f) (values #f #f)]
    [else
     (define str (symbol->string name))
     (define-values (tok idx)
       (cond
         [(string-prefix? str "array<") (values str #f)]
         [(string-prefix? str "ref.")
          (define rest (substring str 4))
          (define dot
            (for/first ([i (in-range (string-length rest))]
                        #:when (char=? (string-ref rest i) #\.))
              i))
          (define n (and dot (string->number (substring rest 0 dot))))
          (if (and dot (exact-nonnegative-integer? n))
              (values (substring rest (add1 dot)) n)
              (values #f #f))]
         [else (values #f #f)]))
     (cond
       [(and tok (string-prefix? tok "array<")) (values tok idx)]
       [else
        (hash-set! non-array-names name #t)
        (values #f #f)])]))

;; Resolves a name to the shape it denotes, or #f. Never mutates the platform.
(define (array-impl-name->repr name)
  (define-values (tok idx) (array-impl-name-parts name))
  (define repr-name (and tok (token->repr-name tok)))
  (define repr
    (and repr-name
         (repr-exists? repr-name)
         (let ([r (get-representation repr-name)])
           (and (array-representation? r)
                (for/and ([slot (in-list (array-representation-slots r))])
                  (not (equal? (representation-type slot) 'bool)))
                r))))
  (cond
    [(not repr) (values #f #f)]
    [(not idx) (values repr #f)]
    [(< idx (length (array-representation-slots repr))) (values repr idx)]
    [else (values #f #f)]))

;; Impl names encode their whole shape, so a missing array impl can be rebuilt
;; from its name alone.
(define (array-impl-name? name)
  (define-values (repr _idx) (array-impl-name->repr name))
  (and repr #t))

(define (synthesize-array-impl! name)
  (define-values (repr idx) (array-impl-name->repr name))
  (and repr
       (begin
         (ensure-array-impls! repr)
         #t)))

;; Expression predicates ;;

(define (impl-exists? op)
  (define platform (*active-platform*))
  (define impls (platform-implementations platform))
  (or (hash-has-key? impls op) (array-impl-name? op)))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (impl-info impl-name field)
  (-> symbol? (or/c 'vars 'itype 'otype 'spec 'fpcore 'fl 'cost 'aggregate) any/c)
  (define impls (platform-implementations (*active-platform*)))
  (define impl
    (hash-ref
     impls
     impl-name
     (lambda ()
       (synthesize-array-impl! impl-name)
       (hash-ref
        impls
        impl-name
        (lambda ()
          (error 'impl-info "unknown impl '~a in platform ~a" impl-name (*platform-name*)))))))
  (case field
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
(define ((platform-node-cost-proc platform) expr)
  (match expr
    [(literal _ precision) (lambda () (platform-repr-cost platform (get-representation precision)))]
    [(? symbol?) (lambda () 0)]
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
  (λ (expr)
    (let loop ([expr expr])
      (match expr
        [(? literal?) ((node-cost-proc expr))]
        [(? symbol?) ((node-cost-proc expr))]
        [(approx _ impl) (loop impl)]
        [(list impl args ...)
         (define cost-proc (node-cost-proc expr))
         (apply cost-proc (map loop args))]))))

;; Extracts the `fpcore` field of an operator implementation
;; as a property dictionary and operation.
(define (impl->fpcore impl)
  (define-values (props body)
    (match (impl-info impl 'fpcore)
      [(list '! props ... body) (values (props->dict props) body)]
      [body (values '() body)]))
  (values props
          (if (symbol? body)
              (list body)
              body)))

(define/reset op-hash #f)

(define (reset-fpcore-op-cache!)
  (op-hash #f))

;; For a given FPCore operator, rounding context, and input representations,
;; finds the best operator implementation. Panics if none can be found.
(define/contract (get-fpcore-impl op prop-dict ireprs)
  (-> symbol? prop-dict/c (listof representation?) (or/c symbol? #f))
  (unless (op-hash)
    (define h (make-hash))
    (for ([impl (in-list (platform-impls (*active-platform*)))])
      (define-values (_ expr) (impl->fpcore impl))
      (when (list? expr)
        (hash-update! h (car expr) (curry cons impl) '())))
    (op-hash h))

  ; gather all implementations that have the same spec, input representations,
  ; and its FPCore translation has properties that are found in `prop-dict`
  (define impls
    (reap [sow]
          (for ([impl (in-list (hash-ref (op-hash) op '()))]
                #:when (equal? ireprs (impl-info impl 'itype)))
            (define-values (prop-dict* expr) (impl->fpcore impl))
            (define pattern (cons op (map (lambda (_) (gensym)) ireprs)))
            (when (and (subset? prop-dict* prop-dict) (pattern-match pattern expr))
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
