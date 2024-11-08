#lang racket

(require racket/file
         "programs.rkt"
         "rules.rkt"
         "../syntax/matcher.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../config.rkt"
         "../syntax/load-plugin.rkt"
         "../utils/timeline.rkt"
         "batch.rkt")

(provide prelude
         run-egglog
         (struct-out egglog-program)
         egglog-expr->expr
         egglog-add-exprs)

(module+ test
  (require rackunit)
  (require "../syntax/load-plugin.rkt")
  (load-herbie-builtins))

(define op-string-names
  (hash '+ 'Add '- 'Sub '* 'Mul '/ 'Div '== 'Eq '!= 'Neq '> 'Gt '< 'Lt '>= 'Gte '<= 'Lte))

(define id->egglog (make-hash))
(define egglog->id (make-hash))

;; [Copied from egg-herbie.rkt] Returns all representatations (and their types) in the current platform.
(define (all-repr-names [pform (*active-platform*)])
  (remove-duplicates (map (lambda (repr) (representation-name repr)) (platform-reprs pform))))

;; Track the entire Egglog program in one go by "converting" into racket based code
;; TODO : prelude, rules, expressions, extractions
(struct egglog-program (program) #:prefab)

(define program-to-egglog "program-to-egglog.egg")

; Types handled
; - rationals
; - string
(define (write-program-to-egglog program)
  (with-output-to-file program-to-egglog #:exists 'replace (lambda () (for-each writeln program))))

(define (process-egglog egglog-filename)
  (define egglog-path
    (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

  (define curr-path (build-path (current-directory) egglog-filename))

  (define-values (sp out in err) (subprocess #f #f #f egglog-path curr-path))

  (subprocess-wait sp)

  (define stdout-content (port->string out))
  (define stderr-content (port->string err))

  (close-input-port out)
  (close-output-port in)
  (close-input-port err)

  (cons stdout-content stderr-content))

;; High-level function that writes the program to a file, runs it then returns output
;;; TODO : Faster way to read/write from/to files
(define (run-egglog program-struct)
  (write-program-to-egglog (egglog-program-program program-struct))

  (process-egglog program-to-egglog))

(define (prelude #:mixed-egraph? [mixed-egraph? #t])
  (load-herbie-builtins)
  (define pform (*active-platform*))
  (define spec-egraph
    `(datatype M
               (Num Rational :cost 4294967295)
               (Var String :cost 4294967295)
               (If M M M :cost 4294967295)
               (Approx M M :cost 4294967295)
               ,@(platform-spec-nodes)
               ,@(platform-untyped-nodes pform)))
  (define typed-graph
    `(datatype MTy
               ,@(num-typed-nodes pform)
               ,@(var-typed-nodes pform)
               (IfTy MTy
                     MTy
                     MTy
                     :cost
                     ,(match (platform-impl-cost pform 'if)
                        [`(max ,n) n] ; Not quite right (copied from egg-herbie.rkt)
                        [`(sum ,n) n]))
                (ApproxTy M MTy :cost 0)
               ,@(platform-typed-nodes pform)))
  (hash-set! id->egglog 'if 'If)
  (hash-set! egglog->id 'IfTy 'if)
  (hash-set! id->egglog 'approx 'Approx)
  (hash-set! egglog->id 'ApproxTy 'approx)
  (define proj-fn `(function typed-id (M String) MTy))
  (define impl-rules (impl-proj-rules pform))
  (define num-rules (num-proj-rules))
  (define if-rules (if-proj-rules))
  (printf "~s\n" spec-egraph)
  (printf "~s\n" typed-graph)
  (printf "~s\n" proj-fn)
  (for ([rule (in-list impl-rules)])
    (printf "~s\n" rule))
  (for ([rule (in-list num-rules)])
    (printf "~s\n" rule))
  (for ([rule (in-list if-rules)])
    (printf "~s\n" rule))

  (define rules (append (*fp-safe-simplify-rules*) (real-rules (*simplify-rules*))))
  (define rewrite-rules (egglog-rewrite-rules rules))
  (for ([rule (in-list rewrite-rules)])
    (printf "~s\n" rule)))

(define (platform-spec-nodes)
  (for/list ([op (in-list (all-operators))])
    (hash-set! id->egglog op (serialize-op op))
    (define arity (length (operator-info op 'itype)))
    `(,(serialize-op op) ,@(for/list ([i (in-range arity)])
                             'M)
                         :cost
                         4294967295)))

(define (platform-untyped-nodes pform)
  (for/list ([impl (in-list (platform-impls pform))]
             #:when (string-contains? (symbol->string impl) "."))
    (define arity (length (impl-info impl 'itype)))
    (hash-set! id->egglog impl (serialize-impl impl))
    `(,(serialize-impl impl) ,@(for/list ([i (in-range arity)])
                                 'M)
                             :cost
                             4294967295)))

(define (platform-typed-nodes pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    (define typed-name (string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty")))
    (hash-set! egglog->id typed-name impl)
    `(,typed-name ,@(for/list ([i (in-range arity)])
                      'MTy)
                  :cost
                  ,(platform-impl-cost pform impl))))

(define (num-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(,(string->symbol (string-append "Num" (symbol->string repr)))
      Rational
      :cost
      ,(platform-repr-cost pform (get-representation repr)))))

(define (var-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))])
    `(,(string->symbol (string-append "Var" (symbol->string repr)))
      String
      :cost
      ,(platform-repr-cost pform (get-representation repr)))))

(define (num-proj-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (Num n)))
           ((let tx ,(symbol->string repr)
              )
            (let etx (,(string->symbol (string-append "Num" (symbol->string repr)))
                      n)
              )
            (union (typed-id e tx) etx)))))

(define (if-proj-rules)
  (for/list ([repr (in-list (all-repr-names))])
    `(rule ((= e (If ifc ift iff)) (= tifc (typed-id ifc "bool"))
                                   (= tift (typed-id ift ,(symbol->string repr)))
                                   (= tiff (typed-id iff ,(symbol->string repr))))
           ((let t0 ,(symbol->string repr)
              )
            (let et0 (IfTy
                      tifc
                      tift
                      tiff)
              )
            (union (typed-id e t0) et0)))))

(define (approx-proj-rules)
  (for/list ([repr (in-list (all-repr-names))])
    `(rule ((= e (Approx spec impl)) (= timpl (typed-id impl ,(symbol->string repr))))
           ((let t0 ,(symbol->string repr)
              )
            (let et0 (ApproxTy
                      spec
                      timpl)
              )
            (union (typed-id e t0) et0)))))

(define (impl-proj-rules pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    `(rule ((= e (,(serialize-impl impl) ,@(impl-info impl 'vars)))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "t" (symbol->string v)))
                    (typed-id ,v ,(symbol->string (representation-name vt))))))
           ((let t0 ,(symbol->string (representation-name (impl-info impl 'otype)))
              )
            (let et0 (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                      ,@(for/list ([v (in-list (impl-info impl 'vars))])
                          (string->symbol (string-append "t" (symbol->string v)))))
              )
            (union (typed-id e t0) et0)))))

(define (serialize-op op)
  (if (hash-has-key? op-string-names op)
      (hash-ref op-string-names op)
      (string->symbol (string-titlecase (symbol->string op)))))

(define (serialize-impl impl)
  (define impl-split (string-split (symbol->string impl) "."))
  (define op (string->symbol (car impl-split)))
  (define type
    (if (= 2 (length impl-split))
        (cadr impl-split)
        ""))
  (string->symbol (string-append (symbol->string (serialize-op op)) type)))

(define (rule->egglog-rule ru)
  `(rewrite ,(expr->egglog-pattern (rule-input ru)) ,(expr->egglog-pattern (rule-output ru)))) ; TODO

(define (expr->egglog-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(? number?) `(Num (rational ,(numerator expr) ,(denominator expr)))]
      [(? literal?)
       `(Num (rational ,(numerator (literal-value expr)) ,(denominator (literal-value expr))))]
      [(? symbol?) `(Var ,expr)]
      [(list op args ...) `(,(hash-ref id->egglog op) ,@(map loop args))])))

(define (egglog-rewrite-rules rules)
  (for/list ([rule (in-list rules)])
    `(rewrite ,(expr->egglog-pattern (rule-input rule)) ,(expr->egglog-pattern (rule-output rule)))))

(define (egglog-add-exprs batch ctx)
  (define insert-batch (batch-remove-zombie batch (batch-roots batch)))
  (define mappings (build-vector (batch-length insert-batch) values))
  (define bindings (make-hash))
  (define (remap x)
    (vector-ref mappings x))

  ; node -> egglog node binding
  ; inserts an expression into the e-graph, returning binding variable.
  (define (insert-node! node n root?)
    (define binding
      (if root?
          (string->symbol (format "?r~a" n))
          (string->symbol (format "?b~a" n))))
    (hash-set! bindings binding node)
    binding)

  (define root-bindings '())
  ; Inserting nodes bottom-up
  (define root-mask (make-vector (batch-length insert-batch) #f))
  (for ([root (in-vector (batch-roots insert-batch))])
    (vector-set! root-mask root #t))
  (for ([node (in-vector (batch-nodes insert-batch))]
        [root? (in-vector root-mask)]
        [n (in-naturals)])
    (define node*
      (match node
        [(literal v _) `(Num (rational ,(numerator v) ,(denominator v)))]
        [(? number?) `(Num (rational ,(numerator node) ,(denominator node)))]
        [(? symbol?) `(Var ,(symbol->string node))]
        [(approx spec impl) `(Approx ,(symbol->string 'APRROXTEST))]
        [(list impl args ...) `(,(hash-ref id->egglog impl) ,@(map remap args))]))
      (vector-set! mappings n (insert-node! node* n root?))

    (when root?
      (set! root-bindings (cons (vector-ref mappings n) root-bindings))))

  (define binding-exprs
    (for/list ([root? (in-vector root-mask)]
               [n (in-naturals)])
      (define binding
        (if root?
            (string->symbol (format "?r~a" n))
            (string->symbol (format "?b~a" n))))
      `(let ,binding ,(hash-ref bindings binding))))

  (for ([binding-expr (in-list binding-exprs)])
    (printf "~s\n" binding-expr))

  ; Var rules
  (define var-rules
    (for/list ([var (in-list (context-vars ctx))]
               [repr (in-list (context-var-reprs ctx))])
      `(rule ((= e (Var ,(symbol->string var))))
             ((let ty ,(symbol->string (representation-name repr))
                )
              (let ety (,(string->symbol (string-append "Var" (symbol->string (representation-name repr))))
                ,(symbol->string var)))
              (union (typed-id e ty) ety)))))

  (for ([var-rule (in-list var-rules)])
    (printf "~s\n" var-rule))

  (printf "~s\n" `(run 10))

  (define extract-exprs
    (for/list ([root (in-list root-bindings)])
      `(extract (typed-id ,root ,(symbol->string (representation-name (context-repr ctx)))))))

  (for ([extract-expr (in-list extract-exprs)])
    (printf "~s\n" extract-expr))
  (void))

(define (egglog-num? id)
  (string-prefix? (symbol->string id) "Num"))

(define (egglog-num-repr id)
  (string->symbol (substring (symbol->string id) 3)))

(define (egglog-var? id)
  (string-prefix? (symbol->string id) "Var"))

(define (egglog-expr-typed? expr)
  (match expr
    [(? number?) #t]
    [(? variable?) #t]
    [`(,impl ,args ...) (and (not (eq? impl 'typed-id)) (andmap egglog-expr-typed? args))]))

(define (egglog-expr->expr expr)
  (let loop ([expr expr])
    (match expr
      [`(,(? egglog-num? num) (rational ,n 1)) (literal n (egglog-num-repr num))]
      [`(,(? egglog-var? var) ,v) (string->symbol v)]
      [`() (approx)]
      [`(,impl ,args ...) `(,(hash-ref egglog->id impl) ,@(map loop args))])))
