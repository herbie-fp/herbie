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
         "batch.rkt"
         "egg-herbie.rkt")

(provide prelude
         run-egglog-process
         (struct-out egglog-program)
         make-egglog-runner
         run-egglog-single-extractor
         run-egglog-multi-extractor
         run-egglog-proofs
         run-egglog-equal?)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;
;; High-level function that writes the program to a file, runs it then returns output
(define (run-egglog-process program-struct)
  (write-program-to-egglog (egglog-program-program program-struct))

  (process-egglog program-to-egglog))

;; Most calls to egglog should be done through this interface.
;;  - `make-egglog-runner`: creates a struct that describes a _reproducible_ egglog instance
;;  - `run-egglog`: takes an egglog runner and performs an extraction (exprs or proof)

;; Herbie's version of an egglog runner.
;; Defines parameters for running rewrite rules with egglog
(struct egglog-runner (batch roots reprs schedule ctx)
  #:transparent ; for equality
  #:methods gen:custom-write ; for abbreviated printing
  [(define (write-proc alt port mode)
     (fprintf port "#<egglog-runner>"))])

;; Constructs an egglog runner. Exactly same as egg-runner
;; But needs some amount of specifics - TODO
;;
;; The schedule is a list of pairs specifying
;;  - a list of rules
;;  - scheduling parameters:
;;     - node limit: `(node . <number>)`
;;     - iteration limit: `(iteration . <number>)`
;;     - constant fold: `(const-fold? . <boolean>)` [default: #t]
;;     - scheduler: `(scheduler . <name>)` [default: backoff]
;;        - `simple`: run all rules without banning
;;        - `backoff`: ban rules if the fire too much
(define (make-egglog-runner batch roots reprs schedule #:context [ctx (*context*)])
  (define (oops! fmt . args)
    (apply error 'verify-schedule! fmt args))
  ; verify the schedule
  (for ([instr (in-list schedule)])
    (match instr
      [(cons rules params)
       ;; `run` instruction
       (unless (and (list? rules) (andmap rule? rules))
         (oops! "expected list of rules: `~a`" rules))
       (for ([param (in-list params)])
         (match param
           [(cons 'node (? nonnegative-integer?)) (void)]
           [(cons 'iteration (? nonnegative-integer?)) (void)]
           [(cons 'const-fold? (? boolean?)) (void)]
           [(cons 'scheduler mode)
            (unless (set-member? '(simple backoff) mode)
              (oops! "in instruction `~a`, unknown scheduler `~a`" instr mode))]
           [_ (oops! "in instruction `~a`, unknown parameter `~a`" instr param)]))]
      [_ (oops! "expected `(<rules> . <params>)`, got `~a`" instr)]))
  ; make the runner
  (egglog-runner batch roots reprs schedule ctx))

;; 2. 4 types of run-egglog
;; Runs egg using an egg runner.
;;
;; Argument `cmd` specifies what to get from the e-graph:
;;  - single extraction: `(single . <extractor>)`
;;  - multi extraction: `(multi . <extractor>)`
;;  - proofs: `(proofs . ((<start> . <end>) ...))`

;; TODO : Need to run egglog to get the actual ids per
(define (run-egglog-single-extractor runner extractor) ; single expression extraction
  (define curr-batch (egg-runner-batch runner))

  ;; (Listof (Listof batchref))
  (define out
    (for/list ([root (batch-roots curr-batch)])
      (list (batchref curr-batch root))))

  out)

;; TODO : Need to run egglog to get the actual ids
;; very hard - per id recruse one level and ger simplest child
(define (run-egglog-multi-extractor runner extractor) ; multi expression extraction
  (define curr-batch (egg-runner-batch runner))

  ;; (Listof (Listof batchref))
  (define out
    (for/list ([root (batch-roots curr-batch)])
      (list (batchref curr-batch root))))

  out)

;; egglog does not have proof
;; there is some value that herbie has which indicates we could not
;; find a proof. Might be (list #f #f ....)
(define (run-egglog-proofs runner rws) ; proof extraction
  (for/list ([(start-expr end-expr) (in-dict rws)])
    #f))

; ; 1. ask within egglog program what is id
; ; 2. Extract expression from each expr
; ; TODO: if i have  two expressions how di i know if they are in the same e-class
; ; if we are outside of egglog
(define (run-egglog-equal? runner expr-pairs) ; term equality?
  (for/list ([(start-expr end-expr) (in-dict expr-pairs)])
    #f))

(define (prelude #:mixed-egraph? [mixed-egraph? #t])
  (load-herbie-builtins)
  (define pform (*active-platform*))
  (define spec-egraph
    `(datatype M
               (Num Rational :cost 4294967295)
               (Var String :cost 4294967295)
               (If M M M :cost 4294967295)
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
               ,@(platform-typed-nodes pform)))
  (define proj-fn `(function typed-id (M String) MTy))
  (define impl-rules (impl-proj-rules pform))
  (define num-rules (num-proj-rules))
  (define if-rules (if-proj-rules))
  '())

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
    (define typed-name (string-append (symbol->string (serialize-impl impl)) "Ty"))
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
  '()) ; TODO

(define (expr->egglog-expr expr)
  (let loop ([expr expr])
    (match expr
      [(? number?) `(Num (Rational ,expr 1))]
      [(? symbol?) `(Var ,(symbol->string expr))]
      [(list op args) `(,(hash-ref id->egglog op) ,@(map loop args))])))
