#lang racket

(require racket/file
         "rules.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../config.rkt"
         "../syntax/load-plugin.rkt"
         "batch.rkt"
         "egglog-program.rkt")

(provide (struct-out egglog-runner)
         prelude
         egglog-add-exprs
         make-egglog-runner
         run-egglog-multi-extractor
         e2->expr
         e1->expr
         egglog-var?
         serialize-op
         e1->id
         e2->id)

(define op-string-names
  (hash '+ 'Add '- 'Sub '* 'Mul '/ 'Div '== 'Eq '!= 'Neq '> 'Gt '< 'Lt '>= 'Gte '<= 'Lte))

(define/reset id->e1 (make-hasheq))
(define/reset e1->id (make-hasheq))
(define/reset id->e2 (make-hasheq))
(define/reset e2->id (make-hasheq))

;; [Copied from egg-herbie.rkt] Returns all representatations (and their types) in the current platform.
(define (all-repr-names [pform (*active-platform*)])
  (remove-duplicates (map (lambda (repr) (representation-name repr)) (platform-reprs pform))))

; Types handled
; - rationals
; - string

;; High-level function that writes the program to a file, runs it then returns output
(define (process-egglog program)
  (define curr-program (get-current-program program))

  (define egglog-file-path
    (let ([temp-file (make-temporary-file "program-to-egglog-~a.egg")])
      (with-output-to-file temp-file #:exists 'replace (lambda () (for-each writeln curr-program)))
      temp-file))

  (define egglog-path
    (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

  (define stdout-port (open-output-string))
  (define stderr-port (open-output-string))

  (define old-error-port (current-error-port))

  ;; Run egglog and capture output
  (parameterize ([current-output-port stdout-port]
                 [current-error-port stderr-port])
    (unless (system (format "~a ~a" egglog-path egglog-file-path))
      (begin
        (fprintf old-error-port "stdout-port ~a\n" (get-output-string stdout-port))
        ; Tail the last 100 lines of the error instead of everything
        (fprintf old-error-port
                 "stderr-port ~a\n"
                 (string-join (take-right (string-split (get-output-string stderr-port) "\n") 100)
                              "\n"))
        (fprintf old-error-port "incorrect program in ~a\n" egglog-file-path)
        (error "Failed to execute egglog"))))

  (delete-file egglog-file-path)

  (cons (get-output-string stdout-port) (get-output-string stderr-port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
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

;; Constructs an egglog runner - structurally serves the same purpose as egg-runner
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
       (unless (or (equal? `lift rules)
                   (equal? `lower rules)
                   (and (list? rules) (andmap rule? rules)))
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

;; Runs egglog using an egglog runner by extracting multiple variants
(define (run-egglog-multi-extractor runner
                                    batch
                                    #:num-variants [num-variants #t]) ; multi expression extraction

  (define curr-batch (batch-remove-zombie batch))
  (define curr-program (make-egglog-program))

  ;; 1. Add the Prelude
  (prelude curr-program #:mixed-egraph? #t)

  ;; 2. User Rules which comes from schedule (need to be translated)
  (define tag-schedule
    (for/list ([i (in-naturals 1)]
               [element (in-list (egglog-runner-schedule runner))])

      (define rule-type (car element))
      (define schedule-params (cdr element))
      (define tag
        (match rule-type
          ['lift 'lifting]
          ['lower 'lowering]
          [_
           (define curr-tag (string->symbol (string-append "?tag" (number->string i))))
           ;; Add rulesets
           (egglog-program-add! `(ruleset ,curr-tag) curr-program)

           ;; Add the actual egglog rewrite rules
           (egglog-program-add-list! (egglog-rewrite-rules rule-type curr-tag) curr-program)

           curr-tag]))

      (cons tag schedule-params)))

  ;; 3. Inserting expressions into the egglog program and getting a Listof (exprs . extract bindings)
  (define extract-bindings (egglog-add-exprs curr-batch (egglog-runner-ctx runner) curr-program))

  ;; 4. Running the schedule : having code inside to emulate egraph-run-rules
  (define run-schedule '())
  (define schedule-lower #f)
  (define schedule-lift #f)

  (for ([(tag schedule-params) (in-dict tag-schedule)])
    (match tag
      ['lifting (set! schedule-lift #t)]
      ['lowering (set! schedule-lower #t)]
      [_ (void)]))

  ; First lifting
  (when schedule-lift
    (set! run-schedule (append run-schedule (list `(saturate lifting)))))

  ; Then math rules tag
  (for ([(tag schedule-params) (in-dict tag-schedule)])
    (match tag
      [(or 'lifting 'lowering) (void)]
      [_
       ;; Get the best iter limit by looking at the program from scratch
       (define-values (best-iter-limit)
         (egglog-unsound-detected curr-program tag schedule-params schedule-lower schedule-lift))

       (set! run-schedule (append run-schedule `((repeat ,best-iter-limit ,tag))))]))

  ; Last lowering
  (when schedule-lower
    (set! run-schedule (append run-schedule (list `(saturate lowering)))))

  (egglog-program-add! `(run-schedule ,@run-schedule) curr-program)

  ;; 5. Extraction -> should just need root ids
  (egglog-program-add-list! (for/list ([binding extract-bindings])
                              `(extract ,binding 100))
                            curr-program)

  ;; 6. After step-by-step building the program, process it
  ;; by running it using egglog
  (define egglog-output (process-egglog curr-program))

  ;; Extract its returned value
  (define stdout-content (car egglog-output))

  (define input-batch curr-batch)
  (define out (batch->mutable-batch input-batch))

  ;; (Listof (Listof exprs))
  (define herbie-exprss
    (let ([input-port (open-input-string stdout-content)])
      (for/list ([next-expr (in-port read input-port)])
        (if num-variants
            (map e2->expr next-expr)
            (list (e2->expr next-expr))))))

  (define result
    (for/list ([variants (in-list herbie-exprss)])
      (remove-duplicates
       (for/list ([v (in-list variants)])
         (egglog->batchref v input-batch out (context-repr (egglog-runner-ctx runner))))
       #:key batchref-idx)))

  (batch-copy-mutable-nodes! input-batch out)

  ;; (Listof (Listof batchref))
  result)

(define (egglog->batchref expr input-batch out type)
  (define idx
    (let loop ([expr expr]
               [type type])
      (define term
        (match expr
          [(? number?)
           (if (representation? type)
               (literal expr (representation-name type))
               expr)]
          [(? symbol?) expr]
          [`(if ,cond ,ift ,iff)
           (if (representation? type)
               (list 'if (loop cond (get-representation 'bool)) (loop ift type) (loop iff type))
               (list 'if (loop cond 'bool) (loop ift type) (loop iff type)))]
          [(approx spec impl) (approx (loop spec #f) (loop impl type))]
          [(list (? impl-exists? impl) args ...) (cons impl (map loop args (impl-info impl 'itype)))]
          [(list op args ...)
           (define args*
             (for/list ([arg (in-list args)])
               (loop arg #f)))
           (cons op args*)]))
      (mutable-batch-push! out term)))
  (batchref input-batch idx))

(define (normalize-cost c min-cost)
  (exact-round (* (/ c min-cost) 100)))

(define (prelude curr-program #:mixed-egraph? [mixed-egraph? #t])
  (load-herbie-builtins)
  (define pform (*active-platform*))

  (define spec-egraph
    `(datatype M
               (Num BigRat :cost 4294967295)
               (Var String :cost 4294967295)
               (If M M M :cost 4294967295)
               ,@(platform-spec-nodes)))

  (egglog-program-add! spec-egraph curr-program)

  ;;; To add support for floating point cost (which egglog does not support), compute
  ;;; the minimum by accumulating all raw costs and normalize them
  (define raw-costs '())

  ;; Add raw num-typed-nodes and var-typed-nodes costs
  (for ([repr (in-list (all-repr-names))])
    (set! raw-costs (cons (platform-repr-cost pform (get-representation repr)) raw-costs)))

  ;; Add raw if-cost
  (set! raw-costs
        (cons (match (platform-impl-cost pform 'if)
                [`(max ,n) n] ; Not quite right (copied from egg-herbie.rkt)
                [`(sum ,n) n])
              raw-costs))

  ;; Add raw platform-impl-nodes
  (for ([impl (in-list (platform-impls pform))])
    (set! raw-costs (cons (platform-impl-cost pform impl) raw-costs)))

  (define min-cost (apply min raw-costs))

  (define typed-graph
    `(datatype MTy
               ,@(num-typed-nodes pform min-cost)
               ,@(var-typed-nodes pform min-cost)
               (IfTy MTy
                     MTy
                     MTy
                     :cost
                     ,(normalize-cost (match (platform-impl-cost pform 'if)
                                        [`(max ,n) n] ; Not quite right (copied from egg-herbie.rkt)
                                        [`(sum ,n) n])
                                      min-cost))
               (Approx M MTy)
               ,@(platform-impl-nodes pform min-cost)))
  (egglog-program-add! typed-graph curr-program)

  (egglog-program-add! `(constructor lower (M String) MTy :unextractable) curr-program)

  (egglog-program-add! `(constructor lift (MTy) M :unextractable) curr-program)

  (egglog-program-add! `(ruleset const-fold) curr-program)

  (egglog-program-add! `(ruleset lowering) curr-program)

  (egglog-program-add! `(ruleset lifting) curr-program)

  ;;; Adding function unsound before rules

  ;; unsound functions
  (egglog-program-add! `(function unsound () bool :merge (or old new)) curr-program)
  (egglog-program-add! `(ruleset unsound-rule) curr-program)
  (egglog-program-add! `(set (unsound) false) curr-program)

  (egglog-program-add!
   `(rule ((= (Num c1) (Num c2)) (!= c1 c2)) ((set (unsound) true)) :ruleset unsound-rule)
   curr-program)

  (egglog-program-add! `(let ?one (Num
                                   [bigrat
                                    (from-string "1")
                                    (from-string "1")])
                          )
                       curr-program)
  (egglog-program-add! `(let ?two (Num
                                   [bigrat
                                    (from-string "2")
                                    (from-string "1")])
                          )
                       curr-program)

  (for ([curr-expr const-fold])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (impl-lowering-rules pform)])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (impl-lifting-rules pform)])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (num-lowering-rules)])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (num-lifting-rules)])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (if-lowering-rules)])
    (egglog-program-add! curr-expr curr-program))

  (egglog-program-add! (if-lifting-rule) curr-program)

  (egglog-program-add! (approx-lifting-rule) curr-program)

  (void))

(define const-fold
  `((let ?zero (bigrat
                [from-string "0"]
                [from-string "1"])
      )
    (rewrite (Add (Num x) (Num y)) (Num (+ x y)) :ruleset const-fold)
    (rewrite (Sub (Num x) (Num y)) (Num (- x y)) :ruleset const-fold)
    (rewrite (Mul (Num x) (Num y)) (Num (* x y)) :ruleset const-fold)
    ;(rule ((= e (Div (Num x) (Num y))) (!= ?zero y)) ((union e (Num (/ x y)))) :ruleset const-fold)
    (rewrite (Neg (Num x)) (Num (neg x)) :ruleset const-fold)
    (rule ((= e (Pow (Num x) (Num y))) (= ?zero x) (> y ?zero))
          ((union e (Num ?zero)))
          :ruleset
          const-fold)
    (rule ((= e (Pow (Num x) (Num y))) (= ?zero y) (!= ?zero x))
          ((union e (Num (bigrat (from-string "1") (from-string "1")))))
          :ruleset
          const-fold)
    (rule ((= e (Pow (Num x) (Num y))) (> y ?zero) (!= ?zero x) (= y (round y)))
          ((union e (Num (pow x y))))
          :ruleset
          const-fold)
    (rule ((= e (Log (Num x))) (= (numer x) (denom x))) ((union e (Num ?zero))) :ruleset const-fold)
    (rule ((= e (Cbrt (Num x))) (= (numer x) (denom x)))
          ((union e (Num (bigrat (from-string "1") (from-string "1")))))
          :ruleset
          const-fold)
    (rewrite (Fabs (Num x)) (Num (abs x)) :ruleset const-fold)
    (rewrite (Floor (Num x)) (Num (floor x)) :ruleset const-fold)
    (rewrite (Ceil (Num x)) (Num (ceil x)) :ruleset const-fold)
    (rewrite (Round (Num x)) (Num (round x)) :ruleset const-fold)))

(define (platform-spec-nodes)
  (for/list ([op (in-list (all-operators))])
    (hash-set! (id->e1) op (serialize-op op))
    (hash-set! (e1->id) (serialize-op op) op)
    (define arity (length (operator-info op 'itype)))
    `(,(serialize-op op) ,@(for/list ([i (in-range arity)])
                             'M)
                         :cost
                         4294967295)))

(define (platform-impl-nodes pform min-cost)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    (define typed-name (string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty")))
    (hash-set! (id->e2) impl typed-name)
    (hash-set! (e2->id) typed-name impl)
    `(,typed-name ,@(for/list ([i (in-range arity)])
                      'MTy)
                  :cost
                  ,(normalize-cost (platform-impl-cost pform impl) min-cost))))

(define (typed-num-id repr-name)
  (string->symbol (string-append "Num" (symbol->string repr-name))))

(define (typed-var-id repr-name)
  (string->symbol (string-append "Var" (symbol->string repr-name))))

(define (num-typed-nodes pform min-cost)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(,(typed-num-id repr) BigRat
                           :cost
                           ,(normalize-cost (platform-repr-cost pform (get-representation repr))
                                            min-cost))))

(define (var-typed-nodes pform min-cost)
  (for/list ([repr (in-list (all-repr-names))])
    `(,(typed-var-id repr) String
                           :cost
                           ,(normalize-cost (platform-repr-cost pform (get-representation repr))
                                            min-cost))))

(define (num-lowering-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (Num n)))
           ((let tx ,(symbol->string repr)
              )
            (let etx (,(typed-num-id repr)
                      n)
              )
            (union (lower e tx) etx))
           :ruleset
           lowering)))

(define (num-lifting-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (,(typed-num-id repr) n)))
           ((let se (Num
                     n)
              )
            (union (lift e) se))
           :ruleset
           lifting)))

(define (if-lowering-rules)
  (for/list ([repr (in-list (all-repr-names))])
    `(rule ((= e (If ifc ift iff)) (= tifc (lower ifc "bool"))
                                   (= tift (lower ift ,(symbol->string repr)))
                                   (= tiff (lower iff ,(symbol->string repr))))
           ((let t0 ,(symbol->string repr)
              )
            (let et0 (IfTy
                      tifc
                      tift
                      tiff)
              )
            (union (lower e t0) et0))
           :ruleset
           lowering)))

(define (if-lifting-rule)
  `(rule ((= e (IfTy ifc ift iff)) (= sifc (lift ifc)) (= sift (lift ift)) (= siff (lift iff)))
         ((let se (If
                   sifc
                   sift
                   siff)
            )
          (union (lift e) se))
         :ruleset
         lifting))

(define (approx-lifting-rule)
  `(rule ((= e (Approx spec impl))) ((union (lift e) spec)) :ruleset lifting))

(define (impl-lowering-rules pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define spec-expr (impl-info impl 'spec))
    (define arity (length (impl-info impl 'itype)))
    `(rule ((= e ,(expr->egglog-spec-serialized spec-expr ""))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "t" (symbol->string v)))
                    (lower ,v ,(symbol->string (representation-name vt))))))
           ((let t0 ,(symbol->string (representation-name (impl-info impl 'otype)))
              )
            (let et0 (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                      ,@(for/list ([v (in-list (impl-info impl 'vars))])
                          (string->symbol (string-append "t" (symbol->string v)))))
              )
            (union (lower e t0) et0))
           :ruleset
           lowering)))

(define (impl-lifting-rules pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define spec-expr (impl-info impl 'spec))
    (define op (string->symbol (car (string-split (symbol->string impl) "."))))
    (define arity (length (impl-info impl 'itype)))
    `(rule ((= e
               (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                ,@(impl-info impl 'vars)))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "s" (symbol->string v))) (lift ,v))))
           ((let se ,(expr->egglog-spec-serialized spec-expr "s")
              )
            (union (lift e) se))
           :ruleset
           lifting)))

(define (expr->egglog-spec-serialized expr s)
  (let loop ([expr expr])
    (match expr
      [(? number?)
       `(Num (bigrat (from-string ,(number->string (numerator expr)))
                     (from-string ,(number->string (denominator expr)))))]
      [(? symbol?) (string->symbol (string-append s (symbol->string expr)))]
      [(list op args ...)
       `(,(hash-ref (if (hash-has-key? (id->e1) op)
                        (id->e1)
                        (id->e2))
                    op)
         ,@(map loop args))])))

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

(define (expr->e2-pattern expr repr)
  (let loop ([expr expr]
             [repr repr])
    (match expr
      [(? literal?)
       `(,(typed-num-id (representation-name repr))
         (bigrat (from-string ,(number->string (numerator (literal-value expr))))
                 (from-string ,(number->string (denominator (literal-value expr))))))]
      [(? symbol?) expr]
      [`(if ,cond ,ift ,iff) `(IfTy ,(loop cond repr) ,(loop ift repr) ,(loop iff repr))]
      [(list op args ...)
       `(,(hash-ref (id->e2) op) ,@(for/list ([arg (in-list args)]
                                              [itype (in-list (impl-info op 'itype))])
                                     (loop arg itype)))])))

(define (expr->e1-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(? number?)
       `(Num (bigrat (from-string ,(number->string (numerator expr)))
                     (from-string ,(number->string (denominator expr)))))]
      [(? symbol?) expr]
      [`(if ,cond ,ift ,iff) `(If ,(loop cond) ,(loop ift) ,(loop iff))]
      [(list op args ...) `(,(hash-ref (id->e1) op) ,@(map loop args))])))

(define (expr->e1-expr expr)
  (let loop ([expr expr])
    (match expr
      [(? number?)
       `(Num (bigrat (from-string ,(number->string (numerator expr)))
                     (from-string ,(number->string (denominator expr)))))]
      [(? symbol?) `(Var ,(symbol->string expr))]
      [`(if ,cond ,ift ,iff) `(If ,(loop cond) ,(loop ift) ,(loop iff))]
      [(list op args ...) `(,(hash-ref (id->e1) op) ,@(map loop args))])))

(define (egglog-rewrite-rules rules tag)
  (for/list ([rule (in-list rules)]
             #:when (not (symbol? (rule-input rule))))
    (if (not (representation? (rule-otype rule)))
        `(rewrite ,(expr->e1-pattern (rule-input rule))
                  ,(expr->e1-pattern (rule-output rule))
                  :ruleset
                  ,tag)
        `(rewrite ,(expr->e2-pattern (rule-input rule) (rule-otype rule))
                  ,(expr->e2-pattern (rule-output rule) (rule-otype rule))
                  :ruleset
                  ,tag))))

(define (egglog-add-exprs batch ctx curr-program)
  (define mappings (build-vector (batch-length batch) values))
  (define bindings (make-hash))
  (define vars (make-hash))
  (define (remap x spec?)
    (cond
      [(hash-has-key? vars x)
       (if spec?
           (string->symbol (format "?~a" (hash-ref vars x)))
           (string->symbol (format "?t~a" (hash-ref vars x))))]
      [else (vector-ref mappings x)]))

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
  (define root-mask (make-vector (batch-length batch) #f))
  (define spec-mask (make-vector (batch-length batch) #f))

  (for ([n (in-range (batch-length batch))])
    (define node (vector-ref (batch-nodes batch) n))
    (match node
      [(? literal?) (vector-set! spec-mask n #f)] ;; If literal, not a spec
      [(? number?) (vector-set! spec-mask n #t)] ;; If number, it's a spec
      [(? symbol?)
       (vector-set!
        spec-mask
        n
        #f)] ;; If symbol, assume not a spec could be either (find way to distinguish) : PREPROCESS
      [(hole _ _) (vector-set! spec-mask n #f)] ;; If hole, not a spec
      [(approx _ _) (vector-set! spec-mask n #f)] ;; If approx, not a spec

      [(list appl args ...)
       (if (hash-has-key? (id->e1) appl)
           (vector-set! spec-mask n #t) ;; appl with op -> Is a spec
           (vector-set! spec-mask n #f))] ;; appl impl -> Not a spec

      ;; If the condition or any branch is a spec, then this is a spec
      [`(if ,cond ,ift ,iff) (vector-set! spec-mask n (vector-ref spec-mask cond))]))

  (for ([root (in-vector (batch-roots batch))])
    (vector-set! root-mask root #t))
  (for ([node (in-vector (batch-nodes batch))]
        [root? (in-vector root-mask)]
        [spec? (in-vector spec-mask)]
        [n (in-naturals)])
    (define node*
      (match node
        [(literal v repr)
         `(,(typed-num-id repr) (bigrat (from-string ,(number->string (numerator v)))
                                        (from-string ,(number->string (denominator v)))))]
        [(? number?)
         `(Num (bigrat (from-string ,(number->string (numerator node)))
                       (from-string ,(number->string (denominator node)))))]
        [(? symbol?) #f]
        [`(if ,cond ,ift ,iff)
         (list (if spec? 'If 'IfTy) (remap cond spec?) (remap ift spec?) (remap iff spec?))]
        [(approx spec impl) `(Approx ,(remap spec #t) ,(remap impl #f))]
        [(list impl args ...)
         `(,(hash-ref (if spec?
                          (id->e1)
                          (id->e2))
                      impl)
           ,@(for/list ([arg (in-list args)])
               (remap arg spec?)))]

        [(hole ty spec) `(lower ,(remap spec #t) ,(symbol->string ty))]))

    (if node*
        (vector-set! mappings n (insert-node! node* n root?))
        (hash-set! vars n node))
    (when root?
      (set! root-bindings (cons (vector-ref mappings n) root-bindings))))

  ; Var-lowering-rules
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])

    (define curr-var-lowering-rule
      `(rule ((= e (Var ,(symbol->string var))))
             ((let ty ,(symbol->string (representation-name repr))
                )
              (let ety (,(typed-var-id (representation-name repr))
                        ,(symbol->string var))
                )
              (union (lower e ty) ety))
             :ruleset
             lowering))

    (egglog-program-add! curr-var-lowering-rule curr-program))

  ; Var-lifting-rules
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])

    (define curr-var-lifting-rule
      `(rule ((= e (,(typed-var-id (representation-name repr)) ,(symbol->string var))))
             ((let se (Var
                       ,(symbol->string var))
                )
              (union (lift e) se))
             :ruleset
             lifting))

    (egglog-program-add! curr-var-lifting-rule curr-program))

  ; Var-spec-bindings
  (for ([var (in-list (context-vars ctx))])
    (define curr-var-spec-binding
      `(let ,(string->symbol (format "?~a" var)) (Var ,(symbol->string var))))

    (egglog-program-add! curr-var-spec-binding curr-program))

  ; Var-typed-bindings
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])
    (define curr-var-typed-binding
      `(let ,(string->symbol (format "?t~a" var))
         (,(typed-var-id (representation-name repr)) ,(symbol->string var))))

    (egglog-program-add! curr-var-typed-binding curr-program))

  ; Binding Exprs
  (for ([root? (in-vector root-mask)]
        [n (in-naturals)]
        #:when (not (hash-has-key? vars n)))
    (define binding
      (if root?
          (string->symbol (format "?r~a" n))
          (string->symbol (format "?b~a" n))))

    (define curr-binding-exprs `(let ,binding ,(hash-ref bindings binding)))

    (egglog-program-add! curr-binding-exprs curr-program))

  ; Only thing returned -> Extract Bindings
  (define extract-bindings
    (for/list ([root (batch-roots batch)])
      (if (hash-has-key? vars root)
          (if (vector-ref spec-mask root)
              (string->symbol (format "?~a" (hash-ref vars root)))
              (string->symbol (format "?t~a" (hash-ref vars root))))
          (string->symbol (format "?r~a" root)))))

  extract-bindings)

(define (egglog-unsound-detected curr-program tag params schedule-lower schedule-lift)
  (define node-limit (dict-ref params 'node (*node-limit*)))
  (define iter-limit (dict-ref params 'iteration 50))

  ;; Make a copy here too so that we don't modify our original clean copy
  (define temp-program (egglog-program-copy curr-program))

  ;; Algorithm:
  ;; 1. Saturate lifting and lowering
  ;; 2. Repeat rules based on their ruleset tag once
  ;; 3. Run the unsound-rule function ruleset once
  ;; 4. Extract the (unsound) function that returns a bool
  ;; 5. If (unsound) function returns "true", we have unsoundless -> optimal iter limit is one below this
  ;; 6. Run (print-size) to get nodes of the form "node_name : num_nodes" for all nodes in egraph
  ;; 7. If the total number of nodes is more than node-limit -> optimal iter limit is one below this
  ;; 8. Increment until we hit above consition or iter-limit

  ;; TODO : const-fold
  ;; Add lifting and lowering to the schedule that we know will exist

  (when schedule-lower
    (egglog-program-add! `(run-schedule (saturate lowering)) temp-program))

  (when schedule-lift
    (egglog-program-add! `(run-schedule (saturate lifting)) temp-program))

  ;; Loop to check unsoundness
  (let loop ([curr-iter 1])
    (cond
      [(> curr-iter iter-limit) (values iter-limit)]
      [else
       ;; Run the ruleset once more
       (egglog-program-add! `(run-schedule (repeat 1 ,tag)) temp-program)
       (egglog-program-add! `(print-size) temp-program)
       (egglog-program-add! `(run unsound-rule 1) temp-program)
       (egglog-program-add! `(extract (unsound)) temp-program)

       ;; Extract returned value
       (define egglog-output (process-egglog temp-program))

       (define stdout-content (car egglog-output))
       (define lines (string-split (string-trim stdout-content) "\n"))
       (define last-line (list-ref lines (- (length lines) 1)))

       (define total_nodes (calculate-nodes lines))

       ;  (when (equal? last-line "true")
       ;    (printf "ALERT : UNSOUNDNESS DETECTED when...\n"))

       ;; If Unsoundness detected or node-limit reached, then return the
       ;; optimal iter limit (one less than current)
       (if (or (equal? last-line "true") (> total_nodes node-limit))
           (values (sub1 curr-iter))
           (loop (add1 curr-iter)))])))

(define (calculate-nodes lines)
  ;; Don't start from last index, but previous to last index - as last has current unsoundness result
  (define process-lines
    (reverse (if (empty? lines)
                 lines ;; Has no nodes or first iteration
                 (take lines (- (length lines) 1)))))

  ;; Break when we reach the previous unsoundness result -> NOTE: "true" should technically never be reached
  (for/fold ([total_nodes 0]) ([line (in-list process-lines)])
    #:break (or (equal? line "true") (equal? line "false"))

    ;; We need to add the total number of nodes for this one of the format
    ;; "node_name : num_nodes"
    ;; break up into (list node_name num_nodes) with spaces
    (define parts (string-split line ":"))

    ;; Get num_nodes in number
    (define num_nodes (string->number (string-trim (cadr parts))))

    (values (+ total_nodes num_nodes))))

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

(define (e1->expr expr)
  (let loop ([expr expr])
    (match expr
      [`(,(? egglog-num? num) (bigrat (from-string ,n) (from-string ,d)))
       (/ (string->number n) (string->number d))]
      [`(,(? egglog-var? var) ,v) (string->symbol v)]
      [`(If ,cond ,ift ,iff)
       `(if ,(loop cond)
            ,(loop ift)
            ,(loop iff))]
      [`(,op ,args ...) `(,(hash-ref (e1->id) op) ,@(map loop args))])))

(define (e2->expr expr)
  (let loop ([expr expr])
    (match expr
      [`(,(? egglog-num? num) (bigrat (from-string ,n) (from-string ,d)))
       (/ (string->number n) (string->number d))]
      [`(,(? egglog-var? var) ,v) (string->symbol v)]
      [`(IfTy ,cond ,ift ,iff)
       `(if ,(loop cond)
            ,(loop ift)
            ,(loop iff))]
      [`(Approx ,spec ,impl) (approx (e1->expr spec) (loop impl))] ;;; todo approx bug or not?
      [`(,impl ,args ...) `(,(hash-ref (e2->id) impl) ,@(map loop args))])))
