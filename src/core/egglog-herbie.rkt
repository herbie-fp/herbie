#lang racket

(require racket/file
         racket/set
         "rules.rkt"
         "../syntax/platform.rkt"
         "../syntax/platform-state.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../config.rkt"
         "../syntax/block.rkt"
         "../utils/common.rkt"
         "egglog-subprocess.rkt")

(provide (struct-out egglog-runner)
         prelude
         egglog-add-exprs
         make-egglog-runner
         run-egglog
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
  (map representation-name (platform-reprs pform)))

(define (egglog-repr-token repr-name)
  (match repr-name
    [(? representation?) (egglog-repr-token (representation-name repr-name))]
    [(? symbol?) (format "sym_~a" repr-name)]
    [`(array ,elem ,len) (format "arr_~a_~a" len (egglog-repr-token elem))]))

(define (egglog-repr-name token)
  (cond
    [(string-prefix? token "sym_") (string->symbol (substring token 4))]
    [(string-prefix? token "arr_")
     (define rest (substring token 4))
     (define split
       (for/first ([i (in-range (string-length rest))]
                   #:when (char=? (string-ref rest i) #\_))
         i))
     `(array ,(egglog-repr-name (substring rest (add1 split)))
             ,(string->number (substring rest 0 split)))]
    ;; Legacy scalar encoding used in older tests and dumps.
    [else (string->symbol token)]))

(define (real->bigrat val)
  `(bigrat (from-string ,(~s (numerator val))) (from-string ,(~s (denominator val)))))

; Types handled
; - rationals
; - string

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;; Most calls to egglog should be done through this interface.
;;  - `make-egglog-runner`: creates a struct that describes a _reproducible_ egglog instance
;;  - `run-egglog`: takes an egglog runner and performs an extraction (exprs or proof)

;; Herbie's version of an egglog runner.
;; Defines parameters for running rewrite rules with egglog
(struct egglog-runner (block vs schedule ctx)
  #:transparent ; for equality
  #:methods gen:custom-write ; for abbreviated printing
  [(define (write-proc alt port mode)
     (fprintf port "#<egglog-runner>"))])

;; Constructs an egglog runner - structurally serves the same purpose as egg-runner
;;
;; The schedule is a list of step symbols:
;;  - `lift`: run lifting rules for 1 iteration with simple scheduler
;;  - `rewrite`: run rewrite rules up to node limit with backoff scheduler
;;  - `unsound`: run sound-removal rules for 1 iteration with simple scheduler
;;  - `lower`: run lowering rules for 1 iteration with simple scheduler
(define (make-egglog-runner block vs schedule ctx)
  (define (oops! fmt . args)
    (apply error 'verify-schedule! fmt args))
  ; verify the schedule
  (for ([step (in-list schedule)])
    (unless (memq step '(lift lower unsound rewrite))
      (oops! "unknown schedule step `~a`" step)))

  ; make the runner
  (egglog-runner block vs schedule ctx))

;; Runs egglog using an egglog runner by extracting multiple variants
(define (run-egglog runner
                    output-block
                    reprs
                    [label #f]
                    #:extract extract) ; multi expression extraction
  (define insert-block (egglog-runner-block runner))
  (define insert-vs (egglog-runner-vs runner))
  (define schedule (egglog-runner-schedule runner))
  (define pform (*active-platform*))

  ;;;; SUBPROCESS START ;;;;
  ;; Without dump:egglog, reuse a long-lived subprocess whose prelude and rule
  ;; declarations are already loaded (see static-egglog-commands), and isolate
  ;; this call with push/pop. With dump:egglog, spawn a fresh subprocess so
  ;; every dump file is a complete, replayable session.
  (define use-persistent? (not (flag-set? 'dump 'egglog)))
  (define subproc
    (cond
      [use-persistent? (get-persistent-subprocess (static-egglog-commands pform))]
      [else
       (define fresh (create-new-egglog-subprocess label))
       ;; 1. Add the prelude - send directly to egglog.
       (prelude fresh #:mixed-egraph? #t)
       fresh]))

  ;; 2. Inserting expressions into the egglog program and getting a Listof (exprs . extract bindings)

  ;; Overview of the new extraction method:
  ;;
  ;; The idea is to wrap the top-level `let` bindings inside a rule, and then
  ;; execute that rule to perform the computation and store results using constructors.
  ;;
  ;; In the original design, we had a sequence of `let` bindings followed by a schedule run and
  ;; then we perform an extraction of the required bindings.
  ;;
  ;; The new design introduces unextractable constructors to hold each intermediate result.
  ;; These constructors are used in combination with a rule that performs all the bindings
  ;; and assigns them via `set`
  ;;
  ;;   (constructor const1 () Expr :unextractable)
  ;;   (constructor const2 () Expr :unextractable)
  ;;   (constructor const3 () Expr :unextractable)
  ;;   ...
  ;;
  ;;   (ruleset init)
  ;;
  ;;   (rule () (
  ;;     (let a1 ...)
  ;;     (union (const1) a1)
  ;;
  ;;     (let a2 ...)
  ;;     (union (const2) a2)
  ;;
  ;;     (let b1 ...)
  ;;     (union (const3) b1)
  ;;   )
  ;;   :ruleset init)
  ;;
  ;;   (run init 1)
  ;;   (extract (const1))
  ;;   (extract (const2))
  ;;   (extract (const3))
  ;;
  ;;
  ;; The idea behind this updated design is to prevent egglog from constantly rebuilding which
  ;; it does after every top level command. Hence, we wrap the top level bindings into the actions
  ;; of a rule and make them accessible through their unique constructor. Therefore, we must
  ;; keep track of the mapping between each binding and its corresponding constructor.

  ;; If anything fails mid-call, the subprocess protocol state is unknown:
  ;; discard the persistent subprocess so the next call starts fresh.
  (with-handlers ([exn:fail? (lambda (e)
                               (when use-persistent?
                                 (discard-persistent-subprocess!))
                               (raise e))])
    (when use-persistent?
      (egglog-send subproc '(push)))

    (define-values (all-bindings extract-bindings)
      (egglog-add-exprs insert-block insert-vs subproc))

    (egglog-send subproc
                 `(ruleset run-extract-commands)
                 `(rule () (,@all-bindings) :ruleset run-extract-commands)
                 `(run-schedule (repeat 1 run-extract-commands)))

    ;; 4. Running the schedule : having code inside to emulate egraph-run-rules

    (for ([step (in-list schedule)])
      ;; The persistent subprocess already has every step's rules declared.
      (unless use-persistent?
        (apply egglog-send subproc (egglog-step-commands step pform)))
      (match step
        ['lift (egglog-send subproc '(run-schedule (saturate lift)))]
        ['lower (egglog-send subproc '(run-schedule (saturate lower)))]
        ['unsound (egglog-send subproc '(run-schedule (saturate unsound)))]
        ;; Run the rewrite ruleset interleaved with const-fold until the best iteration
        ['rewrite (egglog-unsound-detected-subprocess step subproc)]))

    ;; 5. Extract using constructor names returned by egglog-add-exprs. With
    ;; :dag, subterms shared across variants arrive let-bound once instead of
    ;; expanded at every use (responses shrink by an order of magnitude).
    (define stdout-content
      (egglog-multi-extract subproc
                            `(multi-extract ,extract
                                            :dag
                                            ,@(for/list ([constructor-name
                                                          (in-list extract-bindings)]
                                                         [repr (in-list reprs)])
                                                `(do-lower (,constructor-name)
                                                           ,(egglog-repr-token repr))))))

    ;; Roll the persistent subprocess back to its post-prelude state, or close
    ;; everything subprocess related
    (cond
      [use-persistent?
       (egglog-send subproc '(pop))
       (set! persistent-call-in-progress? #f)]
      [else (egglog-subprocess-close subproc)])

    (match-define `(let ,dag-bindings ,dag-body) stdout-content)
    (egglog-dag->blockrefs dag-bindings dag-body output-block)))

;; Convert the (let ((name def) ...) ((variant ...) ...)) response of
;; (multi-extract :dag ...) into blockrefs. Shared subterms arrive as ?tN
;; binding references; each is resolved, converted, and interned exactly
;; once, so the total work is proportional to the response DAG rather than
;; to the expanded variant trees.
(define (egglog-dag->blockrefs bindings body output-block)
  ;; Binding name -> its definition with references resolved. Reusing the
  ;; same pair object at every reference preserves sharing, which the
  ;; eq?-keyed memo tables below rely on.
  (define env (make-hasheq))
  (define (resolve expr)
    (match expr
      [(? symbol?) (hash-ref env expr)]
      [(list head args ...) (cons head (map resolve args))]
      [_ expr]))
  (for ([binding (in-list bindings)])
    (match-define (list name def) binding)
    (hash-set! env name (resolve def)))

  ;; eq?-memoized counterparts of e1->expr and e2->expr: a shared node is
  ;; converted once. Impl (MTy) terms are interned eagerly and referenced as
  ;; blockrefs, which block-add! accepts as children; spec (M) terms stay
  ;; plain expressions since approx nodes store their spec unmunged.
  (define spec-memo (make-hasheq))
  (define impl-memo (make-hasheq))
  (define (spec->expr expr)
    (hash-ref! spec-memo
               expr
               (lambda ()
                 (match expr
                   [`(Num (bigrat (from-string ,n) (from-string ,d)))
                    (/ (string->number n) (string->number d))]
                   [`(Var ,v) (string->symbol v)]
                   [`(,op ,args ...) `(,(hash-ref (e1->id) op) ,@(map spec->expr args))]))))
  (define (add-impl! expr)
    (hash-ref!
     impl-memo
     expr
     (lambda ()
       (match expr
         [`(,(? egglog-num? num) (bigrat (from-string ,n) (from-string ,d)))
          (block-add! output-block
                      (literal (/ (string->number n) (string->number d)) (egglog-num-repr num)))]
         [`(,(? egglog-var? var) ,v) (block-add! output-block (string->symbol v))]
         ; Approx stores a spec expression in E1/M and an implementation in E2/MTy.
         [`(Approx ,spec ,impl)
          (block-add! output-block (approx (spec->expr spec) (add-impl! impl)))]
         [`(,impl ,args ...)
          (block-add! output-block `(,(hash-ref (e2->id) impl) ,@(map add-impl! args)))]))))

  (for/list ([variants (in-list body)])
    (for/list ([v (in-list variants)])
      (add-impl! (resolve v)))))

;; Egglog requires integer costs, but Herbie uses floating-point costs.
;; Scale by 1000 to convert Herbie's float costs to Egglog's integer costs.
(define (normalize-cost c)
  (exact-round (* c 1000)))

(define (prelude-commands pform)
  (list `(datatype M ,@(platform-spec-nodes))
        `(datatype MTy
                   ,@(num-typed-nodes pform)
                   ,@(var-typed-nodes pform)
                   (Approx M MTy)
                   ,@(platform-impl-nodes pform))
        `(constructor do-lower (M String) MTy :unextractable)
        `(constructor do-lift (MTy) M :unextractable)
        `(ruleset lower)
        `(ruleset lift)
        `(ruleset unsound)
        `(function bad-merge? () bool :merge (or old new))
        `(ruleset bad-merge-rule)
        `(set (bad-merge?) false)
        `(rule ((= (Num c1) (Num c2)) (!= c1 c2)) ((set (bad-merge?) true)) :ruleset bad-merge-rule)))

(define (prelude subproc #:mixed-egraph? [mixed-egraph? #t])
  (apply egglog-send subproc (prelude-commands (*active-platform*)))
  (void))

;; The prelude and every step's rule declarations are identical for all calls
;; within a run, so they can be declared once in a long-lived subprocess and
;; each call isolated with push/pop. Rules of steps a call never runs are
;; inert. The command list doubles as the cache key, so any change in
;; platform or rules respawns the subprocess.
(define (static-egglog-commands pform)
  (append (prelude-commands pform)
          (egglog-step-commands 'lift pform)
          (egglog-step-commands 'lower pform)
          (egglog-step-commands 'unsound pform)
          (egglog-step-commands 'rewrite pform)))

(define persistent-subprocess #f)
(define persistent-subprocess-key #f)
;; Owns the subprocess and its ports, so per-test custodians (e.g. timeouts)
;; cannot reclaim them between calls.
(define persistent-custodian (make-custodian))
;; Set while a call is using the subprocess. If it is still set when the next
;; call starts, the previous call was interrupted mid-protocol (e.g. its
;; thread was killed by a timeout), so the subprocess state is unknown and it
;; must be discarded.
(define persistent-call-in-progress? #f)

(define (discard-persistent-subprocess!)
  (when persistent-subprocess
    (with-handlers ([exn:fail? void])
      (egglog-subprocess-close persistent-subprocess))
    (set! persistent-subprocess #f)
    (set! persistent-subprocess-key #f)))

(define (persistent-subprocess-usable? static-commands)
  (and persistent-subprocess
       (not persistent-call-in-progress?)
       (not (port-closed? (egglog-subprocess-input persistent-subprocess)))
       (eq? (subprocess-status (egglog-subprocess-process persistent-subprocess)) 'running)
       (equal? persistent-subprocess-key static-commands)))

(define (get-persistent-subprocess static-commands)
  (unless (persistent-subprocess-usable? static-commands)
    (discard-persistent-subprocess!)
    (define subproc
      (parameterize ([current-custodian persistent-custodian])
        (create-new-egglog-subprocess #f)))
    (apply egglog-send subproc static-commands)
    (set! persistent-subprocess subproc)
    (set! persistent-subprocess-key static-commands))
  (set! persistent-call-in-progress? #t)
  persistent-subprocess)

(define (egglog-step-commands step pform)
  (match step
    ['lift (append (list (approx-lifting-rule)) (impl-lifting-rules pform) (num-lifting-rules))]
    ['lower (append (impl-lowering-rules pform) (num-lowering-rules))]
    ['unsound (egglog-rewrite-rules (*sound-removal-rules*) 'unsound)]
    ['rewrite
     (append (list `(ruleset rewrite))
             (const-fold-rules)
             (egglog-rewrite-rules (*rules*) 'rewrite))]))

(define (const-fold-rules)
  `((ruleset const-fold)
    (let $0 ,(real->bigrat 0)
      )
    (let $1 ,(real->bigrat 1)
      )
    (rewrite (Add (Num x) (Num y)) (Num (+ x y)) :ruleset const-fold)
    (rewrite (Sub (Num x) (Num y)) (Num (- x y)) :ruleset const-fold)
    (rewrite (Mul (Num x) (Num y)) (Num (* x y)) :ruleset const-fold)
    ; TODO : Non-total operator
    (rule ((= e (Div (Num x) (Num y))) (!= $0 y)) ((union e (Num (/ x y)))) :ruleset const-fold)
    (rewrite (Neg (Num x)) (Num (neg x)) :ruleset const-fold)
    ;; Power rules -> only case missing is 0^0 making it non-total
    ;; 0^y where y > 0
    (rule ((= e (Pow (Num x) (Num y))) (= $0 x) (> y $0)) ((union e (Num $0))) :ruleset const-fold)
    ;; x^0 where x != 0
    (rule ((= e (Pow (Num x) (Num y))) (= $0 y) (!= $0 x)) ((union e (Num $1))) :ruleset const-fold)
    ;; x^y when y is a whole number and y > 0 and x != 0
    (rule ((= e (Pow (Num x) (Num y))) (> y $0) (!= $0 x) (= y (round y)) (<= y ,(real->bigrat 16)))
          ((union e (Num (pow x y))))
          :ruleset
          const-fold)
    ;; New rule according to Rust : x^y where y is not a whole number
    ; (rule ((= e (Pow (Num x) (Num y))) (> y $0) (!= $0 x) (!= y (round y)))
    ;       ((union e (Num (pow x (round y)))))
    ;       :ruleset
    ;       const-fold)
    ;; Sqrt rules -> Non-total but egglog implementation handles it
    (rule ((= e (Sqrt (Num n))) (sqrt n)) ((union e (Num (sqrt n)))) :ruleset const-fold)
    (rewrite (Log (Num $1)) (Num $0) :ruleset const-fold)
    (rewrite (Cbrt (Num $1)) (Num $1) :ruleset const-fold)
    (rewrite (Fabs (Num x)) (Num (abs x)) :ruleset const-fold)
    (rewrite (Floor (Num x)) (Num (floor x)) :ruleset const-fold)
    (rewrite (Ceil (Num x)) (Num (ceil x)) :ruleset const-fold)
    (rewrite (Round (Num x)) (Num (round x)) :ruleset const-fold)))

(define (platform-spec-nodes)
  (for ([op '(sound-/ sound-log sound-pow)])
    (hash-set! (id->e1) op (serialize-op op))
    (hash-set! (e1->id) (serialize-op op) op))
  (hash-set! (id->e1) 'array 'Array)
  (hash-set! (e1->id) 'Array 'array)
  (hash-set! (e1->id) 'Array3 'array)
  (list* '(Num BigRat :cost 4294967295)
         '(Var String :cost 4294967295)
         '(Sound-/ M M M :cost 4294967295)
         '(Sound-Log M M :cost 4294967295)
         '(Sound-Pow M M M :cost 4294967295)
         '(Array M M :cost 4294967295)
         '(Array3 M M M :cost 4294967295)
         (for/list ([op (in-list (all-operators))]
                    #:unless (eq? op 'array))
           (define arity (length (operator-info op 'itype)))
           (hash-set! (id->e1) op (serialize-op op))
           (hash-set! (e1->id) (serialize-op op) op)
           `(,(serialize-op op) ,@(make-list arity 'M) :cost 4294967295))))

(define (platform-impl-nodes pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    (define typed-name (string->symbol (format "~aTy" (serialize-impl impl))))
    (hash-set! (id->e2) impl typed-name)
    (hash-set! (e2->id) typed-name impl)
    (define cost (normalize-cost (impl-info impl 'cost)))
    `(,typed-name ,@(make-list arity 'MTy) :cost ,cost)))

(define (typed-num-id repr-name)
  (string->symbol (format "Num_~a" (egglog-repr-token repr-name))))

(define (typed-var-id repr-name)
  (string->symbol (format "Var_~a" (egglog-repr-token repr-name))))

(define (num-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    (define cost (normalize-cost (platform-repr-cost pform (get-representation repr))))
    `(,(typed-num-id repr) BigRat :cost ,cost)))

(define (var-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))])
    `(,(typed-var-id repr) String :cost 0)))

(define (num-lowering-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (Num n)))
           ((union (do-lower e ,(egglog-repr-token repr)) (,(typed-num-id repr) n)))
           :ruleset
           lower)))

(define (num-lifting-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (,(typed-num-id repr) n))) ((union (do-lift e) (Num n))) :ruleset lift)))

(define (approx-lifting-rule)
  `(rule ((= e (Approx spec impl))) ((union (do-lift e) spec)) :ruleset lift))

(define (impl-lowering-rules pform)
  (define helper-impls
    (for/seteq ([extension (in-list (*platform-extensions*))])
      (fpcore-extension-name extension)))
  (for/list ([impl (in-list (platform-impls pform))]
             #:unless (set-member? helper-impls impl))
    (define spec-expr (impl-info impl 'spec))
    `(rule ((= ?root ,(expr->egglog-spec-serialized spec-expr ""))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "t" (symbol->string v)))
                    (do-lower ,v ,(egglog-repr-token vt)))))
           ((union (do-lower ?root ,(egglog-repr-token (impl-info impl 'otype)))
                   (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                    ,@(for/list ([v (in-list (impl-info impl 'vars))])
                        (string->symbol (string-append "t" (symbol->string v)))))))
           :ruleset
           lower)))

(define (impl-lifting-rules pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define spec-expr (impl-info impl 'spec))
    `(rule ((= ?root
               (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                ,@(impl-info impl 'vars)))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "s" (symbol->string v))) (do-lift ,v))))
           ((union (do-lift ?root) ,(expr->egglog-spec-serialized spec-expr "s")))
           :ruleset
           lift)))

(define (serialize-spec-op op arity)
  (match* (op arity)
    [('array 2) 'Array]
    [('array 3) 'Array3]
    [(_ _) (hash-ref (id->e1) op)]))

(define (expr->egglog-spec-serialized expr s)
  (let loop ([expr expr])
    (match expr
      [(? number?) `(Num ,(real->bigrat expr))]
      [(? symbol?) (string->symbol (string-append s (symbol->string expr)))]
      [(list op args ...)
       `(,(if (hash-has-key? (id->e1) op)
              (serialize-spec-op op (length args))
              (hash-ref (id->e2) op))
         ,@(map loop args))])))

(define (serialize-op op)
  (if (hash-has-key? op-string-names op)
      (hash-ref op-string-names op)
      (string->symbol (string-titlecase (symbol->string op)))))

(define (serialize-impl impl)
  (define impl-split (string-split (symbol->string impl) "."))
  (define op (string->symbol (car impl-split)))
  (define type
    (if (null? (cdr impl-split))
        ""
        (string-join (cdr impl-split) "")))
  (string->symbol (string-append (symbol->string (serialize-op op)) type)))

(define (expr->e1-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(? number?) `(Num ,(real->bigrat expr))]
      [(? symbol?) expr]
      [(list op args ...) `(,(serialize-spec-op op (length args)) ,@(map loop args))])))

(define (egglog-rewrite-rules rules tag)
  (for/list ([rule (in-list rules)]
             #:when (not (symbol? (rule-input rule))))
    `(rewrite ,(expr->e1-pattern (rule-input rule))
              ,(expr->e1-pattern (rule-output rule))
              :ruleset
              ,tag)))

(define (egglog-add-exprs block vs subproc)
  (define bindings (make-hash))
  (define (var-binding var)
    (string->symbol (format "?s~a" var)))

  ; node -> egglog node binding
  ; inserts an expression into the e-graph, returning binding variable.
  (define (insert-node! node n root?)
    (define binding
      (if root?
          (string->symbol (format "?r~a" n))
          (string->symbol (format "?b~a" n))))
    (hash-set! bindings binding node)
    binding)

  (define root-mask (make-vector (block-length block) #f))
  (define reachable-vs '())

  (for ([v (in-list vs)])
    (vector-set! root-mask (val-idx v) #t))
  (define add-to-egglog
    (block-recurse block
                   (lambda (v recurse)
                     (define n (val-idx v))
                     (define node (val-def v))
                     (define root? (vector-ref root-mask n))
                     (define node*
                       (match node
                         [(? number?) `(Num ,(real->bigrat node))]
                         [(? symbol?) #f]
                         [(list impl args ...)
                          `(,(hash-ref (id->e1) impl) ,@(for/list ([arg (in-list args)])
                                                          (recurse arg)))]))

                     (set! reachable-vs (cons v reachable-vs))
                     (if node*
                         (insert-node! node* n root?)
                         (var-binding node)))))

  (define root-bindings
    (for/list ([v (in-list vs)])
      (add-to-egglog v)))

  ; Var-lowering-rules
  (for ([var (in-list (block-vars block))]
        [repr (in-list (block-var-reprs block))])
    (egglog-send subproc
                 `(rule ((= e (Var ,(symbol->string var))))
                        ((union (do-lower e ,(egglog-repr-token repr))
                                (,(typed-var-id (representation-name repr)) ,(symbol->string var))))
                        :ruleset
                        lower)))

  ; Var-lifting-rules
  (for ([var (in-list (block-vars block))]
        [repr (in-list (block-var-reprs block))])
    (egglog-send subproc
                 `(rule ((= e (,(typed-var-id (representation-name repr)) ,(symbol->string var))))
                        ((union (do-lift e) (Var ,(symbol->string var))))
                        :ruleset
                        lift)))

  (define all-bindings '())
  (define binding->constructor (make-hash)) ; map from binding name to constructor name

  (define constructor-num 1)

  ; ; Var-spec-bindings
  (for ([var (in-list (block-vars block))])
    ; Get the binding names for the program
    (define binding-name (string->symbol (format "?s~a" var)))
    (define constructor-name (string->symbol (format "const~a" constructor-num)))
    (hash-set! binding->constructor binding-name constructor-name)

    ; Define the actual binding
    (define curr-var-spec-binding `(let ,binding-name (Var ,(symbol->string var))))

    ; Send the constructor definition
    (egglog-send subproc `(constructor ,constructor-name () M :unextractable))

    ; Add the binding and constructor union to all-bindings for the future rule
    (set! all-bindings (cons curr-var-spec-binding all-bindings))
    (set! all-bindings (cons `(union (,constructor-name) ,binding-name) all-bindings))

    (set! constructor-num (add1 constructor-num)))

  ; Binding Exprs
  (for ([v (in-list (reverse reachable-vs))]
        #:unless (symbol? (val-def v)))
    (define n (val-idx v))

    (define binding-name
      (if (vector-ref root-mask n)
          (string->symbol (format "?r~a" n))
          (string->symbol (format "?b~a" n))))

    (define constructor-name (string->symbol (format "const~a" constructor-num)))
    (hash-set! binding->constructor binding-name constructor-name)

    (define actual-binding (hash-ref bindings binding-name))
    (define curr-binding-exprs `(let ,binding-name ,actual-binding))

    (egglog-send subproc `(constructor ,constructor-name () M :unextractable))

    (set! all-bindings (cons curr-binding-exprs all-bindings))
    (set! all-bindings (cons `(union (,constructor-name) ,binding-name) all-bindings))

    (set! constructor-num (add1 constructor-num)))

  (define curr-bindings
    (for/list ([binding-name (in-list root-bindings)])
      (hash-ref binding->constructor binding-name)))

  (values (reverse all-bindings) curr-bindings))

(define (egglog-unsound-detected-subprocess tag subproc)
  (define node-limit (*node-limit*))
  (define iter-limit (*default-egglog-iter-limit*))

  ;; The back-off scheduler's :node-limit keeps the e-graph within the node
  ;; limit as it chooses matches; the :until guards with get-node-size! stop
  ;; the schedule once the limit is reached. Both measure e-nodes (rows of
  ;; eq-sort tables), matching what egg counts, rather than get-size!'s total
  ;; table size. After each iteration, we check for unsound merges via
  ;; bad-merge-rule. The schedule runs until:
  ;;   1. Node limit is reached (get-node-size! >= node-limit)
  ;;   2. Saturation (no more progress)
  ;;   3. Iter limit is reached
  ;;   4. Unsoundness is detected (bad-merge? becomes true)

  (egglog-send subproc
               `(run-schedule
                 (let-scheduler bo (back-off :node-limit ,node-limit :eager-apply 1))
                 (repeat ,iter-limit
                         (seq (run-with bo ,tag :until (<= ,node-limit (get-node-size!)))
                              (run-with bo const-fold :until (<= ,node-limit (get-node-size!)))
                              (run bad-merge-rule :until (bad-merge?))))))
  (void))

(define (egglog-num? id)
  (string-prefix? (symbol->string id) "Num"))

(define (egglog-num-repr id)
  (define id-str (symbol->string id))
  (if (string-prefix? id-str "Num_")
      (egglog-repr-name (substring id-str 4))
      (string->symbol (substring id-str 3))))

(define (egglog-var? id)
  (string-prefix? (symbol->string id) "Var"))

(define (e1->expr expr)
  (match expr
    [`(Num (bigrat (from-string ,n) (from-string ,d))) (/ (string->number n) (string->number d))]
    [`(Var ,v) (string->symbol v)]
    [`(,op ,args ...) `(,(hash-ref (e1->id) op) ,@(map e1->expr args))]))

(define (e2->expr expr)
  (match expr
    [`(,(? egglog-num? num) (bigrat (from-string ,n) (from-string ,d)))
     (literal (/ (string->number n) (string->number d)) (egglog-num-repr num))]
    [`(,(? egglog-var? var) ,v) (string->symbol v)]
    ; Approx stores a spec expression in E1/M and an implementation in E2/MTy.
    [`(Approx ,spec ,impl) (approx (e1->expr spec) (e2->expr impl))]
    [`(,impl ,args ...) `(,(hash-ref (e2->id) impl) ,@(map e2->expr args))]))
