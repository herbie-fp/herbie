#lang racket

(require racket/file
         "rules.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../config.rkt"
         "batch.rkt"
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
(struct egglog-runner (batch brfs reprs schedule ctx)
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
(define (make-egglog-runner batch brfs reprs schedule ctx)
  (define (oops! fmt . args)
    (apply error 'verify-schedule! fmt args))
  ; verify the schedule
  (for ([step (in-list schedule)])
    (unless (memq step '(lift lower unsound rewrite))
      (oops! "unknown schedule step `~a`" step)))

  ; make the runner
  (egglog-runner batch brfs reprs schedule ctx))

;; Runs egglog using an egglog runner by extracting multiple variants
(define (run-egglog runner output-batch [label #f] #:extract extract) ; multi expression extraction
  (define insert-batch (egglog-runner-batch runner))
  (define insert-brfs (egglog-runner-brfs runner))

  ;;;; SUBPROCESS START ;;;;
  (define subproc (create-new-egglog-subprocess label))

  (thread (lambda ()
            (with-handlers ([exn:fail? (lambda (_) (void))])
              (for ([line (in-lines (egglog-subprocess-error subproc))])
                (void)))))

  ;; 1. Add the Prelude (includes all rules) - send directly to egglog
  (prelude subproc #:mixed-egraph? #t)

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
  ;;     (set (const1) a1)
  ;;
  ;;     (let a2 ...)
  ;;     (set (const2) a2)
  ;;
  ;;     (let b1 ...)
  ;;     (set (const3) b1)
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

  (define-values (all-bindings extract-bindings)
    (egglog-add-exprs insert-batch insert-brfs (egglog-runner-ctx runner) subproc))

  (egglog-send subproc
               `(ruleset run-extract-commands)
               `(rule () (,@all-bindings) :ruleset run-extract-commands)
               `(run-schedule (repeat 1 run-extract-commands)))

  ;; 4. Running the schedule : having code inside to emulate egraph-run-rules

  (for ([step (in-list (egglog-runner-schedule runner))])
    (match step
      ['lift (egglog-send subproc '(run-schedule (saturate lift)))]
      ['lower (egglog-send subproc '(run-schedule (saturate lower)))]
      ['unsound (egglog-send subproc '(run-schedule (saturate unsound)))]
      ;; Run the rewrite ruleset interleaved with const-fold until the best iteration
      ['rewrite (egglog-unsound-detected-subprocess step subproc)]))

  ;; Check for unsoundness
  (egglog-send subproc '(run bad-merge-rule 1))
  (when (eq? (egglog-extract subproc '(extract (bad-merge?))) 'true)
    (eprintf "Warning: Unsoundness detected in Egglog e-graph!\n"))

  ;; 5. Extraction -> should just need constructor names from egglog-add-exprs
  (define stdout-content
    (for/list ([constructor-name extract-bindings])
      (egglog-extract subproc `(extract (,constructor-name) ,extract))))

  ;; Close everything subprocess related
  (egglog-subprocess-close subproc)

  ;; (Listof (Listof exprs))
  (define herbie-exprss
    (for/list ([next-expr (in-list stdout-content)])
      (map e2->expr next-expr)))

  (for/list ([variants (in-list herbie-exprss)])
    (for/list ([v (in-list variants)])
      (batch-add! output-batch v))))

;; Egglog requires integer costs, but Herbie uses floating-point costs.
;; Scale by 1000 to convert Herbie's float costs to Egglog's integer costs.
(define (normalize-cost c)
  (exact-round (* c 1000)))

(define (prelude subproc #:mixed-egraph? [mixed-egraph? #t])
  (define pform (*active-platform*))

  (egglog-send subproc `(datatype M ,@(platform-spec-nodes)))

  (apply egglog-send
         subproc
         (append (list `(datatype MTy
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
                       `(rule ((= (Num c1) (Num c2)) (!= c1 c2))
                              ((set (bad-merge?) true))
                              :ruleset
                              bad-merge-rule))
                 (const-fold-rules)
                 (impl-lowering-rules pform)
                 (impl-lifting-rules pform)
                 (num-lowering-rules)
                 (num-lifting-rules)
                 (list (approx-lifting-rule))
                 (egglog-rewrite-rules (*sound-removal-rules*) 'unsound)
                 (list `(ruleset rewrite))
                 (egglog-rewrite-rules (*rules*) 'rewrite)))

  (void))

(define (const-fold-rules)
  `((ruleset const-fold)
    (let ?0 ,(real->bigrat 0)
      )
    (let ?1 ,(real->bigrat 1)
      )
    (rewrite (Add (Num x) (Num y)) (Num (+ x y)) :ruleset const-fold)
    (rewrite (Sub (Num x) (Num y)) (Num (- x y)) :ruleset const-fold)
    (rewrite (Mul (Num x) (Num y)) (Num (* x y)) :ruleset const-fold)
    ; TODO : Non-total operator
    (rule ((= e (Div (Num x) (Num y))) (!= ?0 y)) ((union e (Num (/ x y)))) :ruleset const-fold)
    (rewrite (Neg (Num x)) (Num (neg x)) :ruleset const-fold)
    ;; Power rules -> only case missing is 0^0 making it non-total
    ;; 0^y where y > 0
    (rule ((= e (Pow (Num x) (Num y))) (= ?0 x) (> y ?0)) ((union e (Num ?0))) :ruleset const-fold)
    ;; x^0 where x != 0
    (rule ((= e (Pow (Num x) (Num y))) (= ?0 y) (!= ?0 x)) ((union e (Num ?1))) :ruleset const-fold)
    ;; x^y when y is a whole number and y > 0 and x != 0
    (rule ((= e (Pow (Num x) (Num y))) (> y ?0) (!= ?0 x) (= y (round y)))
          ((union e (Num (pow x y))))
          :ruleset
          const-fold)
    ;; New rule according to Rust : x^y where y is not a whole number
    (rule ((= e (Pow (Num x) (Num y))) (> y ?0) (!= ?0 x) (!= y (round y)))
          ((union e (Num (pow x (round y)))))
          :ruleset
          const-fold)
    ;; Sqrt rules -> Non-total but egglog implementation handles it
    (rule ((= e (Sqrt (Num n))) (sqrt n)) ((union e (Num (sqrt n)))) :ruleset const-fold)
    (rewrite (Log (Num ?1)) (Num ?0) :ruleset const-fold)
    (rewrite (Cbrt (Num ?1)) (Num ?1) :ruleset const-fold)
    (rewrite (Fabs (Num x)) (Num (abs x)) :ruleset const-fold)
    (rewrite (Floor (Num x)) (Num (floor x)) :ruleset const-fold)
    (rewrite (Ceil (Num x)) (Num (ceil x)) :ruleset const-fold)
    (rewrite (Round (Num x)) (Num (round x)) :ruleset const-fold)))

(define (platform-spec-nodes)
  (for ([op '(sound-/ sound-log sound-pow)])
    (hash-set! (id->e1) op (serialize-op op))
    (hash-set! (e1->id) (serialize-op op) op))
  (list* '(Num BigRat :cost 4294967295)
         '(Var String :cost 4294967295)
         '(Sound-/ M M M :cost 4294967295)
         '(Sound-Log M M :cost 4294967295)
         '(Sound-Pow M M M :cost 4294967295)
         (for/list ([op (in-list (all-operators))])
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
  (string->symbol (format "Num~a" repr-name)))

(define (typed-var-id repr-name)
  (string->symbol (format "Var~a" repr-name)))

(define (num-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    (define cost (normalize-cost (platform-repr-cost pform (get-representation repr))))
    `(,(typed-num-id repr) BigRat :cost ,cost)))

(define (var-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))])
    (define cost (normalize-cost (platform-repr-cost pform (get-representation repr))))
    `(,(typed-var-id repr) String :cost ,cost)))

(define (num-lowering-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (Num n)))
           ((let etx (,(typed-num-id repr)
                      n)
              )
            (union (do-lower e ,(symbol->string repr)) etx))
           :ruleset
           lower)))

(define (num-lifting-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (,(typed-num-id repr) n)))
           ((let se (Num
                     n)
              )
            (union (do-lift e) se))
           :ruleset
           lift)))

(define (approx-lifting-rule)
  `(rule ((= e (Approx spec impl))) ((union (do-lift e) spec)) :ruleset lift))

(define (impl-lowering-rules pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define spec-expr (impl-info impl 'spec))
    (define arity (length (impl-info impl 'itype)))
    `(rule ((= e ,(expr->egglog-spec-serialized spec-expr ""))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "t" (symbol->string v)))
                    (do-lower ,v ,(symbol->string (representation-name vt))))))
           ((let t0 ,(symbol->string (representation-name (impl-info impl 'otype)))
              )
            (let et0 (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                      ,@(for/list ([v (in-list (impl-info impl 'vars))])
                          (string->symbol (string-append "t" (symbol->string v)))))
              )
            (union (do-lower e t0) et0))
           :ruleset
           lower)))

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
                `(= ,(string->symbol (string-append "s" (symbol->string v))) (do-lift ,v))))
           ((let se ,(expr->egglog-spec-serialized spec-expr "s")
              )
            (union (do-lift e) se))
           :ruleset
           lift)))

(define (expr->egglog-spec-serialized expr s)
  (let loop ([expr expr])
    (match expr
      [(? number?) `(Num ,(real->bigrat expr))]
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

(define (expr->e1-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(? number?) `(Num ,(real->bigrat expr))]
      [(? symbol?) expr]
      [(list op args ...) `(,(hash-ref (id->e1) op) ,@(map loop args))])))

(define (egglog-rewrite-rules rules tag)
  (for/list ([rule (in-list rules)]
             #:when (not (symbol? (rule-input rule))))
    `(rewrite ,(expr->e1-pattern (rule-input rule))
              ,(expr->e1-pattern (rule-output rule))
              :ruleset
              ,tag)))

(define (egglog-add-exprs batch brfs ctx subproc)
  (define mappings (build-vector (batch-length batch) values))
  (define bindings (make-hash))
  (define vars (make-hash))
  (define (remap x spec?)
    (cond
      [(hash-has-key? vars x)
       (if spec?
           (string->symbol (format "?s~a" (hash-ref vars x)))
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

  ;; Batchref -> Boolean
  (define spec?
    (batch-recurse
     batch
     (lambda (brf recurse)
       (define node (deref brf))
       (match node
         [(? literal?) #f] ;; If literal, not a spec
         [(? number?) #t] ;; If number, it's a spec
         [(? symbol?)
          #f] ;; If symbol, assume not a spec could be either (find way to distinguish) : PREPROCESS
         [(hole _ _) #f] ;; If hole, not a spec
         [(approx _ _) #f] ;; If approx, not a spec
         [`(if ,cond ,ift ,iff)
          (recurse cond)] ;; If the condition or any branch is a spec, then this is a spec
         [(list appl args ...)
          (if (hash-has-key? (id->e1) appl)
              #t ;; appl with op -> Is a spec
              #f)])))) ;; appl impl -> Not a spec

  (for ([brf (in-list brfs)])
    (vector-set! root-mask (batchref-idx brf) #t))
  (for ([node (in-batch batch)]
        [root? (in-vector root-mask)]
        [n (in-naturals)])
    (define node*
      (match node
        [(literal v repr) `(,(typed-num-id repr) ,(real->bigrat v))]
        [(? number?) `(Num ,(real->bigrat node))]
        [(? symbol?) #f]
        [(approx spec impl) `(Approx ,(remap spec #t) ,(remap impl #f))]
        [(list impl args ...)
         `(,(hash-ref (if (spec? (batchref batch n))
                          (id->e1)
                          (id->e2))
                      impl)
           ,@(for/list ([arg (in-list args)])
               (remap arg (spec? (batchref batch n)))))]

        [(hole ty spec) `(do-lower ,(remap spec #t) ,(symbol->string ty))]))

    (if node*
        (vector-set! mappings n (insert-node! node* n root?))
        (hash-set! vars n node))
    (when root?
      (set! root-bindings (cons (vector-ref mappings n) root-bindings))))

  ; Var-lowering-rules
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])
    (egglog-send subproc
                 `(rule ((= e (Var ,(symbol->string var))))
                        ((let ty ,(symbol->string (representation-name repr))
                           )
                         (let ety (,(typed-var-id (representation-name repr))
                                   ,(symbol->string var))
                           )
                         (union (do-lower e ty) ety))
                        :ruleset
                        lower)))

  ; Var-lifting-rules
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])
    (egglog-send subproc
                 `(rule ((= e (,(typed-var-id (representation-name repr)) ,(symbol->string var))))
                        ((let se (Var
                                  ,(symbol->string var))
                           )
                         (union (do-lift e) se))
                        :ruleset
                        lift)))

  (define all-bindings '())
  (define binding->constructor (make-hash)) ; map from binding name to constructor name

  (define constructor-num 1)

  ; ; Var-spec-bindings
  (for ([var (in-list (context-vars ctx))])
    ; Get the binding names for the program
    (define binding-name (string->symbol (format "?s~a" var)))
    (define constructor-name (string->symbol (format "const~a" constructor-num)))
    (hash-set! binding->constructor binding-name constructor-name)

    ; Define the actual binding
    (define curr-var-spec-binding `(let ,binding-name (Var ,(symbol->string var))))

    ; Send the constructor definition
    (egglog-send subproc `(constructor ,constructor-name () M :unextractable))

    ; Add the binding and constructor set to all-bindings for the future rule
    (set! all-bindings (cons curr-var-spec-binding all-bindings))
    (set! all-bindings (cons `(set (,constructor-name) ,binding-name) all-bindings))

    (set! constructor-num (add1 constructor-num)))

  ; Var-typed-bindings
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])
    ; Get the binding names for the program
    (define binding-name (string->symbol (format "?t~a" var)))
    (define constructor-name (string->symbol (format "const~a" constructor-num)))
    (hash-set! binding->constructor binding-name constructor-name)

    ; Define the actual binding
    (define curr-var-typed-binding
      `(let ,binding-name (,(typed-var-id (representation-name repr)) ,(symbol->string var))))

    ; Send the constructor definition
    (egglog-send subproc `(constructor ,constructor-name () MTy :unextractable))

    ; Add the binding and constructor set to all-bindings for the future rule
    (set! all-bindings (cons curr-var-typed-binding all-bindings))
    (set! all-bindings (cons `(set (,constructor-name) ,binding-name) all-bindings))

    (set! constructor-num (add1 constructor-num)))

  ; Binding Exprs
  (for ([root? (in-vector root-mask)]
        [n (in-naturals)]
        #:when (not (hash-has-key? vars n)))

    (define binding-name
      (if root?
          (string->symbol (format "?r~a" n))
          (string->symbol (format "?b~a" n))))

    (define constructor-name (string->symbol (format "const~a" constructor-num)))
    (hash-set! binding->constructor binding-name constructor-name)

    (define actual-binding (hash-ref bindings binding-name))

    (define curr-datatype
      (match actual-binding
        [(cons 'do-lower _) 'MTy]
        [(cons 'do-lift _) 'M]

        ;; TODO : fix this way of getting spec or impl
        [_ (if root? 'MTy 'M)]))

    (define curr-binding-exprs `(let ,binding-name ,actual-binding))

    (egglog-send subproc `(constructor ,constructor-name () ,curr-datatype :unextractable))

    (set! all-bindings (cons curr-binding-exprs all-bindings))
    (set! all-bindings (cons `(set (,constructor-name) ,binding-name) all-bindings))

    (set! constructor-num (add1 constructor-num)))

  (define curr-bindings
    (for/list ([brf brfs])
      (define root (batchref-idx brf))
      (define curr-binding-name
        (if (hash-has-key? vars root)
            (if (spec? brf)
                (string->symbol (format "?s~a" (hash-ref vars root)))
                (string->symbol (format "?t~a" (hash-ref vars root))))
            (string->symbol (format "?r~a" root))))

      (hash-ref binding->constructor curr-binding-name)))

  (values (reverse all-bindings) curr-bindings))

(define (egglog-unsound-detected-subprocess tag subproc)

  (define node-limit (*node-limit*))
  (define iter-limit (*default-egglog-iter-limit*))

  ;; Loop to check unsoundness
  (let loop ([curr-iter 1] [prev-nodes -1])
    (cond
      [(> curr-iter iter-limit) (void)]
      [else
       ;; Run the ruleset once more
       (define math-schedule
         (list `(run-schedule (repeat 1 ,tag))
               '(print-size)
               '(extract "DONE")))

       ;; Get egglog output
       (define-values (math-node-limit? math-total-nodes)
         (get-egglog-size math-schedule subproc node-limit))

       (cond
         [math-node-limit? (void)]
         [(equal? math-total-nodes prev-nodes) (void)]
         [else
          ;; 3. Run const-fold
          (define const-schedule
            (list `(run-schedule (repeat 1 const-fold))
                  '(print-size)
                  '(extract "DONE")))

          (define-values (const-node-limit? const-total-nodes)
            (get-egglog-size const-schedule subproc node-limit))

          (cond
            [const-node-limit? (void)]
            [(equal? const-total-nodes math-total-nodes) (void)]
            [else
             (loop (add1 curr-iter) const-total-nodes)])])])))

(define (get-egglog-size curr-schedule subproc node-limit)
  (define node-values (egglog-send-and-read subproc curr-schedule))

  (define total_nodes (calculate-nodes node-values))

  (values (>= total_nodes node-limit) total_nodes))

(define (calculate-nodes lines)
  (for/fold ([total_nodes 0]) ([line (in-list lines)])
    (define parts (string-split line ":"))
    (if (>= (length parts) 2)
        (+ total_nodes (or (string->number (string-trim (cadr parts))) 0))
        total_nodes)))

(define (egglog-num? id)
  (string-prefix? (symbol->string id) "Num"))

(define (egglog-num-repr id)
  (string->symbol (substring (symbol->string id) 3)))

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
    [`(Approx ,spec ,impl) (approx (e1->expr spec) (e2->expr impl))] ;;; todo approx bug or not?
    [`(,impl ,args ...) `(,(hash-ref (e2->id) impl) ,@(map e2->expr args))]))
