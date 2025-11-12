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
  (remove-duplicates (map (lambda (repr) (representation-name repr)) (platform-reprs pform))))

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

  ;; 5. Extraction -> should just need constructor names from egglog-add-exprs
  (define extract-commands
    (for/list ([constructor-name extract-bindings])
      `(extract (,constructor-name) ,extract)))

  (define stdout-content (egglog-extract subproc extract-commands))

  ;; (Listof (Listof exprs))
  (define herbie-exprss
    (for/list ([next-expr (in-list stdout-content)])
      (map e2->expr next-expr)))

  (define result
    (for/list ([variants (in-list herbie-exprss)])
      (remove-duplicates
       (for/list ([v (in-list variants)])
         (egglog->batchref v output-batch (context-repr (egglog-runner-ctx runner))))
       #:key batchref-idx)))

  ;; Close everything subprocess related
  (egglog-subprocess-close subproc)

  ;; (Listof (Listof batchref))
  result)

(define (egglog->batchref expr batch type)
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
          [(approx spec impl) (approx (loop spec #f) (loop impl type))]
          [(list (? impl-exists? impl) args ...) (cons impl (map loop args (impl-info impl 'itype)))]
          [(list op args ...)
           (define args*
             (for/list ([arg (in-list args)])
               (loop arg #f)))
           (cons op args*)]))
      (batchref-idx (batch-push! batch term))))
  (batchref batch idx))

;; Egglog requires integer costs, but Herbie uses floating-point costs.
;; Scale by 1000 to convert Herbie's float costs to Egglog's integer costs.
(define (normalize-cost c)
  (exact-round (* c 1000)))

(define (prelude subproc #:mixed-egraph? [mixed-egraph? #t])
  (define pform (*active-platform*))

  (egglog-send
   subproc
   `(datatype M (Num BigRat :cost 4294967295) (Var String :cost 4294967295) ,@(platform-spec-nodes)))

  (apply egglog-send
         subproc
         (append (list `(datatype MTy
                                  ,@(num-typed-nodes pform)
                                  ,@(var-typed-nodes pform)
                                  (Approx M MTy)
                                  ,@(platform-impl-nodes pform))
                       `(constructor do-lower (M String) MTy :unextractable)
                       `(constructor do-lift (MTy) M :unextractable)
                       `(ruleset const-fold)
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
                 const-fold
                 (impl-lowering-rules pform)
                 (impl-lifting-rules pform)
                 (num-lowering-rules)
                 (num-lifting-rules)
                 (list (approx-lifting-rule))
                 (egglog-rewrite-rules (*sound-removal-rules*) 'unsound)
                 (list `(ruleset rewrite))
                 (egglog-rewrite-rules (*rules*) 'rewrite)))

  (void))

(define const-fold
  `((let ?zero (bigrat
                [from-string "0"]
                [from-string "1"])
      )
    (rewrite (Add (Num x) (Num y)) (Num (+ x y)) :ruleset const-fold)
    (rewrite (Sub (Num x) (Num y)) (Num (- x y)) :ruleset const-fold)
    (rewrite (Mul (Num x) (Num y)) (Num (* x y)) :ruleset const-fold)
    ; TODO : Non-total operator
    (rule ((= e (Div (Num x) (Num y))) (!= ?zero y)) ((union e (Num (/ x y)))) :ruleset const-fold)
    (rewrite (Neg (Num x)) (Num (neg x)) :ruleset const-fold)
    ;; Power rules -> only case missing is 0^0 making it non-total
    ;; 0^y where y > 0
    (rule ((= e (Pow (Num x) (Num y))) (= ?zero x) (> y ?zero))
          ((union e (Num ?zero)))
          :ruleset
          const-fold)
    ;; x^0 where x != 0
    (rule ((= e (Pow (Num x) (Num y))) (= ?zero y) (!= ?zero x))
          ((union e (Num (bigrat (from-string "1") (from-string "1")))))
          :ruleset
          const-fold)
    ;; x^y when y is a whole number and y > 0 and x != 0
    (rule ((= e (Pow (Num x) (Num y))) (> y ?zero) (!= ?zero x) (= y (round y)))
          ((union e (Num (pow x y))))
          :ruleset
          const-fold)
    ;; New rule according to Rust : x^y where y is not a whole number
    (rule ((= e (Pow (Num x) (Num y))) (> y ?zero) (!= ?zero x) (!= y (round y)))
          ((union e (Num (pow x (round y)))))
          :ruleset
          const-fold)
    ;; Sqrt rules -> Non-total but egglog implementation handles it
    (rule ((= e (Sqrt (Num n))) (sqrt n)) ((union e (Num (sqrt n)))) :ruleset const-fold)
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
  (append* (for/list ([op (in-list (all-operators))])
             (hash-set! (id->e1) op (serialize-op op))
             (hash-set! (e1->id) (serialize-op op) op)

             ; Unsound versions of operations
             (define unsound-op (sym-append "unsound-" op))
             (hash-set! (id->e1) unsound-op (serialize-op unsound-op))
             (hash-set! (e1->id) (serialize-op unsound-op) unsound-op)

             (define sound-op (sym-append "sound-" op))
             (hash-set! (id->e1) sound-op (serialize-op sound-op))
             (hash-set! (e1->id) (serialize-op sound-op) sound-op)

             (define arity (length (operator-info op 'itype)))
             (list `(,(serialize-op op) ,@(make-list arity 'M) :cost 4294967295)
                   `(,(serialize-op sound-op) ,@(make-list arity 'M) M :cost 4294967295)
                   `(,(serialize-op unsound-op) ,@(make-list arity 'M) :cost 4294967295)))))

(define (platform-impl-nodes pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    (define typed-name (string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty")))
    (hash-set! (id->e2) impl typed-name)
    (hash-set! (e2->id) typed-name impl)
    `(,typed-name ,@(for/list ([i (in-range arity)])
                      'MTy)
                  :cost
                  ,(normalize-cost (impl-info impl 'cost)))))

(define (typed-num-id repr-name)
  (string->symbol (string-append "Num" (symbol->string repr-name))))

(define (typed-var-id repr-name)
  (string->symbol (string-append "Var" (symbol->string repr-name))))

(define (num-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(,(typed-num-id repr) BigRat
                           :cost
                           ,(normalize-cost (platform-repr-cost pform (get-representation repr))))))

(define (var-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))])
    `(,(typed-var-id repr) String
                           :cost
                           ,(normalize-cost (platform-repr-cost pform (get-representation repr))))))

(define (num-lowering-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (Num n)))
           ((let tx ,(symbol->string repr)
              )
            (let etx (,(typed-num-id repr)
                      n)
              )
            (union (do-lower e tx) etx))
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
      [(list op args ...) `(,(hash-ref (id->e1) op) ,@(map loop args))])))

(define (expr->e1-expr expr)
  (let loop ([expr expr])
    (match expr
      [(? number?)
       `(Num (bigrat (from-string ,(number->string (numerator expr)))
                     (from-string ,(number->string (denominator expr)))))]
      [(? symbol?) `(Var ,(symbol->string expr))]
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

  (for ([brf brfs])
    (vector-set! root-mask (batchref-idx brf) #t))
  (for ([node (in-batch batch)]
        [root? (in-vector root-mask)]
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
  (apply egglog-send
         subproc
         (for/list ([var (in-list (context-vars ctx))]
                    [repr (in-list (context-var-reprs ctx))])
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
  (apply egglog-send
         subproc
         (for/list ([var (in-list (context-vars ctx))]
                    [repr (in-list (context-var-reprs ctx))])
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

  ;; Algorithm:
  ;; 1. Run (PUSH) to the save the above state of the egraph
  ;; 2. Repeat rules based on their ruleset tag once
  ;; 3. Run the bad-merge-rule ruleset once
  ;; 4. Extract the (bad-merge?) function that returns a bool
  ;; 5. If (bad-merge?) function returns "true", we have a bad merge, so go to Step 10 for ROLLBACK
  ;; 6. Run (print-size) to get nodes of the form "node_name : num_nodes" for all nodes in egraph
  ;; 7. If the total number of nodes is more than node-limit, do NOT ROLLBACK and go to Step 11
  ;; 8. Repeat rules based on the const-fold tag once and repeat Steps 3-7
  ;; 9. Increment curr-iter by 1, and if it has not reach iter-limit, restart from Step 1
  ;; 10. If we reach the ROLLBACK stage, the optimal number of iterations for this ruleset is one
  ;;    below current. Therefore, we run (POP) to ROLLBACK to the last valid state.
  ;;    Then run the whole thing one last time from the ideal number of iterations
  ;; 11. Exit the unsound detection process

  ; Saturation detection by verifying the previous number of nodes and nw ones
  (define prev-number-nodes -1)

  ;; Loop to check unsoundness
  (let loop ([curr-iter 1])
    (cond
      ; Note we do NOT (pop) here
      ; Return that we do not need to run again because we did not pop
      [(> curr-iter iter-limit) (values iter-limit #f)]
      [else

       ;; Run the ruleset once more
       (define math-schedule
         (list '(push)
               `(run-schedule (repeat 1 ,tag))
               '(print-size)
               '(run bad-merge-rule 1)
               '(extract (bad-merge?))))

       ;; Get egglog output
       (define-values (math-unsound? math-node-limit? math-total-nodes)
         (get-egglog-output math-schedule subproc node-limit))

       (cond
         ;;  There are two condiitons where we exit unsoundness dteection WITHOUT running (pop)
         ;;  1. Saturation: when the number of nodes stays the same between iterations.
         ;;  2. Node limit: when the e-graph exceeds the allowed number of nodes.
         ;;  If either condition is met, return the current iteration limit and avoid running another
         ;;  iteration.

         ;; TODO : This saturation condition below is problematic. Simply checking unchanged node
         ;;        count is misleading as we could, theoretically, have a ruleset that merges e-classes
         ;;        without increasing number of nodes, meaning further iterations "could" make
         ;;        progress. This logic incorrectly considers it saturated. Consider modifying the
         ;;        logic or submit a feature request to Egglog for more accurate saturation detection.
         [(equal? math-total-nodes prev-number-nodes) (values curr-iter #f)]

         ;; TODO : This logic below is also algorithmally incorrect.  If we hit the node limit, we
         ;;        should "stop" at that iteration, not rollback by 1. The correct line should be
         ;;        [math-node-limit? (values curr-iter #f)]
         ;;
         ;;        However, we currently use this rollback logic for performance reasons due to
         ;;        extraction. While we have tested that the extraction time linearly increases with
         ;;        larger e-graphs (that indicates we have not done something majorly wrong).
         ;;        However, even 0.1s can be considered too high for a reltively small (~2000 nodes)
         ;;        e-graph while extracting. For now, popping provides a smaller e-graph and gives
         ;;        performance comparable to Egg-Herbie, thought it doesn't affect correctness too much
         [math-node-limit?
          (egglog-send subproc '(pop))
          (values (sub1 curr-iter) #t)]

         ;; If Unsoundness detected or node-limit reached, then return the
         ;; optimal iter limit (one less than current) and run (pop)
         [math-unsound?
          ;; Pop once at the end since the egraph isn't valid
          (egglog-send subproc '(pop))

          ;; Return one less than current iteration and indicate that we need to run again because pop
          (values (sub1 curr-iter) #t)]

         ;; Continue to next iteration of the math rules
         [else
          ;; set for the constant folding iteration
          (set! prev-number-nodes math-total-nodes)

          ;; 3. Run const-fold
          (define const-schedule
            (list '(push)
                  `(run-schedule (repeat 1 const-fold))
                  '(print-size)
                  '(run bad-merge-rule 1)
                  '(extract (bad-merge?))))

          (define-values (const-unsound? const-node-limit? const-total-nodes)
            (get-egglog-output const-schedule subproc node-limit))

          (cond
            ;; TODO:  See the TODO from above
            [(equal? const-total-nodes prev-number-nodes) (values curr-iter #f)]
            [const-node-limit?
             (egglog-send subproc '(pop))
             (values (sub1 curr-iter) #t)]

            [const-unsound?
             (egglog-send subproc '(pop))
             (values (sub1 curr-iter) #t)]

            [else
             ;; Update state for the next iteration
             (set! prev-number-nodes const-total-nodes)

             (loop (add1 curr-iter))])])])))

(define (get-egglog-output curr-schedule subproc node-limit)
  (define-values (node-values unsound?) (egglog-send-unsound-detection subproc curr-schedule))

  ;  (when unsound?
  ;    (printf "ALERT : UNSOUNDNESS DETECTED when...\n"))

  (define total_nodes (calculate-nodes node-values))

  ;; There are 3 cases when we can exit the unsound detection
  ;;  1. Unsoundness is detected in the egraph
  ;;  2. We have reached or exceeded the set node limit
  ;;  3. Saturation check which is done in parent function

  (values unsound? (>= total_nodes node-limit) total_nodes))

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
    ; (define num_nodes (string->number (string-trim (cadr parts))))
    (define num_nodes
      (if (> (length parts) 0)
          (string->number (string-trim (cadr parts)))
          0))

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
    [(? symbol?) #t]
    [`(,impl ,args ...) (and (not (eq? impl 'typed-id)) (andmap egglog-expr-typed? args))]))

(define (e1->expr expr)
  (let loop ([expr expr])
    (match expr
      [`(,(? egglog-num? num) (bigrat (from-string ,n) (from-string ,d)))
       (/ (string->number n) (string->number d))]
      [`(,(? egglog-var? var) ,v) (string->symbol v)]
      [`(,op ,args ...) `(,(hash-ref (e1->id) op) ,@(map loop args))])))

(define (e2->expr expr)
  (let loop ([expr expr])
    (match expr
      [`(,(? egglog-num? num) (bigrat (from-string ,n) (from-string ,d)))
       (/ (string->number n) (string->number d))]
      [`(,(? egglog-var? var) ,v) (string->symbol v)]
      [`(Approx ,spec ,impl) (approx (e1->expr spec) (loop impl))] ;;; todo approx bug or not?
      [`(,impl ,args ...) `(,(hash-ref (e2->id) impl) ,@(map loop args))])))
