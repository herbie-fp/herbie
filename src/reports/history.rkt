#lang racket

(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench core->tex supported-by-lang?)
         json)
(require "../core/rules.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../core/bsearch.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../core/points.rkt"
         "../core/programs.rkt"
         "common.rkt")
(provide collect-expressions
         render-history
         render-json)

(struct interval (alt-idx start-point end-point expr))

(define (interval->string ival repr)
  (define start (interval-start-point ival))
  (define end (interval-end-point ival))
  (string-join (list (if start
                         (format "~a < " (value->string start repr))
                         "")
                     (~a (interval-expr ival))
                     (if (equal? end +nan.0)
                         ""
                         (format " < ~a" (value->string end repr))))))

(define (splice-proof-step step)
  (let/ec k
    (let loop ([expr step]
               [loc '()])
      (match expr
        [(list 'Rewrite=> rule sub)
         (define loc* (reverse loc))
         (k 'Rewrite=> rule loc* (location-set loc* step sub))]
        [(list 'Rewrite<= rule sub)
         (define loc* (reverse loc))
         (k 'Rewrite<= rule loc* (location-set loc* step sub))]
        [(approx spec impl)
         (loop spec (cons 1 loc))
         (loop impl (cons 2 loc))]
        [(hole prec spec) (loop spec (cons 1 loc))]
        [(list op args ...)
         (for ([arg (in-list args)]
               [i (in-naturals 1)])
           (loop arg (cons i loc)))]
        [_ (void)]))
    (k 'Goal #f '() step)))

(define (altn-errors altn pcontext ctx errcache mask)
  (define repr (context-repr ctx))
  (define repr-bits (representation-total-bits repr))
  (define err (errors-score-masked (hash-ref errcache (alt-expr altn)) mask))
  (format-accuracy err repr-bits #:unit "%"))

(define (expr->fpcore expr ctx #:ident [ident #f])
  (list 'FPCore
        (context-vars ctx)
        (let loop ([expr expr])
          (match expr
            [(? symbol?) expr]
            [(? number?) expr]
            [(? literal?) (literal-value expr)]
            [(approx spec impl) (loop impl)]
            [(hole precision spec) (loop spec)]
            [(list op args ...) (cons op (map loop args))]))))

(define (mixed->fpcore expr ctx)
  (define expr*
    (let loop ([expr expr])
      (match expr
        [(? symbol?) expr]
        [(? number?) expr]
        [(? literal?) (literal-value expr)]
        [(approx _ impl) (loop impl)]
        [(hole precision spec) (loop spec)]
        [`(if ,cond ,ift ,iff)
         `(if ,(loop cond)
              ,(loop ift)
              ,(loop iff))]
        [`(,(? impl-exists? impl) ,args ...)
         ; use the FPCore operator without rounding properties
         (define args* (map loop args))
         (define vars (impl-info impl 'vars))
         (define pattern
           (match (impl-info impl 'fpcore)
             [(list '! _ ... body) body]
             [body body]))
         (replace-vars (map cons vars args*) pattern)]
        [`(,op ,args ...) `(,op ,@(map loop args))])))
  `(FPCore ,(context-vars ctx) ,expr*))

(define (collect-expressions altn)
  (reap [sow]
        (let loop ([altn altn])
          (when (impl-prog? (alt-expr altn))
            (sow (alt-expr altn)))

          (match altn
            [(alt prog 'start (list) _) (void)]
            [(alt prog 'add-preprocessing `(,prev) _) (loop prev)]
            [(alt prog `(evaluate ,loc) `(,prev) _) (loop prev)]
            [(alt _ `(regimes ,splitpoints) prevs _) (for-each loop prevs)]
            [(alt prog `(taylor ,loc ,pt ,var) `(,prev) _) (loop prev)]
            [(alt prog `(rr ,loc ,input ,proof) `(,prev) _)
             (loop prev)
             (when proof
               (for ([step proof])
                 (define-values (dir rule loc expr) (splice-proof-step step))
                 (when (impl-prog? expr)
                   (sow expr))))]))))

(define (and-fn a b)
  (and a b))

(define (make-mask pcontext)
  (make-list (pcontext-length pcontext) #f))

;; HTML renderer for derivations
(define (render-history altn pcontext ctx errcache [mask (make-mask pcontext)])
  (match altn
    [(alt prog 'start (list) _)
     (define err (altn-errors altn pcontext ctx errcache mask))
     (list `(li (p "Initial program " (span ((class "error")) ,err))
                (div ((class "math")) "\\[" ,(program->tex prog ctx) "\\]")))]

    [(alt prog 'add-preprocessing `(,prev) _)
     ;; TODO message to user is? proof later
     `(,@(render-history prev pcontext ctx errcache mask) (li "Add Preprocessing"))]

    [(alt _ `(regimes ,splitpoints) prevs _)
     (define intervals
       (for/list ([start-sp (cons (sp -1 -1 #f) splitpoints)]
                  [end-sp splitpoints])
         (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp))))
     (define repr (context-repr ctx))

     `((li ((class "event")) "Split input into " ,(~a (length prevs)) " regimes")
       (li ,@(apply append
                    (for/list ([entry prevs]
                               [idx (in-naturals)]
                               [new-mask (regimes-pcontext-masks pcontext splitpoints prevs ctx)])
                      (define mask* (map and-fn mask new-mask))
                      (define entry-ivals
                        (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx)) intervals))
                      (define condition
                        (string-join (map (curryr interval->string repr) entry-ivals) " or "))
                      `((h2 (code "if " (span ((class "condition")) ,condition)))
                        (ol ,@(render-history entry pcontext ctx errcache mask*))))))
       (li ((class "event")) "Recombined " ,(~a (length prevs)) " regimes into one program."))]

    [(alt prog `(taylor ,loc ,pt ,var) `(,prev) _)
     (define core (mixed->fpcore prog ctx))
     `(,@(render-history prev pcontext ctx errcache mask)
       (li (p "Taylor expanded in " ,(~a var) " around " ,(~a pt))
           (div ((class "math"))
                "\\[\\leadsto "
                ,(core->tex core #:loc (and loc (cons 2 loc)) #:color "blue")
                "\\]")))]

    [(alt prog `(evaluate ,loc) `(,prev) _)
     (define core (mixed->fpcore prog ctx))
     (define err (altn-errors altn pcontext ctx errcache mask))
     `(,@(render-history prev pcontext ctx errcache mask)
       (li (p "Evaluated real constant" (span ((class "error")) ,err))
           (div ((class "math"))
                "\\[\\leadsto "
                ,(core->tex core #:loc (and loc (cons 2 loc)) #:color "blue")
                "\\]")))]

    [(alt prog `(rr ,loc ,input ,proof) `(,prev) _)
     (define err (altn-errors altn pcontext ctx errcache mask))
     `(,@(render-history prev pcontext ctx errcache mask)
       (li ,(if proof
                (render-proof (render-proof-json proof pcontext ctx errcache mask) ctx)
                ""))
       (li (p "Applied rewrites" (span ((class "error")) ,err))
           (div ((class "math")) "\\[\\leadsto " ,(program->tex prog ctx #:loc loc) "\\]")))]))

(define (errors-score-masked errs mask)
  (if (ormap identity mask)
      (errors-score (for/list ([err (in-list errs)]
                               [use? (in-list mask)]
                               #:when use?)
                      err))
      (errors-score errs)))

(define (render-proof proof-json ctx)
  `(div ([class "proof"])
        (details (summary "Step-by-step derivation")
                 (ol ,@(for/list ([step (in-list proof-json)])
                         (define dir
                           (match (hash-ref step 'direction)
                             ["goal" "goal"]
                             ["rtl" "right to left"]
                             ["ltr" "left to right"]))
                         (define err (hash-ref step 'error))
                         (define loc (hash-ref step 'loc))
                         (define rule (hash-ref step 'rule))
                         (define prog (read (open-input-string (hash-ref step 'program))))
                         (define bits (representation-total-bits (context-repr ctx)))
                         (if (equal? dir "goal")
                             ""
                             `(li (p (code ([title ,dir]) ,rule)
                                     (span ([class "error"]) ,(format-accuracy err bits)))
                                  (div ((class "math"))
                                       "\\[\\leadsto "
                                       ,(core->tex prog #:loc (and loc (cons 2 loc)) #:color "blue")
                                       "\\]"))))))))

(define (render-json altn pcontext ctx errcache [mask (make-list (pcontext-length pcontext) #f)])
  (define repr (context-repr ctx))
  (define err
    (if (impl-prog? (alt-expr altn))
        (errors-score-masked (hash-ref errcache (alt-expr altn)) mask)
        "N/A"))

  (match altn
    [(alt prog 'start (list) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx))) (type . "start") (error . ,err))]

    [(alt prog `(regimes ,splitpoints) prevs _)
     (define intervals
       (for/list ([start-sp (cons (sp -1 -1 #f) splitpoints)]
                  [end-sp splitpoints])
         (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp))))

     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "regimes")
            (conditions . ,(for/list ([entry prevs]
                                      [idx (in-naturals)])
                             (define entry-ivals
                               (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx)) intervals))
                             (map (curryr interval->string repr) entry-ivals)))
            (prevs . ,(for/list ([entry prevs]
                                 [new-mask (regimes-pcontext-masks pcontext splitpoints prevs ctx)])
                        (define mask* (map and-fn mask new-mask))
                        (render-json entry pcontext ctx errcache mask*))))]

    [(alt prog `(taylor ,loc ,pt ,var) `(,prev) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "taylor")
            (prev . ,(render-json prev pcontext ctx errcache mask))
            (pt . ,(~a pt))
            (var . ,(~a var))
            (loc . ,loc)
            (error . ,err))]

    [(alt prog `(evaluate ,loc) `(,prev) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "evaluate")
            (prev . ,(render-json prev pcontext ctx errcache mask))
            (loc . ,loc)
            (error . ,err))]

    [(alt prog `(rr ,loc ,input ,proof) `(,prev) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "rr")
            (prev . ,(render-json prev pcontext ctx errcache mask))
            (proof . ,(if proof
                          (render-proof-json proof pcontext ctx errcache mask)
                          (json-null)))
            (loc . ,loc)
            (error . ,err))]

    [(alt prog 'add-preprocessing `(,prev) preprocessing)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "add-preprocessing")
            (prev . ,(render-json prev pcontext ctx errcache mask))
            (error . ,err)
            (preprocessing . ,(map (curry map symbol->string) preprocessing)))]))

(define (render-proof-json proof pcontext ctx errcache mask)
  (for/list ([step proof])
    (define-values (dir rule loc expr) (splice-proof-step step))
    (define-values (err fpcore)
      (cond
        [(impl-prog? expr)
         (values (errors-score-masked (hash-ref errcache expr) mask)
                 (program->fpcore expr ctx))]
        [else
         (values "N/A" (mixed->fpcore expr ctx))]))

    `#hash((error . ,err)
           (program . ,(fpcore->string fpcore))

           (direction . ,(match dir
                           ['Rewrite<= "rtl"]
                           ['Rewrite=> "ltr"]
                           ['Goal "goal"]))
           (rule . ,(~a rule))
           (loc . ,loc))))
