#lang racket

(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench core->tex supported-by-lang?)
         json)
(require "../core/rules.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/platform.rkt"
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
  (define err (errors-score-masked (hash-ref errcache (alt-expr altn)) mask))
  (format-accuracy err repr #:unit "%"))

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
            [(alt prog 'start (list)) (void)]
            [(alt prog 'add-preprocessing `(,prev)) (loop prev)]
            [(alt prog `(evaluate ,loc) `(,prev)) (loop prev)]
            [(alt _ `(regimes ,splitpoints) prevs) (for-each loop prevs)]
            [(alt prog `(taylor ,loc ,pt ,var) `(,prev)) (loop prev)]
            [(alt prog `(rr ,loc ,input ,proof) `(,prev))
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
(define (render-history json ctx)
  (define err
    (match (hash-ref json 'error)
      [(? number? n) (format-accuracy n (context-repr ctx) #:unit "%")]
      [other other]))
  (define prog (read (open-input-string (hash-ref json 'program))))
  (match (hash-ref json 'type)
    ["start"
     (list `(li (p "Initial program " (span ((class "error")) ,err))
                (div ((class "math")) "\\[" ,(fpcore->tex prog) "\\]")))]

    ["add-preprocessing" `(,@(render-history (hash-ref json 'prev) ctx) (li "Add Preprocessing"))]

    ["regimes"
     (define prevs (hash-ref json 'prevs))
     `((li ((class "event")) "Split input into " ,(~a (length prevs)) " regimes")
       (li ,@(apply append
                    (for/list ([entry (in-list prevs)]
                               [condition (in-list (hash-ref json 'conditions))])
                      `((h2 (code "if " (span ((class "condition")) ,(string-join condition " or "))))
                        (ol ,@(render-history entry ctx))))))
       (li ((class "event")) "Recombined " ,(~a (length prevs)) " regimes into one program."))]

    ["taylor"
     (define-values (prev pt var loc) (apply values (map (curry hash-ref json) '(prev pt var loc))))
     `(,@(render-history prev ctx)
       (li (p "Taylor expanded in " ,var " around " ,pt)
           (div ((class "math")) "\\[\\leadsto " ,(fpcore->tex prog #:loc loc) "\\]")))]

    ["evaluate"
     (define-values (prev loc) (apply values (map (curry hash-ref json) '(prev loc))))
     `(,@(render-history prev ctx)
       (li (p "Evaluated real constant" (span ((class "error")) ,err))
           (div ((class "math")) "\\[\\leadsto " ,(fpcore->tex prog #:loc loc) "\\]")))]

    ["rr"
     (define-values (prev loc proof) (apply values (map (curry hash-ref json) '(prev loc proof))))
     `(,@(render-history prev ctx)
       (li ,(if (eq? proof (json-null))
                ""
                (render-proof proof ctx)))
       (li (p "Applied rewrites" (span ((class "error")) ,err))
           (div ((class "math")) "\\[\\leadsto " ,(fpcore->tex prog #:loc loc) "\\]")))]))

(define (errors-score-masked errs mask)
  (if (ormap identity mask)
      (errors-score (for/list ([err (in-list errs)]
                               [use? (in-list mask)]
                               #:when use?)
                      err))
      (errors-score errs)))

(define (render-proof proof-json ctx)
  `(div
    ((class "proof"))
    (details
     (summary "Step-by-step derivation")
     (ol ,@
         (for/list ([step (in-list proof-json)])
           (define-values (direction err loc rule prog-str)
             (apply values (map (curry hash-ref step) '(direction error loc rule program))))
           (define dir
             (match direction
               ["goal" "goal"]
               ["rtl" "right to left"]
               ["ltr" "left to right"]))
           (define prog (read (open-input-string prog-str)))
           (if (equal? dir "goal")
               ""
               `(li (p (code ([title ,dir]) ,rule)
                       (span ((class "error"))
                             ,(if (number? err)
                                  (format-accuracy err (context-repr ctx) #:unit "%")
                                  err)))
                    (div ((class "math")) "\\[\\leadsto " ,(fpcore->tex prog #:loc loc) "\\]"))))))))

(define (render-json altn pcontext ctx errcache [mask (make-list (pcontext-length pcontext) #f)])
  (define repr (context-repr ctx))
  (define err
    (if (impl-prog? (alt-expr altn))
        (errors-score-masked (hash-ref errcache (alt-expr altn)) mask)
        "N/A"))

  (match altn
    [(alt prog 'start (list))
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx))) (type . "start") (error . ,err))]

    [(alt prog `(regimes ,splitpoints) prevs)
     (define intervals
       (for/list ([start-sp (cons (sp -1 -1 #f) splitpoints)]
                  [end-sp splitpoints])
         (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp))))

     `#hash((program . ,(fpcore->string (program->fpcore prog ctx)))
            (type . "regimes")
            (error . ,err)
            (conditions . ,(for/list ([entry prevs]
                                      [idx (in-naturals)])
                             (define entry-ivals
                               (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx)) intervals))
                             (map (curryr interval->string repr) entry-ivals)))
            (prevs . ,(for/list ([entry prevs]
                                 [new-mask (regimes-pcontext-masks pcontext splitpoints prevs ctx)])
                        (define mask* (map and-fn mask new-mask))
                        (render-json entry pcontext ctx errcache mask*))))]

    [(alt prog `(taylor ,loc ,pt ,var) `(,prev))
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx)))
            (type . "taylor")
            (prev . ,(render-json prev pcontext ctx errcache mask))
            (pt . ,(~a pt))
            (var . ,(~a var))
            (loc . ,loc)
            (error . ,err))]

    [(alt prog `(evaluate ,loc) `(,prev))
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx)))
            (type . "evaluate")
            (prev . ,(render-json prev pcontext ctx errcache mask))
            (loc . ,loc)
            (error . ,err))]

    [(alt prog `(rr ,loc ,input ,proof) `(,prev))
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx)))
            (type . "rr")
            (prev . ,(render-json prev pcontext ctx errcache mask))
            (proof . ,(if proof
                          (render-proof-json proof pcontext ctx errcache mask)
                          (json-null)))
            (loc . ,loc)
            (error . ,err))]

    [(alt prog 'add-preprocessing `(,prev))
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx)))
            (type . "add-preprocessing")
            (prev . ,(render-json prev pcontext ctx errcache mask))
            (error . ,err))]))

(define (render-proof-json proof pcontext ctx errcache mask)
  (for/list ([step proof])
    (define-values (dir rule loc expr) (splice-proof-step step))
    (define-values (err fpcore)
      (cond
        [(impl-prog? expr)
         (values (errors-score-masked (hash-ref errcache expr) mask) (program->fpcore expr ctx))]
        [else (values "N/A" (mixed->fpcore expr ctx))]))

    `#hash((error . ,err)
           (program . ,(fpcore->string fpcore))
           (direction . ,(match dir
                           ['Rewrite<= "rtl"]
                           ['Rewrite=> "ltr"]
                           ['Goal "goal"]))
           (rule . ,(~a rule))
           (loc . ,loc))))
