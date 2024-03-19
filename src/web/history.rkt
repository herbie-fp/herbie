#lang racket

(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench core->tex supported-by-lang?)
         json)
(require "../syntax/types.rkt" "../syntax/rules.rkt" "../core/bsearch.rkt" 
         "../alternative.rkt" "../common.rkt" "../float.rkt" "../points.rkt" 
         "../programs.rkt" "../syntax/sugar.rkt" "common.rkt")
(provide render-history render-json)

(define (split-pcontext pcontext splitpoints alts ctx)
  (define preds (splitpoints->point-preds splitpoints alts ctx))

  (for/list ([pred preds])
    (define-values (pts* exs*)
      (for/lists (pts exs)
        ([(pt ex) (in-pcontext pcontext)] #:when (pred pt))
        (values pt ex)))

    ;; TODO: The (if) here just corrects for the possibility that we
    ;; might have sampled new points that include no points in a given
    ;; regime. Instead it would be best to continue sampling until we
    ;; actually have many points in each regime. That would require
    ;; breaking some abstraction boundaries right now so we haven't
    ;; done it yet.
    (if (null? pts*) pcontext (mk-pcontext pts* exs*))))

(struct interval (alt-idx start-point end-point expr))

(define (interval->string ival repr)
  (define start (interval-start-point ival))
  (define end (interval-end-point ival))
  (string-join
   (list
    (if start
        (format "~a < " (value->string start repr))
        "")
    (~a (interval-expr ival))
    (if (equal? end +nan.0)
        ""
        (format " < ~a" (value->string end repr))))))

(define (splice-proof-step step)
  (let/ec k
    (let loop ([expr step] [loc '()])
      (match expr
        [(list 'Rewrite=> rule sub)
         (define loc* (reverse loc))
         (k 'Rewrite=> rule loc* (location-do loc* step (λ _ sub)))]
        [(list 'Rewrite<= rule sub)
         (define loc* (reverse loc))
         (k 'Rewrite<= rule loc* (location-do loc* step (λ _ sub)))]
        [(list op args ...)
         (for ([arg (in-list args)] [i (in-naturals 1)])
           (loop arg (cons i loc)))]
        [_ (void)]))
    (k 'Goal #f '() step)))

;; HTML renderer for derivations
(define/contract (render-history altn pcontext pcontext2 ctx)
  (-> alt? pcontext? pcontext? context? (listof xexpr?))

  (define repr (context-repr ctx))
  (define err
    (format-accuracy (errors-score (errors (alt-expr altn) pcontext ctx)) (representation-total-bits repr) #:unit "%"))
  (define err2
    (format "~a on training set" (format-accuracy (errors-score (errors (alt-expr altn) pcontext2 ctx)) (representation-total-bits repr) #:unit "%")))

  (match altn
    [(alt prog 'start (list) _)
     (list
      `(li (p "Initial program " (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[" ,(program->tex prog ctx) "\\]")))]

    [(alt prog `(start ,strategy) `(,prev) _)
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li ([class "event"]) "Using strategy " (code ,(~a strategy))))]

    [(alt prog 'add-preprocessing `(,prev) _)
      ;; TODO message to user is? proof later
      `(,@(render-history prev pcontext pcontext2 ctx)
        (li "Add Preprocessing"))]	

    [(alt _ `(regimes ,splitpoints) prevs _)
     (define intervals
       (for/list ([start-sp (cons (sp -1 -1 #f) splitpoints)] [end-sp splitpoints])
         (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp))))

     `((li ([class "event"]) "Split input into " ,(~a (length prevs)) " regimes")
       (li
        ,@(apply
           append
           (for/list ([entry prevs] [idx (in-naturals)]
                      [new-pcontext (split-pcontext pcontext splitpoints prevs ctx)]
                      [new-pcontext2 (split-pcontext pcontext2 splitpoints prevs ctx)])
             (define entry-ivals (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx)) intervals))
             (define condition (string-join (map (curryr interval->string repr) entry-ivals) " or "))
             `((h2 (code "if " (span ([class "condition"]) ,condition)))
               (ol ,@(render-history entry new-pcontext new-pcontext2 ctx))))))
       (li ([class "event"]) "Recombined " ,(~a (length prevs)) " regimes into one program."))]

    [(alt prog `(taylor ,loc ,pt ,var) `(,prev) _)
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Taylor expanded in " ,(~a var)
              " around " ,(~a pt) " " (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(program->tex prog ctx #:loc loc) "\\]")))]

    [(alt prog `(simplify ,loc ,input ,proof ,soundiness) `(,prev) _)
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li ,(if proof (render-proof proof soundiness pcontext ctx) ""))
       (li (p "Simplified" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(program->tex prog ctx #:loc loc) 
                "\\]")))]

    [(alt prog `initial-simplify `(,prev) _)
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Initial simplification" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(program->tex prog ctx) "\\]")))]

    [(alt prog `final-simplify `(,prev) _)
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Final simplification" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(program->tex prog ctx) "\\]")))]

    [(alt prog `(rr ,loc ,input ,proof ,soundiness) `(,prev) _)
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li ,(if proof (render-proof proof soundiness pcontext ctx) ""))
       (li (p "Applied " (span ([class "rule"]) , (if (rule? input) "rewrite-once" "egg-rr"))
              (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(program->tex prog ctx #:loc loc) "\\]")))]
    ))

(define (render-proof proof soundiness pcontext ctx)
  `(div ([class "proof"])
    (details
     (summary "Step-by-step derivation")
     (ol
      ,@(for/list ([step proof] [sound soundiness])
          (define-values (dir rule loc expr) (splice-proof-step step))
          (define step-prog (program->fpcore expr ctx))
          (define err
            (format-accuracy (errors-score (errors expr pcontext ctx))
                             (representation-total-bits (context-repr ctx))
                             #:unit "%"))
          (if (equal? dir 'Goal)
              ""
              `(li ,(let ([dir (match dir ['Rewrite<= "right to left"] ['Rewrite=> "left to right"])]
                          [tag (string-append (format " ↑ ~a" (first sound))
                                              (format " ↓ ~a" (second sound)))])
                      `(p (code ([title ,dir]) ,(~a rule))
                          (span ([class "error"] [title ,tag]) ,err)))
                   (div ([class "math"])
                        "\\[\\leadsto "
                        ,(core->tex step-prog #:loc (cons 2 loc) #:color "blue")
                        "\\]"))))))))


(define (render-json altn pcontext pcontext2 ctx)
  (define repr (context-repr ctx))
  (define err (errors-score (errors (alt-expr altn) pcontext ctx)))
  (define err2 (errors-score (errors (alt-expr altn) pcontext2 ctx)))

  (match altn
    [(alt prog 'start (list) _)
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx)))
            (type . "start")
            (error . ,err)
            (training-error . ,err2))]

    [(alt prog `(start ,strategy) `(,prev) _)
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx))) 
            (type . "strategy") 
            (strategy . ,(~a strategy)) 
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (error . ,err) 
            (training-error . ,err2))]

    [(alt prog `(regimes ,splitpoints) prevs _)
     (define intervals
       (for/list ([start-sp (cons (sp -1 -1 #f) splitpoints)] [end-sp splitpoints])
         (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp))))

     `#hash((program . ,(fpcore->string (program->fpcore prog ctx))) 
            (type . "regimes") 
            (conditions . ,(for/list ([entry prevs] [idx (in-naturals)]) 
                (let ([entry-ivals (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx)) intervals)]) (map (curryr interval->string repr) entry-ivals))))
            (prevs . ,(for/list ([entry prevs] 
                                 [new-pcontext (split-pcontext pcontext splitpoints prevs ctx)]
                                 [new-pcontext2 (split-pcontext pcontext2 splitpoints prevs ctx)])
                        (render-json entry new-pcontext new-pcontext2 ctx))))]

    [(alt prog `(taylor ,loc ,pt ,var) `(,prev) _)
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx))) 
            (type . "taylor") 
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (pt . ,(~a pt))
            (var . ,(~a var))
            (loc . ,loc)
            (error . ,err) 
            (training-error . ,err2))]

    [(alt prog `(simplify ,loc ,input ,proof ,soundiness) `(,prev) _)
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx))) 
            (type . "simplify") 
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (proof . ,(if proof (render-proof-json proof soundiness pcontext ctx) (json-null)))
            (loc . ,loc)
            (error . ,err) 
            (training-error . ,err2))]

    [(alt prog `initial-simplify `(,prev) _)
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx))) 
            (type . "initial-simplify") 
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (error . ,err) 
            (training-error . ,err2))]

    [(alt prog `final-simplify `(,prev) _)
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx))) 
            (type . "final-simplify") 
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (error . ,err) 
            (training-error . ,err2))]

    [(alt prog `(rr ,loc ,input ,proof ,soundiness) `(,prev) _)
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx))) 
            (type . "rr") 
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (proof . ,(if proof (render-proof-json proof soundiness pcontext ctx) (json-null)))
            (rule . ,(if (rule? input) "rewrite-once" "egg-rr"))
            (loc . ,loc)
            (error . ,err) 
            (training-error . ,err2))]

    [(alt prog 'add-preprocessing `(,prev) preprocessing)
     `#hash((program . ,(fpcore->string (program->fpcore prog ctx)))
            (type . "add-preprocessing")
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (error . ,err)
            (training-error . ,err2)
            (preprocessing . ,preprocessing))]))

(define (render-proof-json proof soundiness pcontext ctx)
      (for/list ([step proof] [sound soundiness])
          (define-values (dir rule loc expr) (splice-proof-step step))
          (define err (errors-score (errors expr pcontext ctx)))

          `#hash(
                  (error . ,err)
                  (program . ,(fpcore->string (program->fpcore expr ctx)))
                  (direction . ,(match dir ['Rewrite<= "rtl"] ['Rewrite=> "ltr"] ['Goal "goal"]))
                  (rule . ,(~a rule))
                  (loc . ,loc)
                  (tag . ,(string-append (format " ↑ ~a" (first sound))
                                              (format " ↓ ~a" (second sound))))
                  )))

