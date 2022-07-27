#lang racket

(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench core->tex supported-by-lang?))
(require "../points.rkt" "../float.rkt" "../alternative.rkt" "../syntax/types.rkt"
         "../syntax/rules.rkt" "../core/regimes.rkt" "../common.rkt"
         "common.rkt" "../syntax/sugar.rkt")
(provide render-history)

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

(define/contract (render-history altn pcontext pcontext2 ctx)
  (-> alt? pcontext? pcontext? context? (listof xexpr?))

  (define repr (context-repr ctx))
  (define err
    (format-bits (errors-score (errors (alt-program altn) pcontext ctx))))
  (define err2
    (format "Internally ~a" (format-bits (errors-score (errors (alt-program altn) pcontext2 ctx)))))

  (match altn
    [(alt prog 'start (list))
     (define prog* (program->fpcore (resugar-program prog repr)))
     (list
      `(li (p "Initial program " (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[" ,(if (supported-by-lang? prog* "tex") (core->tex prog*) "ERROR") "\\]")))]
    [(alt prog `(start ,strategy) `(,prev))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li ([class "event"]) "Using strategy " (code ,(~a strategy))))]

    [(alt _ `(regimes ,splitpoints) prevs)
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

    [(alt prog `(taylor ,pt ,var ,loc) `(,prev))
     (define prog* (program->fpcore (resugar-program prog repr)))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Taylor expanded in " ,(~a var)
              " around " ,(~a pt) " " (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(if (supported-by-lang? prog* "tex") 
                                                      (core->tex prog* #:loc loc #:color "blue") 
                                                      "ERROR")        
                                                  "\\]")))]

    [(alt prog `(simplify ,loc ,proof ,soundiness) `(,prev))
     (define prog* (program->fpcore (resugar-program prog repr)))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Simplified" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(if (supported-by-lang? prog* "tex") 
                                                      (core->tex prog* #:loc loc #:color "blue") 
                                                      "ERROR") 
                                                  "\\]")
           ;; TODO display soundiness here in comments
           #;(div ([class "math"]) "Proof")
           #;,@(for/list ([step proof] [data soundiness])
                       (define text (format "~a: ~a points increase in error, ~a points decrease in error"
                                    step (first data) (second data)))
                       `(div ([class "math"])
                             ,(if #f (> (first data) 0)
                                  `(b ,text)
                                  text)))))]

    [(alt prog `initial-simplify `(,prev))
     (define prog* (program->fpcore (resugar-program prog repr)))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Initial simplification" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(if (supported-by-lang? prog* "tex") (core->tex prog*) "ERROR") "\\]")))]

    [(alt prog `final-simplify `(,prev))
     (define prog* (program->fpcore (resugar-program prog repr)))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Final simplification" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(if (supported-by-lang? prog* "tex") (core->tex prog*) "ERROR") "\\]")))]

    [(alt prog (list 'change cng) `(,prev))
     (define prog* (program->fpcore (resugar-program prog repr)))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Applied " (span ([class "rule"]) ,(~a (rule-name (change-rule cng))))
              (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(if (supported-by-lang? prog* "tex") 
                                                      (core->tex prog* #:loc (change-location cng) #:color "blue")
                                                      "ERROR")
                                                  "\\]")))]
    ))
