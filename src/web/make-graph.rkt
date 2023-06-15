#lang racket

(require (only-in xml write-xexpr xexpr?) 
         (only-in fpbench core->tex *expr-cse-able?*
                          [core-common-subexpr-elim core-cse]))

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../alternative.rkt" "../syntax/types.rkt" "../cost.rkt"
         "../syntax/read.rkt" "../core/bsearch.rkt" "../sandbox.rkt"
         "common.rkt" "history.rkt" "../syntax/sugar.rkt" "timeline.rkt")

(provide make-graph)

(define/contract (regime-info altn)
  (-> alt? (or/c (listof sp?) #f))
  (let loop ([altn altn])
    (match altn
      [(alt _ `(regimes ,splitpoints) prevs) splitpoints]
      [(alt _ _ (list)) #f]
      [(alt _ _ (list prev _ ...)) (loop prev)])))

(define/contract (render-interactive vars point)
  (-> (listof symbol?) (listof number?) xexpr?)
  `(div ([id "try-it"] [style "display: flow-root"])
    (div ([id "try-inputs-wrapper"])
     (form ([id "try-inputs"])
      (p ([class "header"]) "Your Program's Arguments")
       (ol
        ,@(for/list ([var-name (in-list vars)] [i (in-naturals)] [val point])
            `(li (label ([for ,(string-append "var-name-" (~a i))]) ,(~a var-name))
                 (input ([type "text"] [class "input-submit"]
                         [name ,(string-append "var-name-" (~a i))]
                         [value ,(~a val)])))))))
      (div ([id "try-result"] [class "no-error"])
       (p ([class "header"]) "Results")
        (table
         (tbody
           (tr (td (label ([for "try-original-output"]) "In"))
               (td (output ([id "try-original-output"]))))
           (tr (td (label ([for "try-herbie-output"]) "Out"))
               (td (output ([id "try-herbie-output"]))))))
        (div ([id "try-error"]) "Enter valid numbers for all inputs"))))

(define (alt->tex alt ctx)
  (core->tex (core-cse (program->fpcore (alt-expr alt) ctx))))

(define (at-least-two-ops? expr)
  (match expr
   [(list op args ...) (ormap list? args)]
   [_ #f]))

(define (make-graph result out fpcore? profile?)
  (match-define (job-result test _ time _ warnings backend) result)
  (define vars (test-vars test))
  (define repr (test-output-repr test))
  (define repr-bits (representation-total-bits repr))
  (define ctx (test-context test))
  (match-define (improve-result preprocess pctxs start target end bogosity) backend)

  (match-define (alt-analysis start-alt _ start-error) start)
  (define target-alt (and target (alt-analysis-alt target)))
  (define target-error (and target (alt-analysis-test-errors target)))
  (define-values (end-alts end-errors end-costs)
    (for/lists (l1 l2 l3) ([analysis end])
      (match-define (alt-analysis alt _ test-errs) analysis)
      (values alt test-errs (expr-cost (alt-expr alt) repr))))

  (match-define (list train-pctx test-pctx) pctxs)
  (define-values (points _) (pcontext->lists train-pctx))

  (define end-alt (car end-alts))
  (define end-error (car end-errors))

  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (meta ([charset "utf-8"]))
      (title "Result for " ,(~a (test-name test)))
      ,@js-tex-include
      (script ([src "https://unpkg.com/mathjs@4.4.2/dist/math.min.js"]))
      (script ([src "https://unpkg.com/d3@6.7.0/dist/d3.min.js"]))
      (script ([src "https://unpkg.com/@observablehq/plot@0.4.3/dist/plot.umd.min.js"]))
      (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
      (script ([src "interactive.js"]))
      (script ([src "../report.js"] [type "module"]))
      )
     (body
      ,(render-menu #:path ".."
        (~a (test-name test))
        (list
         '("Report" . "../index.html")
         '("Metrics" . "timeline.html")))

      (div ([id "large"])
       ,(render-comparison
         "Percentage Accurate"
         (format-accuracy (errors-score start-error) repr-bits #:unit "%")
         (format-accuracy (errors-score end-error) repr-bits #:unit "%")
         #:title
         (format "Minimum Accuracy: ~a → ~a"
                 (format-accuracy (apply max (map ulps->bits start-error)) repr-bits #:unit "%")
                 (format-accuracy (apply max (map ulps->bits end-error)) repr-bits #:unit "%")))
       ,(render-large "Time" (format-time time))
       ,(render-large "Precision" `(kbd ,(~a (representation-name repr))))
       ,(if (*pareto-mode*)
            (render-large "Cost" `(kbd ,(format-cost (car end-costs) repr)))
            ""))

      ,(render-warnings warnings)

      (section
        (details ([id "specification"] [class "section"])
          (summary (h2 "Specification")
                   (a ([class "help-button float"] 
                       [href "/doc/latest/report.html#spec"] 
                       [target "_blank"]) "?"))
          (div ([class "math"]) "\\[" ,(alt->tex start-alt ctx) "\\]")
          ,(render-bogosity bogosity)

          ,(if (and fpcore? (for/and ([p points]) (andmap number? p)))
               (render-interactive vars (car points))
               "")))
      
      (figure ([id "graphs"])
        (h2 "Local Percentage Accuracy vs "
            (span ([id "variables"]))
            (a ([class "help-button float"] 
                [href "/doc/latest/report.html#graph"] 
                [target "_blank"]) "?"))
        (svg)
        (div ([id "functions"]))
        (figcaption
         "The average percentage accuracy by input value. Horizontal axis shows "
         "value of an input variable; the variable is choosen in the title. "
         "Vertical axis is accuracy; higher is better. Red represent the original "
         "program, while blue represents Herbie's suggestion. "
         "These can be toggled with buttons below the plot. "
         "The line is an average while dots represent individual samples."))


      (section ([id "cost-accuracy"] [class "section"]
                [data-benchmark-name ,(~a (test-name test))])
        (h2 "Accuracy vs Speed"
            (a ([class "help-button float"] 
                [href "/doc/latest/report.html#cost-accuracy"] 
                [target "_blank"]) "?"))
        (div ([class "figure-row"])
          (svg)
          (div
            (p "Herbie found "  ,(~a (length end-alts)) " alternatives:")
            (table
             (thead (tr (th "Alternative") 
                        (th ([class "numeric"]) "Accuracy")
                        (th ([class "numeric"]) "Speedup")))
             (tbody))))
          (figcaption
           "The accuracy (vertical axis) and speed (horizontal axis) of each "
           "alternatives. Up and to the right is better. The red square shows "
           "the initial program, and each blue circle shows an alternative."
           "The line shows the best available speed-accuracy tradeoffs."))

      ,(if (test-output test)
           `(section ([id "comparison"])
             (h1 "Target")
             (table
              (tr (th "Original") (td ,(format-accuracy (errors-score start-error) repr-bits #:unit "%")))
              (tr (th "Target") (td ,(format-accuracy (errors-score target-error) repr-bits #:unit "%")))
              (tr (th "Herbie") (td ,(format-accuracy (errors-score end-error) repr-bits #:unit "%"))))
             (div ([class "math"]) "\\[" ,(program->tex (test-output test) ctx) "\\]"))
           "")

      ,@(for/list ([alt end-alts] [i (in-naturals 1)])
          `(section ([id ,(format "alternative~a" i)])
            (h2 "Alternative " ,(~a i))
            (div ([class "math"])
                 "\\[" ,(parameterize ([*expr-cse-able?* at-least-two-ops?])
                          (alt->tex alt ctx))
                 "\\]")
            (details
             (summary "Derivation")
             (ol ([class "history"])
                 ,@(render-history alt train-pctx test-pctx ctx)))))

      ,(render-reproduction test)))
    out))
