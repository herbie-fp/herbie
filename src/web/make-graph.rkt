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
  `(section ([id "try-it"])
    (h1 "Try it out" (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#try-it"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?"))
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
  (match-define (job-result test _ time data-list warnings backend) result)
  (define bogosity (hash-ref (second data-list) 'bogosity))
  (define vars (test-vars test))
  (define repr (test-output-repr test))
  (define repr-bits (representation-total-bits repr))
  (define ctx (test-context test))
  (match-define (improve-result preprocess pctxs start target end) backend)

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
      ,(render-menu
        (list
         '("Error" . "#graphs")
         (and fpcore? (for/and ([p points]) (andmap number? p))
              '("Try it out!" . "#try-it"))
         (and (test-output test)
              '("Target" . "#comparison"))
         '("Derivation" . "#history")
         '("Reproduce" . "#reproduce"))
        (list
         '("Report" . "../index.html")
         '("Metrics" . "timeline.html")))

      (section ([id "large"])
       (h1 
        (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#summary"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?"))
       ,(render-large "Average Accuracy"
                      (format-accuracy (errors-score start-error) repr-bits #:unit "%")
                      " → "
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
      ,(render-program preprocess test #:to (alt-expr end-alt))

      (section ([id "graphs"]) (h1 "Error" (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#graph"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?")) (div ([id "graphs-content"])))

      (section ([id "bogosity"])  
       (h1 "Bogosity" (a (
         [class "help-button"]
         [href "/doc/latest/report.html#bogosity"]
         [target "_blank"]
         ) "?"))
       ,@(render-phase-bogosity bogosity)
      )
      
      ,(if (and fpcore? (for/and ([p points]) (andmap number? p)))
           (render-interactive vars (car points))
           "")

      ,(if (test-output test)
           `(section ([id "comparison"])
             (h1 "Target")
             (table
              (tr (th "Original") (td ,(format-accuracy (errors-score start-error) repr-bits #:unit "%")))
              (tr (th "Target") (td ,(format-accuracy (errors-score target-error) repr-bits #:unit "%")))
              (tr (th "Herbie") (td ,(format-accuracy (errors-score end-error) repr-bits #:unit "%"))))
             (div ([class "math"]) "\\[" ,(program->tex (test-output test) ctx) "\\]"))
           "")

      (section ([id "history"])
       (h1 "Derivation" (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#derivation"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?"))
       (ol ([class "history"])
        ,@(render-history end-alt train-pctx test-pctx ctx)))

      ,(if (> (length end-alts) 1)
           `(section ([id "alternatives"])
              (h1 "Alternatives")
              ,@(for/list ([alt (cdr end-alts)]
                           [errs (cdr end-errors)]
                           [cost (cdr end-costs)]
                           [i (in-naturals 1)])
                `(div ([class "entry"])
                  (table
                    (tr (th ([style "font-weight:bold"]) ,(format "Alternative ~a" i)))
                    (tr (th "Accuracy") (td ,(format-accuracy (errors-score errs) repr-bits #:unit "%")))
                    (tr (th "Cost") (td ,(format-cost cost repr))))
                  (div ([class "math"])
                    "\\[" ,(parameterize ([*expr-cse-able?* at-least-two-ops?])
                            (alt->tex alt ctx))
                    "\\]"))))
            "")
                                      
      ,(if (> (length end-alts) 1)
          `(section ([id "cost-accuracy"])
            (h1 "Error")
            (div ([id "pareto-content"] [data-benchmark-name ,(~a (test-name test))])))
            "")

      ,(render-reproduction test)))
    out))
