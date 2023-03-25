#lang racket

(require (only-in xml write-xexpr xexpr?) 
         (only-in fpbench core->tex *expr-cse-able?*
                          [core-common-subexpr-elim core-cse]))

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../alternative.rkt" "../syntax/types.rkt"
         "../syntax/read.rkt" "../core/bsearch.rkt" "../sandbox.rkt"
         "common.rkt" "history.rkt" "../syntax/sugar.rkt")
         
(provide make-graph)

(define/contract (regime-info altn)
  (-> alt? (or/c (listof sp?) #f))
  (let loop ([altn altn])
    (match altn
      [(alt _ `(regimes ,splitpoints) prevs) splitpoints]
      [(alt _ _ (list)) #f]
      [(alt _ _ (list prev _ ...)) (loop prev)])))

(define/contract (render-interactive start-prog point)
  (-> alt? (listof number?) xexpr?)
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
        ,@(for/list ([var-name (program-variables (alt-program start-prog))] [i (in-naturals)] [val point])
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

(define (alt->tex alt repr)
  (core->tex (core-cse (program->fpcore (resugar-program (alt-program alt) repr)))))

(define (at-least-two-ops? expr)
  (match expr
   [(list op args ...) (ormap list? args)]
   [_ #f]))

(define (make-graph result out fpcore? profile?)
  (match-define
   (test-success test bits time timeline warnings
                 start-alt end-alts preprocess points exacts start-est-error end-est-error
                 newpoints newexacts start-error end-errors target-error
                 start-cost costs all-alts)
   result)
  (define repr (test-output-repr test))
  (define end-alt (car end-alts))
  (define end-error (car end-errors))
  (define other-alts (cdr end-alts))
  (define other-errors (cdr end-errors))

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
         '("Report" . "../results.html")
         '("Metrics" . "timeline.html")))

      (section ([id "large"])
       (h1 
        (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#summary"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?"))
       ,(render-large "Average Error"
                      (format-bits (errors-score start-error) repr #:unit "%")
                      " → "
                      (format-bits (errors-score end-error) repr #:unit "%")
                      #:title
                      (format "Maximum error: ~a → ~a"
                              (format-bits (apply max (map ulps->bits start-error)) repr #:unit "%")
                              (format-bits (apply max (map ulps->bits end-error)) repr #:unit "%")))
       ,(render-large "Time" (format-time time))
       ,(render-large "Precision" `(kbd ,(~a (representation-name repr))))
       ,(if (*pareto-mode*)
            (render-large "Cost" `(kbd ,(format-bits (car costs) repr #:unit #f)))
            ""))

      ,(render-warnings warnings)

      ,(render-program preprocess test #:to (alt-program end-alt))

      (section ([id "graphs"]) (h1 "Error" (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#graph"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?")) (div ([id "graphs-content"])))

      ,(if (and fpcore? (for/and ([p points]) (andmap number? p)))
           (render-interactive start-alt (car points))
           "")

      ,(if (test-output test)
           `(section ([id "comparison"])
             (h1 "Target")
             (table
              (tr (th "Original") (td ,(format-bits (errors-score start-error) repr #:unit "%")))
              (tr (th "Target") (td ,(format-bits (errors-score target-error) repr #:unit "%")))
              (tr (th "Herbie") (td ,(format-bits (errors-score end-error) repr #:unit "%"))))
             (div ([class "math"]) "\\[" ,(core->tex
                                            (program->fpcore
                                              (resugar-program (test-target test) repr)))
                                         "\\]"))
           "")

      (section ([id "history"])
       (h1 "Derivation" (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#derivation"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?"))
       (ol ([class "history"])
        ,@(render-history end-alt (mk-pcontext newpoints newexacts)
                          (mk-pcontext points exacts) (test-context test))))

      ,(if (not (null? other-alts))
           `(section ([id "alternatives"])
              (h1 "Alternatives")
              ,@(for/list ([alt other-alts] [cost (cdr costs)] [errs other-errors] [idx (in-naturals 1)])
                `(div ([class "entry"])
                  (table
                    (tr (th ([style "font-weight:bold"]) ,(format "Alternative ~a" idx)))
                    (tr (th "Error") (td ,(format-bits (errors-score errs) repr #:unit "%")))
                    (tr (th "Cost") (td ,(format-bits cost repr #:unit #f))))
                  (div ([class "math"])
                    "\\[" ,(parameterize ([*expr-cse-able?* at-least-two-ops?])
                            (alt->tex alt repr))
                    "\\]"))))
            "")
                                      
      ,(if (not (null? other-alts))
          `(section ([id "cost-accuracy"])
            (h1 "Error")
            (div ([id "pareto-content"] [data-benchmark-name ,(~a (test-name test))])))
            "")

      ,(render-reproduction test)))
    out))
