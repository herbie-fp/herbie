#lang racket

(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench core->tex *expr-cse-able?* [core-common-subexpr-elim core-cse]))

(require "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/read.rkt"
         "../syntax/types.rkt"
         "../core/bsearch.rkt"
         "../core/points.rkt"
         "../api/sandbox.rkt"
         "common.rkt"
         "history.rkt")

(provide make-graph
         dummy-graph)

(define (dummy-graph command)
  `(html (head (meta ([charset "utf-8"]))
               (title "Result page for the " ,(~a command) " command is not available right now.")
               ,@js-tex-include
               (script ([src "https://www.jsdelivr.com/package/npm/mathjs@14"] [defer ""]))
               (script ([src "https://cdn.jsdelivr.net/npm/d3@7"] [defer ""]))
               (script ([src "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6"] [defer ""]))
               (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
               (script ([src "../report.js"])))
         (body (h2 "Result page for the " ,(~a command) " command is not available right now."))))

(define (make-graph result-hash output? profile?)
  (define backend (hash-ref result-hash 'backend))
  (define test (car (load-tests (open-input-string (hash-ref result-hash 'test)))))
  (define pre (test-pre test))

  (define time (hash-ref result-hash 'time))
  (define warnings (hash-ref result-hash 'warnings))
  (define repr (test-output-repr test))
  (define ctx (test-context test))
  (define identifier (test-identifier test))

  (define start-expr (read (open-input-string (hash-ref (hash-ref backend 'start) 'expr))))
  (define start-cost (hash-ref (hash-ref backend 'start) 'cost))
  (define start-error (hash-ref (hash-ref backend 'start) 'errors))

  (define targets (hash-ref backend 'target))

  (define end (hash-ref backend 'end))
  (define end-exprs
    (for/list ([end-analysis (in-list end)])
      (read (open-input-string (hash-ref end-analysis 'expr)))))
  (define end-errors (map (curryr hash-ref 'errors) end))
  (define end-costs (map (curryr hash-ref 'cost) end))
  (define end-histories
    (for/list ([end-analysis (in-list end)])
      (hash-ref end-analysis 'history)))

  ; Speedup is going to be #f if cost is 0 for each alternative
  (define speedup
    (let ([better (filter number?
                          (for/list ([err end-errors]
                                     [cost end-costs]
                                     #:when (<= (errors-score err) (errors-score start-error)))
                            (if (zero? cost) ; catching division by zero
                                #f
                                (/ start-cost cost))))])
      (and (not (null? better)) (apply max better))))

  (define end-error (car end-errors))
  (define alt-number 0)

  `(html
    (head (meta ([charset "utf-8"]))
          (title "Result for " ,(~a (test-name test)))
          ,@js-tex-include
          (script ([src "https://www.jsdelivr.com/package/npm/mathjs@14"] [defer ""]))
          (script ([src "https://cdn.jsdelivr.net/npm/d3@7"] [defer ""]))
          (script ([src "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6"] [defer ""]))
          (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
          (script ([src "../report.js"])))
    (body
     ,(render-menu #:path ".."
                   (~a (test-name test))
                   (if output?
                       (list '("Report" . "../index.html") '("Metrics" . "timeline.html"))
                       (list '("Metrics" . "timeline.html"))))
     (div ([id "large"])
          ,(render-comparison
            "Percentage Accurate"
            (format-accuracy (errors-score start-error) repr #:unit "%")
            (format-accuracy (errors-score end-error) repr #:unit "%")
            #:title (format "Minimum Accuracy: ~a → ~a"
                            (format-accuracy (apply max (map ulps->bits start-error)) repr #:unit "%")
                            (format-accuracy (apply max (map ulps->bits end-error)) repr #:unit "%")))
          ,(render-large "Time" (format-time time))
          ,(render-large "Alternatives" (~a (length end-exprs)))
          ,(render-large "Speedup"
                         (if speedup
                             (~r speedup #:precision '(= 1))
                             "N/A")
                         "×"
                         #:title "Relative speed of fastest alternative that improves accuracy."))
     ,(render-warnings warnings)
     ,(render-specification test)
     (figure ([id "graphs"])
             (h2 "Local Percentage Accuracy vs "
                 (span ([id "variables"]))
                 (a ((class "help-button float") [href ,(doc-url "report.html#graph")]
                                                 [target "_blank"])
                    "?"))
             (svg)
             (div ([id "functions"]))
             (figcaption "The average percentage accuracy by input value. Horizontal axis shows "
                         "value of an input variable; the variable is choosen in the title. "
                         "Vertical axis is accuracy; higher is better. Red represent the original "
                         "program, while blue represents Herbie's suggestion. "
                         "These can be toggled with buttons below the plot. "
                         "The line is an average while dots represent individual samples."))
     (section ([id "cost-accuracy"] (class "section") [data-benchmark-name ,(~a (test-name test))])
              ; TODO : Show all Developer Target Accuracy
              (h2 "Accuracy vs Speed"
                  (a ((class "help-button float") [href ,(doc-url "report.html#cost-accuracy")]
                                                  [target "_blank"])
                     "?"))
              (div ((class "figure-row"))
                   (svg)
                   (div (p "Herbie found " ,(~a (length end-exprs)) " alternatives:")
                        (table (thead (tr (th "Alternative")
                                          (th ((class "numeric")) "Accuracy")
                                          (th ((class "numeric")) "Speedup")))
                               (tbody))))
              (figcaption "The accuracy (vertical axis) and speed (horizontal axis) of each "
                          "alternatives. Up and to the right is better. The red square shows "
                          "the initial program, and each blue circle shows an alternative."
                          "The line shows the best available speed-accuracy tradeoffs."))
     ,(let-values ([(dropdown body) (render-program start-expr ctx #:ident identifier #:pre pre)])
        `(section ([id "initial"] (class "programs"))
                  (h2 "Initial Program"
                      ": "
                      (span ((class "subhead"))
                            (data ,(format-accuracy (errors-score start-error) repr #:unit "%"))
                            " accurate, "
                            (data "1.0×")
                            " speedup")
                      ,dropdown
                      ,(render-help "report.html#alternatives"))
                  ,body))
     ,@(for/list ([i (in-naturals 1)]
                  [expr end-exprs]
                  [errs end-errors]
                  [cost end-costs]
                  [history end-histories]
                  #:unless (equal? start-expr expr))
         (set! alt-number (add1 alt-number))
         (define-values (dropdown body) (render-program expr ctx #:ident identifier #:pre pre))
         `(section ([id ,(format "alternative~a" i)] (class "programs"))
                   (h2 "Alternative "
                       ,(~a alt-number)
                       ": "
                       (span ((class "subhead"))
                             (data ,(format-accuracy (errors-score errs) repr #:unit "%"))
                             " accurate, "
                             (data ,(if (zero? cost)
                                        "N/A"
                                        (~r (/ start-cost cost) #:precision '(= 1)))
                                   "×")
                             " speedup")
                       ,dropdown
                       ,(render-help "report.html#alternatives"))
                   ,body
                   (details (summary "Derivation")
                            (ol ((class "history")) ,@(render-history history ctx)))))
     ,@(for/list ([i (in-naturals 1)]
                  [target (in-list targets)])
         (define target-error (hash-ref target 'errors))
         (define target-cost (hash-ref target 'cost))
         (define target-expr (read (open-input-string (hash-ref target 'expr))))
         (let-values ([(dropdown body) (render-program target-expr ctx #:ident identifier)])
           `(section ([id ,(format "target~a" i)] (class "programs"))
                     (h2 "Developer Target "
                         ,(~a i)
                         ": "
                         (span ((class "subhead"))
                               (data ,(format-accuracy (errors-score target-error) repr #:unit "%"))
                               " accurate, "
                               (data ,(~r (/ start-cost target-cost) #:precision '(= 1)) "×")
                               " speedup")
                         ,dropdown
                         ,(render-help "report.html#target"))
                     ,body)))
     ,(render-reproduction test))))
