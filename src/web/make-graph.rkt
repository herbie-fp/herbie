#lang racket

(require (only-in xml write-xexpr xexpr?) 
         (only-in fpbench core->tex))
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../alternative.rkt" "../interface.rkt"
         "../syntax/read.rkt" "../core/regimes.rkt" "../sandbox.rkt"
         "common.rkt" "history.rkt" "../syntax/sugar.rkt")
(provide make-graph)

(define/contract (regime-info altn)
  (-> alt? (or/c (listof sp?) #f))
  (let loop ([altn altn])
    (match altn
      [(alt _ `(regimes ,splitpoints) prevs) splitpoints]
      [(alt _ _ (list)) #f]
      [(alt _ _ (list prev _ ...)) (loop prev)])))

(define (regime-splitpoints altn)
  (map sp-point (drop-right (regime-info altn) 1)))

(define/contract (regime-var altn)
  (-> alt? (or/c expr? #f))
  (define info (regime-info altn))
  (and info (sp-bexpr (car info))))

(define/contract (render-interactive start-prog point)
  (-> alt? (listof number?) xexpr?)
  `(section ([id "try-it"])
    (h1 "Try it out")
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

(define (make-graph result out fpcore? profile?)
  (match-define
   (test-success test bits time timeline warnings
                 start-alt end-alt preprocess points exacts start-est-error end-est-error
                 newpoints newexacts start-error end-error target-error
                 baseline-error oracle-error all-alts)
   result)
  (define repr (test-output-repr test))

  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (meta ([charset "utf-8"]))
      (title "Result for " ,(~a (test-name test)))
      (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
      ,@js-tex-include
      (script ([src "../report.js"]))
      (script ([src "interactive.js"]))
      (script ([src "https://unpkg.com/mathjs@4.4.2/dist/math.min.js"])))
     (body
      ,(render-menu
        (list/true
         '("Error" . "#graphs")
         (and fpcore? (for/and ([p points]) (andmap number? p))
              '("Try it out!" . "#try-it"))
         (and (test-output test)
              '("Target" . "#comparison"))
         '("Derivation" . "#history")
         '("Reproduce" . "#reproduce"))
        (list/true
         '("Report" . "../results.html")
         '("Log" . "debug.txt")
         '("Metrics" . "timeline.html")))

      (section ([id "large"])
       ,(render-large "Average Error"
                      (format-bits (errors-score start-error) #:unit #f)
                      " → "
                      (format-bits (errors-score end-error) #:unit #f)
                      #:title
                      (format "Maximum error: ~a → ~a"
                              (format-bits (apply max (map ulps->bits start-error)) #:unit #f)
                              (format-bits (apply max (map ulps->bits end-error)) #:unit #f)))
       ,(render-large "Time" (format-time time))
       ,(render-large "Precision" `(kbd ,(~a (representation-name repr)))))

      ,(render-warnings warnings)

      ,(render-program preprocess test #:to (alt-program end-alt))

      (section ([id "graphs"])
       (h1 "Error")
       (div
        ,@(for/list ([var (test-vars test)] [idx (in-naturals)])
            (cond
             [(> (length (remove-duplicates (map (curryr list-ref idx) newpoints))) 1)
              (define split-var? (equal? var (regime-var end-alt)))
              (define title "The X axis uses an exponential scale")
              `(figure ([id ,(format "fig-~a" idx)] [class ,(if split-var? "default" "")])
                (img ([width "800"] [height "300"] [title ,title]
                      [src ,(format "plot-~a.png" idx)]))
                (img ([width "800"] [height "300"] [title ,title] [data-name "Input"]
                      [src ,(format "plot-~ar.png" idx)]))
                ,(if target-error
                     `(img ([width "800"] [height "300"] [title ,title] [data-name "Target"]
                            [src ,(format "plot-~ag.png" idx)]))
                     "")
                (img ([width "800"] [height "300"] [title ,title] [data-name "Result"]
                      [src ,(format "plot-~ab.png" idx)]))
                (figcaption (p "Bits error versus " (var ,(~a var)))))]
             [else ""]))))

      ,(if (and fpcore? (for/and ([p points]) (andmap number? p)))
           (render-interactive start-alt (car points))
           "")

      ,(if (test-output test)
           `(section ([id "comparison"])
             (h1 "Target")
             (table
              (tr (th "Original") (td ,(format-bits (errors-score start-error))))
              (tr (th "Target") (td ,(format-bits (errors-score target-error))))
              (tr (th "Herbie") (td ,(format-bits (errors-score end-error)))))
             (div ([class "math"]) "\\[" ,(core->tex
                                            (program->fpcore
                                              (resugar-program (test-target test) repr)))
                                         "\\]"))
           "")

      (section ([id "history"])
       (h1 "Derivation")
       (ol ([class "history"])
        ,@(parameterize ([*output-repr* repr] [*var-reprs* (map (curryr cons repr) (test-vars test))])
            (render-history end-alt (mk-pcontext newpoints newexacts newpoints) (mk-pcontext points exacts points) repr))))

      ,(render-reproduction test)))
    out))
