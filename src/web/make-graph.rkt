#lang racket

(require (only-in xml write-xexpr xexpr?) (only-in fpbench core->js fpcore?))
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../alternative.rkt" "../errors.rkt" "../interface.rkt"
         "../formats/test.rkt" "../syntax/rules.rkt" "../core/matcher.rkt"
         "../core/regimes.rkt" "../sandbox.rkt" "../fpcore/core2js.rkt"
         "common.rkt" "timeline.rkt" "tex.rkt" "plot.rkt")

(provide all-pages make-page page-error-handler)

(define (unique-values pts idx)
  (length (remove-duplicates (map (curryr list-ref idx) pts))))

(define (all-pages result)
  (define test (test-result-test result))
  (define good? (test-success? result))

  (define pages
    `("graph.html"
      ,(and good? "interactive.js")
      "timeline.html" "timeline.json"
      ,@(for/list ([v (test-vars test)] [idx (in-naturals)]
                   #:when good? [type '("" "r" "g" "b")]
                   #:unless (and (equal? type "g") (not (test-output test)))
                   ;; Don't generate a plot with only one X value else plotting throws an exception
                   #:when (> (unique-values (test-success-newpoints result) idx) 1))
          (format "plot-~a~a.png" idx type))))
  (filter identity pages))

(define ((page-error-handler result page) e)
  (define test (test-result-test result))
  ((error-display-handler)
   (format "Error generating `~a` for \"~a\":\n~a\n" page (test-name test) (exn-message e))
   e))

(define (make-page page out result profile?)
  (define test (test-result-test result))
  (define precision (test-output-prec test))
  (match page
    ["graph.html"
     (match result
       [(? test-success?) (make-graph result out (get-interactive-js result) profile?)]
       [(? test-timeout?) (make-timeout result out profile?)]
       [(? test-failure?) (make-traceback result out profile?)])]
    ["interactive.js"
     (make-interactive-js result out)]
    ["timeline.html"
     (make-timeline result out)]
    ["timeline.json"
     (make-timeline-json result out precision)]
    [(regexp #rx"^plot-([0-9]+).png$" (list _ idx))
     (make-axis-plot result out (string->number idx))]
    [(regexp #rx"^plot-([0-9]+)([rbg]).png$" (list _ idx letter))
     (make-points-plot result out (string->number idx) (string->symbol letter))]))

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

(define/contract (render-command-line)
  (-> string?)
  (format
   "herbie shell --seed ~a ~a"
   (if (vector? (get-seed)) (format "'~a'" (get-seed)) (get-seed))
   (string-join
    (for/list ([rec (changed-flags)])
      (match rec
        [(list 'enabled class flag) (format "+o ~a:~a" class flag)]
        [(list 'disabled class flag) (format "-o ~a:~a" class flag)]))
    " ")))

(define/contract (render-fpcore test)
  (-> test? string?)
  (string-join
   (filter
    identity
    (list
     (format "(FPCore ~a" (test-vars test))
     (format "  :name ~s" (test-name test))
     (format "  :precision ~s" (test-output-prec test))
     (if (equal? (test-precondition test) 'TRUE)
         #f
         (format "  :pre ~a" (resugar-program (test-precondition test)
                                              (test-output-prec test))))
     (if (equal? (test-expected test) #t)
         #f
         (format "  :herbie-expected ~a" (test-expected test)))
     (if (test-output test)
         ;; Extra newlines for clarity
         (format "\n  :herbie-target\n  ~a\n" (resugar-program (test-output test)
                                                               (test-output-prec test)))
         #f)
     (format "  ~a)" (resugar-program (test-input test) (test-output-prec test)))))
   "\n"))

(define/contract (render-reproduction test #:bug? [bug? #f])
  (->* (test?) (#:bug? boolean?) xexpr?)

  `(section ((id "reproduce"))
    (h1 "Reproduce")
    ,(if bug?
         `(p "Please include this information when filing a "
             (a ((href "https://github.com/uwplse/herbie/issues")) "bug report") ":")
         "")
    (pre ((class "shell"))
         (code
          ,(render-command-line) "\n"
          ,(render-fpcore test) "\n"))))

(define (get-interactive-js result)
  (define start-fpcore (program->fpcore (alt-program (test-success-start-alt result))))
  (define end-fpcore (program->fpcore (alt-program (test-success-end-alt result))))
  (and (fpcore? start-fpcore) (fpcore? end-fpcore)
       (string-append
        (core->js start-fpcore "start")
        (core->js end-fpcore "end"))))

(define (make-interactive-js result out)
  (define js-text (get-interactive-js result))
  (when (string? js-text)
    (display js-text out)))

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

(define (make-graph result out valid-js-prog profile?)
  (match-define
   (test-success test bits time timeline warnings
                 start-alt end-alt points exacts start-est-error end-est-error
                 newpoints newexacts start-error end-error target-error
                 baseline-error oracle-error all-alts)
   result)
   (define precision (test-output-prec test))
   (define repr (get-representation precision))
   ;; render-history expects the precision to be 'real rather than 'binary64 or 'binary32
   ;; remove this when the number system interface is added

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
          (and valid-js-prog (for/and ([p points]) (andmap number? p))
               '("Try it out!" . "#try-it"))
          (and (test-output test)
               '("Target" . "#comparison"))
          '("Derivation" . "#history")
          '("Reproduce" . "#reproduce"))
         (list/true
          '("Report" . "../results.html")
          '("Log" . "debug.txt")
          (and profile? '("Profile" . "profile.txt"))
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
        ,(render-large "Precision" (format-bits (*bit-width*) #:unit #f)))

       ,(render-warnings warnings)

       ,(render-program test #:to (alt-program end-alt))

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

       ,(if (and valid-js-prog (for/and ([p points]) (andmap number? p)))
            (render-interactive start-alt (car points))
            "")

       ,(if (test-output test)
            `(section ([id "comparison"])
              (h1 "Target")
              (table
               (tr (th "Original") (td ,(format-bits (errors-score start-error))))
               (tr (th "Target") (td ,(format-bits (errors-score target-error))))
               (tr (th "Herbie") (td ,(format-bits (errors-score end-error)))))
              (div ([class "math"]) "\\[" ,(texify-prog `(λ ,(test-vars test)
                                                            ,(test-output test))
                                                        precision) "\\]"))
            "")

       (section ([id "history"])
        (h1 "Derivation")
        (ol ([class "history"])
         ,@(parameterize ([*output-repr* repr] [*var-reprs* (map (curryr cons repr) (test-vars test))])
             (render-history end-alt (mk-pcontext newpoints newexacts) (mk-pcontext points exacts) precision))))

       ,(render-reproduction test)))
    out))

(define (make-traceback result out profile?)
  (match-define (test-failure test bits time timeline warnings exn) result)
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (meta ((charset "utf-8")))
      (title "Exception for " ,(~a (test-name test)))
      (link ((rel "stylesheet") (type "text/css") (href "../report.css")))
      ,@js-tex-include
      (script ([src "../report.js"])))
     (body
      ,(render-menu
        (list/true)
        (list/true
         '("Report" . "../results.html")
         '("Log" . "debug.txt")
         (and profile? '("Profile" . "profile.txt"))
         '("Metrics" . "timeline.html")))

      ,(render-warnings warnings)

      ,(if (exn:fail:user:herbie? exn)
           `(section ([id "user-error"])
             (h2 ,(~a (exn-message exn)) (a ([href ,(herbie-error-url exn)]) " (more)"))
             ,(if (exn:fail:user:herbie:syntax? exn)
                  `(table
                    (thead
                     (th ([colspan "2"]) ,(exn-message exn)) (th "L") (th "C"))
                    (tbody
                     ,@(for/list ([(stx msg) (in-dict (exn:fail:user:herbie:syntax-locations exn))])
                         `(tr
                           (td ([class "procedure"]) ,(~a msg))
                           (td ,(~a (syntax-source stx)))
                           (td ,(or (~a (syntax-line stx) "")))
                           (td ,(or (~a (syntax-column stx)) (~a (syntax-position stx))))))))
                  ""))
           "")

      ,(render-program test)

      ,(if (not (exn:fail:user:herbie? exn))
           `(,@(render-reproduction test #:bug? #t)
             (section ([id "backtrace"])
              (h1 "Backtrace")
              ,(render-traceback exn)))
           "")))
   out))

(define (render-traceback exn)
  `(table
    (thead
     (th ([colspan "2"]) ,(exn-message exn)) (th "L") (th "C"))
    (tbody
     ,@(for/list ([tb (continuation-mark-set->context (exn-continuation-marks exn))])
         (match (cdr tb)
           [(srcloc file line col _ _)
            `(tr
              (td ([class "procedure"]) ,(~a (or (car tb) "(unnamed)")))
              (td ,(~a file))
              (td ,(~a line))
              (td ,(~a col)))]
           [#f
            `(tr
              (td ([class "procedure"]) ,(~a (or (car tb) "(unnamed)")))
              (td ([colspan "3"]) "unknown"))])))))

(define (make-timeout result out profile?)
  (match-define (test-timeout test bits time timeline warnings) result)
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (meta ((charset "utf-8")))
      (title ,(format "Timeout for ~a" (test-name test)))
      (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
      ,@js-tex-include
      (script ([src "../report.js"])))
     (body
      ,(render-menu
        (list/true)
        (list/true
         '("Report" . "../results.html")
         '("Log" . "debug.txt")
         (and profile? '("Profile" . "profile.txt"))
         '("Metrics" . "timeline.html")))
      ,(render-warnings warnings)

      (h1 "Timeout in " ,(format-time time))
      (p "Use the " (code "--timeout") " flag to change the timeout.")

      ,(render-program test)

      ,(render-reproduction test)))
   out))

(struct interval (alt-idx start-point end-point expr))

(define (interval->string ival repr)
  (define start (interval-start-point ival))
  (define end (interval-end-point ival))
  (string-join
   (list
    (if start
        (format "~a < " (repr->fl start repr))
        "")
    (~a (interval-expr ival))
    (if (equal? end +nan.0)
        ""
        (format " < ~a" (repr->fl end repr))))))

(define (split-pcontext pcontext splitpoints alts precision)
  (define repr (get-representation precision))
  (define preds (splitpoints->point-preds splitpoints alts repr))

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

(define (render-history altn pcontext pcontext2 precision)
  (define repr (get-representation precision))
  (define err
    (format-bits (errors-score (errors (alt-program altn) pcontext repr))))
  (define err2
    (format "Internally ~a" (format-bits (errors-score (errors (alt-program altn) pcontext2 repr)))))

  (match altn
    [(alt prog 'start (list))
     (list
      `(li (p "Initial program " (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[" ,(texify-prog prog precision) "\\]")))]
    [(alt prog `(start ,strategy) `(,prev))
     `(,@(render-history prev pcontext pcontext2 precision)
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
                      [new-pcontext (split-pcontext pcontext splitpoints
                                                    prevs precision)]
                      [new-pcontext2 (split-pcontext pcontext splitpoints
                                                     prevs precision)])
             (define entry-ivals (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx)) intervals))
             (define condition (string-join (map (curryr interval->string repr) entry-ivals) " or "))
             `((h2 (code "if " (span ([class "condition"]) ,condition)))
               (ol ,@(render-history entry new-pcontext new-pcontext2 precision))))))
       (li ([class "event"]) "Recombined " ,(~a (length prevs)) " regimes into one program."))]

    [(alt prog `(taylor ,pt ,loc) `(,prev))
     `(,@(render-history prev pcontext pcontext2 precision)
       (li (p "Taylor expanded around " ,(~a pt) " " (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog precision #:loc loc
                                                               #:color "blue") "\\]")))]

    [(alt prog `(simplify ,loc) `(,prev))
     `(,@(render-history prev pcontext pcontext2 precision)
       (li (p "Simplified" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog precision #:loc loc
                                                               #:color "blue") "\\]")))]

    [(alt prog `initial-simplify `(,prev))
     `(,@(render-history prev pcontext pcontext2 precision)
       (li (p "Initial simplification" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog precision) "\\]")))]

    [(alt prog `final-simplify `(,prev))
     `(,@(render-history prev pcontext pcontext2 precision)
       (li (p "Final simplification" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog precision) "\\]")))]

    [(alt prog (list 'change cng) `(,prev))
     `(,@(render-history prev pcontext pcontext2 precision)
       (li (p "Applied " (span ([class "rule"]) ,(~a (rule-name (change-rule cng))))
              (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog precision #:loc (change-location cng) #:color "blue") "\\]")))]
    ))
