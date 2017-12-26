#lang racket

(require "../common.rkt" "common.rkt")
(require "../points.rkt" "../float.rkt")
(require "../alternative.rkt" "../errors.rkt")
(require "../formats/test.rkt")
(require "../formats/datafile.rkt")
(require "../core/matcher.rkt")
(require "../core/regimes.rkt")
(require "../programs.rkt")
(require "../plot.rkt")
(require "../sandbox.rkt")
(require "../formats/tex.rkt")
(require (only-in xml write-xexpr xexpr?))

(provide make-graph make-traceback make-timeout make-axis-plot make-points-plot make-plots)

(define/contract (regime-var alt)
  (-> alternative? (or/c expr? #f))
  (let loop ([alt alt])
    (match alt
      [(alt-event _ `(regimes ,splitpoints) prevs)
       (sp-bexpr (car splitpoints))]
      [(alt-event _ _ (list)) #f]
      [(alt-event _ _ (list prev _ ...)) (loop prev)]
      [(alt-delta _ _ prev) (loop prev)])))

(define/contract (regime-splitpoints alt)
  (-> alternative? (listof number?))
  (let loop ([alt alt])
    (match alt
      [(alt-event _ `(regimes ,splitpoints) prevs)
       (map sp-point (take splitpoints (sub1 (length splitpoints))))]
      [(alt-event _ _ (list)) #f]
      [(alt-event _ _ (list prev _ ...)) (loop prev)]
      [(alt-delta _ _ prev) (loop prev)])))

(define/contract (render-command-line)
  (-> string?)
  (format
   "herbie shell --seed '~a' ~a"
   (get-seed)
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
     (if (equal? (test-precondition test) 'TRUE)
         #f
         (format "  :pre ~a" (test-precondition test)))
     (if (equal? (test-expected test) #t)
         #f
         (format "  :herbie-expected ~a" (test-expected test)))
     (if (test-output test)
         (format "\n  :herbie-target\n  ~a\n" (test-output test)) ; Extra newlines for clarity
         #f)
     (format "  ~a)" (test-input test))))
   "\n"))

(define timeline? any/c)

(define/contract (render-timeline timeline)
  (-> timeline? xexpr?)
  `(div ((class "timeline"))
        ,@(for/list ([curr timeline] [next (cdr timeline)])
            `(div
              ((class ,(format "timeline-phase ~a" (dict-ref curr 'type)))
               (data-timespan ,(~a (- (dict-ref next 'time) (dict-ref curr 'time))))
               ,@(for/list ([(type value) (in-dict curr)] #:when (not (member type '(time))))
                   `(,(string->symbol (format "data-~a" type)) ,(~a value))))))))


(define/contract (render-process-info time timeline profile? test #:bug? [bug? #f])
  (->* (number? timeline? boolean? test?) (#:bug? boolean?) xexpr?)
  `(section ((id "process-info"))
    (h1 "Runtime")
    (p ((class "header"))
     "Time bar (total: " (span ((class "number")) ,(format-time time)) ")"
     (a ((class "attachment") (href "debug.txt")) "Debug log")
     ,(if profile?
          `(a ((class "attachment") (href "profile.txt")) "Profile")
          "")
     ,(render-timeline timeline)
     ,(if bug?
          `(p "Please include this information when filing a "
              (a ((href "https://github.com/uwplse/herbie/issues")) "bug report") ":")
          "")
     (pre ((class "shell"))
      (code
       ,(render-command-line) "\n"
       ,(render-fpcore test) "\n")))))

(define (make-axis-plot result idx out)
  (define var (list-ref (test-vars (test-result-test result)) idx))
  (define split-var? (equal? var (regime-var (test-result-end-alt result))))
  (define pts (test-result-newpoints result))
  (herbie-plot
   #:port out #:kind 'png
   (error-axes pts #:axis idx)
   (map error-mark (if split-var? (regime-splitpoints (test-result-end-alt result)) '()))))

(define (make-points-plot result idx letter out)
  (define-values (theme accessor)
    (match letter
      ['r (values *red-theme*   test-result-start-error)]
      ['g (values *green-theme* test-result-target-error)]
      ['b (values *blue-theme*  test-result-end-error)]))

  (define pts (test-result-newpoints result))
  (define err (accessor result))

  (herbie-plot
   #:port out #:kind 'png
   (error-points err pts #:axis idx #:color theme)
   (error-avg err pts #:axis idx #:color theme)))

(define (make-plots result rdir profile?)
  (define (open-file #:type [type #f] idx fun . args)
    (call-with-output-file (build-path rdir (format "plot-~a~a.png" idx (or type ""))) #:exists 'replace
      (apply curry fun args)))

  (for ([var (test-vars (test-result-test result))] [idx (in-naturals)])
    (when (> (length (remove-duplicates (map (curryr list-ref idx) (test-result-newpoints result)))) 1)
      ;; This is bad code
      (open-file idx make-axis-plot result idx)
      (open-file idx #:type 'r make-points-plot result idx 'r)
      (when (test-result-target-error result)
        (open-file idx #:type 'g make-points-plot result idx 'g))
      (open-file idx #:type 'b make-points-plot result idx 'b))))

(define (make-graph result rdir profile?)
  (match result
    [(test-result test time bits start-alt end-alt points exacts
                  start-est-error end-est-error newpoints newexacts start-error end-error target-error timeline)
     (printf "<!doctype html>\n")
     (printf "<html>\n")
     (printf "<head>")
     (printf "<meta charset='utf-8' />")
     (printf "<title>Results for ~a</title>" (test-name test))
     (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
     (printf "<script src='~a'></script>" mathjax-url)
     (printf "<script src='../report.js'></script>")
     (printf "</head>\n")
     (printf "<body onload='graph()'>\n")


     ; Big bold numbers
     (printf "<section id='large'>\n")
     (printf "<div>Average Error: <span class='number' title='Maximum error: ~a → ~a'>~a → ~a</span></div>\n"
             (format-bits (apply max (map ulps->bits start-error)) #:unit #f)
             (format-bits (apply max (map ulps->bits end-error)) #:unit #f)
             (format-bits (errors-score start-error) #:unit #f)
             (format-bits (errors-score end-error) #:unit #f))
     (printf "<div>Time: <span class='number'>~a</span></div>\n" (format-time time))
     (printf "<div>Precision: <span class='number'>~a</span></div>\n" (format-bits (*bit-width*) #:unit #f))
     (printf "<div>Internal precision: <span class='number'>~a</span></div>\n" (format-bits bits #:unit #f))
     (printf "</section>\n")


     (printf "<section id='program'>\n")
     (printf "<div class='program'>\\[~a\\]</div>\n"
             (texify-prog (alt-program start-alt)))
     (printf "<div class='arrow'>⬇</div>")
     (printf "<div class='program'>\\[~a\\]</div>\n"
             (texify-prog (alt-program end-alt)))
     (printf "</section>\n")


     (printf "<section id='graphs'>\n")
     (printf "<h1>Error</h1>\n")
     (printf "<div>\n")
     (for ([var (test-vars test)] [idx (in-naturals)])
       (when (> (length (remove-duplicates (map (curryr list-ref idx) newpoints))) 1)
         (define split-var? (equal? var (regime-var end-alt)))

         (define title "The X axis uses an exponential scale")
         (printf "<figure id='fig-~a' ~a>" idx (if split-var? "class='default'" ""))
         (printf "<img width='800' height='300' src='plot-~a.png' title='~a'/>" idx title)
         (printf "<img width='800' height='300' src='plot-~ar.png' title='~a' data-name='Input'/>" idx title)
         (when target-error
           (printf "<img width='800' height='300' src='plot-~ag.png' title='~a' data-name='Target'/>" idx title))
         (printf "<img width='800' height='300' src='plot-~ab.png' title='~a' data-name='Result'/>" idx title)
         (printf "<figcaption><p>Bits error versus <var>~a</var></p></figcaption>" var)
         (printf "</figure>\n")))
     (printf "</div>\n")
     (printf "</section>\n")

     (when (test-output test)
       (printf "<section id='comparison'>\n")
       (printf "<h1>Target</h1>")
       (printf "<table>\n")
       (printf "<tr><th>Original</th><td>~a</td></tr>\n"
               (format-bits (errors-score start-error)))
       (printf "<tr><th>Comparison</th><td>~a</td></tr>\n"
               (format-bits (errors-score target-error)))
       (printf "<tr><th>Herbie</th><td>~a</td></tr>\n"
               (format-bits (errors-score end-error)))
       (printf "</table><!--\n")
       (printf "--><div>\\[ ~a \\]</div>\n"
               (texify-prog `(λ ,(test-vars test) ,(test-output test))))
       (printf "</section>\n"))


     (printf "<section id='history'>\n")
     (printf "<h1>Derivation</h1>\n")
     (printf "<ol class='history'>\n")
     (parameterize ([*pcontext* (mk-pcontext newpoints newexacts)]
                    [*start-prog* (alt-program start-alt)])
       (render-history end-alt))
     (printf "</ol>\n")
     (printf "</section>\n")


     (write-xexpr (render-process-info time timeline profile? test))


     (printf "</body>\n")
     (printf "</html>\n")]))

(define (make-traceback result rdir profile?)
  (match-define (test-failure test bits exn time timeline) result)
  (printf "<!doctype html>\n")
  (write-xexpr
   '(html
     (head
      (meta ((charset "utf-8")))
      (title "Exception for " ,(~a (test-name test)))
      (link ((rel "stylesheet") (type "text/css") (href "../graph.css"))))
     (body
      (h1 "Error in " ,(format-time time))
      (cond
       [(exn:fail:user:herbie? exn)
        `((section ([id "user-error"])
           (h2 ,(~a (exn-message exn)) (a ([href ,(herbie-error-url exn)]) "(more)"))
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
                         (td ,(or (~a (syntax-column stx)) (~a (syntax-position stx)))))))))))]
       [else
        `(,(render-process-info time timeline profile? test #:bug? #t)
          (section ([id "backtrace"])
           (h1 "Backtrace")
           (table
            (thead
             (th ([colspan "2"]) ,(exn-message exn)) (th "L") (th "C"))
            (tbody
             ,@(for ([tb (continuation-mark-set->context (exn-continuation-marks exn))])
                 (match (cdr tb)
                   [(srcloc file line col _ _)
                    `(tr
                      (td ([class "procedure"]) ,(procedure-name->string (car tb)))
                      (td ,(~a file))
                      (td ,(~a line))
                      (td ,(~a col)))]
                   [#f
                    `(tr
                      (td ([class "procedure"]) ,(procedure-name->string (car tb)))
                      (td ([colspan "3"]) "unknown"))]))))))])))))

(define (make-timeout result rdir profile?)
  (match-define (test-timeout test bits time timeline) result)
  (printf "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (meta ((charset "utf-8")))
      (title ,(format "Timeout for ~a" (test-name test)))
      (link ([rel "stylesheet"] [type "text/css"] [href "../graph.css"])))
     (body
      (h1 "Timeout in " (format-time time))
      (p "Use the " (code "--timeout") " flag to change the timeout.")
      ,(render-process-info time timeline profile? test)))))

(struct interval (alt-idx start-point end-point expr))

(define (render-history altn)
  (define err (format-bits (errors-score (alt-errors altn))))
  (match altn
    [(alt-event prog 'start _)
     (printf "<li><p>Initial program <span class='error'>~a</span></p><div>\\[~a\\]</div></li>\n"
             err (texify-prog prog))]

    [(alt-event prog `(start ,strategy) `(,prev))
     (render-history prev)
     (printf "<li class='event'>Using strategy <code>~a</code></li>\n"
             strategy)]

    [(alt-event _ `(regimes ,splitpoints) prevs)
     (let* ([start-sps (cons (sp -1 -1 -inf.0) (take splitpoints (sub1 (length splitpoints))))]
            [vars (program-variables (alt-program altn))]
            [intervals
             (for/list ([start-sp start-sps] [end-sp splitpoints])
               (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp)))]
            [preds (splitpoints->point-preds splitpoints (length prevs))]
            [interval->string
             (λ (ival)
               (string-join
                (list
                 (if (ordinary-float? (interval-start-point ival))
                     (format "~a &lt; " (interval-start-point ival))
                     "")
                 (~a (interval-expr ival))
                 (if (ordinary-float? (interval-end-point ival))
                     (format " &lt; ~a" (interval-end-point ival))
                     ""))))])
       (printf "<li class='event'>Split input into ~a regimes.</li>\n" (length prevs))
       (printf "<li>\n")
       (for ([entry prevs] [entry-idx (range (length prevs))] [pred preds])
         (let* ([entry-ivals
                 (filter (λ (intrvl) (= (interval-alt-idx intrvl) entry-idx)) intervals)]
                [condition
                 (string-join (map interval->string entry-ivals) " or ")])
           (define-values (ivalpoints ivalexacts)
             (for/lists (pts exs) ([(pt ex) (in-pcontext (*pcontext*))] #:when (pred pt))
               (values pt ex)))
           (printf "<h2><code>if <span class='condition'>~a</span></code></h2>\n" condition)
           (printf "<ol>\n")
           ;; TODO: The (if) here just corrects for the possibility
           ;; that we might have sampled new points that include no
           ;; points in a given regime. Instead it would be best to
           ;; continue sampling until we actually have many points in
           ;; each regime. That would require breaking some
           ;; abstraction boundaries right now so we haven't dont it
           ;; yet.
           (define new-pcontext
             (if (null? ivalpoints) (*pcontext*) (mk-pcontext ivalpoints ivalexacts)))
           (parameterize ([*pcontext* new-pcontext])
             (render-history entry))
           (printf "</ol>\n")))
       (printf "</li>\n")
       (printf "<li class='event'>Recombined ~a regimes into one program.</li>\n"
               (length prevs)))]

    [(alt-event prog `(taylor ,pt ,loc) `(,prev))
     (render-history prev)
     (printf "<li><p>Taylor expanded around ~a <span class='error'>~a</span></p> <div>\\[\\leadsto ~a\\]</div></li>"
             pt err (texify-prog prog #:loc loc #:color "blue"))]

    [(alt-event prog 'periodicity `(,base ,subs ...))
     (render-history base)
     (for ([sub subs])
       (printf "<li class='event'>Optimizing periodic subexpression</li>\n")
       (render-history sub))
     (printf "<li class='event'>Combined periodic subexpressions</li>\n")]

    [(alt-event prog 'removed-pows `(,alt))
     (render-history alt)
     (printf "<li class='event'>Removed slow pow expressions</li>\n")]

    [(alt-event prog 'final-simplify `(,alt))
     (render-history alt)
     (printf "<li class='event'>Applied final simplification</li>\n")]

    [(alt-delta prog cng prev)
     (render-history prev)
     (printf "<li><p>Applied <span class='rule'>~a</span> <span class='error'>~a</span></p>"
             (rule-name (change-rule cng)) err)
     (printf "<div>\\[\\leadsto ~a\\]</div></li>\n"
             (texify-prog prog #:loc (change-location cng) #:color "blue"))]))

(define (procedure-name->string name)
  (if name
      (html-escape-unsafe (~a name))
      "(unnamed)"))
