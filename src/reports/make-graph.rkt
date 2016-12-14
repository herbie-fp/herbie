#lang racket

(require "../common.rkt" "common.rkt")
(require "../points.rkt" "../float.rkt")
(require "../alternative.rkt")
(require "../formats/test.rkt")
(require "../formats/datafile.rkt")
(require "../core/matcher.rkt")
(require "../core/regimes.rkt")
(require "../programs.rkt")
(require "../plot.rkt")
(require "../sandbox.rkt")
(require "../formats/tex.rkt")

(provide make-graph make-traceback make-timeout)

(define (make-axis pts #:axis idx #:out path)
  (call-with-output-file path #:exists 'replace
    (λ (out) (herbie-plot #:port out #:kind 'png (error-axes pts #:axis idx)))))

(define (make-plot err pts #:axis idx #:color theme #:out path)
  (call-with-output-file path #:exists 'replace
    (λ (out)
      (herbie-plot #:port out #:kind 'png
                   (error-points err pts #:axis idx #:color theme)
                   (error-avg err pts #:axis idx #:color theme)))))

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
     (printf "<script src='~a'></script>" ; MathJax URL for prettifying programs
             "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML")
     (printf "<script src='../report.js'></script>")
     (printf "</head>\n")
     (printf "<body onload='graph()'>\n")


     ; Big bold numbers
     (printf "<section id='large'>\n")
     (printf "<div>Time: <span class='number'>~a</span></div>\n" (format-time time))
     (printf "<div>Input Error: <span class='number'>~a</span></div>\n"
             (format-bits (errors-score start-error)))
     (printf "<div>Output Error: <span class='number'>~a</span></div>\n"
             (format-bits (errors-score end-error)))
     (printf "<div>Precision: <span class='number'>~a</span></div>\n" (format-bits (*bit-width*)))
     (printf "<div>Ground Truth: <span class='number'>~a</span></div>\n" (format-bits bits))
     (printf "</section>\n")


     (printf "<section id='program'>\n")
     (printf "<div class='program'>\\[~a\\]</div>\n"
             (texify-prog (alt-program start-alt)))
     (printf "<div class='arrow'>⬇</div>")
     (printf "<div class='program'>\\[~a\\]</div>\n"
             (texify-prog (alt-program end-alt)))
     (printf "</section>\n")


     (printf "<section id='graphs'>\n")
     (for ([var (test-vars test)] [idx (in-naturals)])
       (when (> (length (remove-duplicates (map (curryr list-ref idx) newpoints))) 1)
         (make-axis newpoints #:axis idx #:out (build-path rdir (format "plot-~a.png" idx)))
         (make-plot start-error newpoints #:axis idx #:color *red-theme*
                    #:out (build-path rdir (format "plot-~ar.png" idx)))
         (when target-error
           (make-plot target-error newpoints #:axis idx #:color *green-theme*
                      #:out (build-path rdir (format "plot-~ag.png" idx))))
         (make-plot end-error newpoints #:axis idx #:color *blue-theme*
                    #:out (build-path rdir (format "plot-~ab.png" idx)))
         (printf "<figure>")
         (printf "<img width='400' height='200' src='plot-~a.png'/>" idx)
         (printf "<img width='400' height='200' src='plot-~ar.png' data-name='Input'/>" idx)
         (when target-error
           (printf "<img width='400' height='200' src='plot-~ag.png' data-name='Target'/>" idx))
         (printf "<img width='400' height='200' src='plot-~ab.png' data-name='Result'/>" idx)
         (printf "<figcaption>Bits error versus <var>~a</var></figcaption>" var)
         (printf "</figure>\n")))
     (printf "</section>\n")


     (printf "<ol class='history'>\n")
     (parameterize ([*pcontext* (mk-pcontext newpoints newexacts)]
                    [*start-prog* (alt-program start-alt)])
       (output-history end-alt))
     (printf "</ol>\n")


     (printf "<section id='process-info'>\n")
     (printf "<div id='runtime'>\n")
     (printf "Total time: <span class='number'>~a</span>\n" (format-time time))
     (printf "<a class='attachment' href='debug.txt'>Debug log</a>")
     (when profile?
       (printf "<a class='attachment' href='profile.txt'>Profile</a></div>"))
     (output-timeline timeline)
     (printf "<div id='reproduce'>\n")
     (printf "<pre><code>")
     (printf "herbie --seed '~a'\n" (get-seed))
     (printf "(FPCore ~a\n  :name ~s\n  ~a~a)"
          (test-vars test) (test-name test)
          (if (test-output test) (format "\n  :target\n  ~a" (test-output test)) "") (test-input test))
     (printf "</code></pre>\n")
     (printf "</div>\n")
     (printf "</section>\n")


     (printf "</body>\n")
     (printf "</html>\n")]))

(define (make-traceback result rdir profile?)
  (match result
    [(test-failure test bits exn time timeline)
     (printf "<!doctype html>\n")
     (printf "<html>\n")
     (printf "<head>\n")
     (printf "<meta charset='utf-8' />\n")
     (printf "<title>Exception for ~a</title>" (test-name test))
     (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
     (printf "</head>")
     (printf "<body>\n")

     (printf "<dl id='about'>\n")
     (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
     (printf "<dt>Logs:</dt>")
     (printf "<dd><a href='debug.txt'>Debug output</a>")
     (when profile?
       (printf ", <a href='profile.txt'>Profiling report</a>"))
     (printf "</dd>\n")
     (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
     (printf "</dl>\n")

     (printf "<h2 id='error-message'>~a</h2>\n" (html-escape-unsafe (exn-message exn)))
     (printf "<ol id='traceback'>\n")
     (for ([tb (continuation-mark-set->context (exn-continuation-marks exn))])
       (printf "<li><code>~a</code> in <code>~a</code></li>\n"
               (html-escape-unsafe (~a (car tb))) (srcloc->string (cdr tb))))
     (printf "</ol>\n")

     (output-timeline timeline)
     
     (printf "<p>Please <a href='https://github.com/uwplse/herbie/issues'>report this bug</a>!</p>\n")

     (printf "</body>\n")
     (printf "</html>\n")]))

(define (make-timeout result rdir profile?)
  (match result
    [(test-timeout test bits time timeline)
     (printf "<!doctype html>\n")
     (printf "<html>\n")
     (printf "<head>\n")
     (printf "<meta charset='utf-8' />\n")
     (printf "<title>Timeout for ~a</title>" (test-name test))
     (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
     (printf "</head>")
     (printf "<body>\n")

     (printf "<dl id='about'>\n")
     (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
     (printf "<dt>Logs:</dt>")
     (printf "<dd><a href='debug.txt'>Debug output</a>")
     (when profile?
       (printf ", <a href='profile.txt'>Profiling report</a>"))
     (printf "</dd>\n")
     (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
     (printf "</dl>\n")

     (printf "<h2>Test timed out</h2>\n")

     (output-timeline timeline)

     (printf "</body>\n")
     (printf "</html>\n")]))

(struct interval (alt-idx start-point end-point expr))

(define (output-history altn)
  (define err (format-bits (errors-score (alt-errors altn))))
  (match altn
    [(alt-event prog 'start _)
     (printf "<li>Started with <div>\\[~a\\]</div> <div class='error'>~a</div></li>\n"
             (texify-prog prog) err)]

    [(alt-event prog `(start ,strategy) `(,prev))
     (output-history prev)
     (printf "<li class='event'>Using strategy <code>~a</code> <div class='error'>~a</div></li>\n"
             strategy err)]

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
             (output-history entry))
           (printf "</ol>\n"))))]

    [(alt-event prog `(taylor ,pt ,loc) `(,prev))
     (output-history prev)
     (printf "<li>Taylor expanded around ~a to get <div>\\[~a \\leadsto ~a\\]</div> <div class='error'>~a</div></li>"
             pt
             (texify-prog (alt-program prev) #:loc loc #:color "red")
             (texify-prog prog               #:loc loc #:color "blue")
             err)]

    [(alt-event prog 'periodicity `(,base ,subs ...))
     (output-history base)
     (for ([sub subs])
       (printf "<hr/><li class='event'>Optimizing periodic subexpression</li>\n")
       (output-history sub))
     (printf "<hr/><li class='event'>Combined periodic subexpressions</li>\n")]

    [(alt-event prog 'removed-pows `(,alt))
     (output-history alt)
     (printf "<hr/><li class='event'>Removed slow pow expressions</li>\n")]

    [(alt-event prog 'final-simplify `(,alt))
     (output-history alt)
     (printf "<hr/><li class='event'>Applied final simplification</li>\n")]

    [(alt-delta prog cng prev)
     (output-history prev)
     (printf "<li>Applied <span class='rule'>~a</span> "
             (rule-name (change-rule cng)))
     (printf "to get <div>\\[~a \\leadsto ~a\\]</div> <div class='error'>~a</div></li>\n"
             (texify-prog (alt-program prev) #:loc (change-location cng) #:color "red")
             (texify-prog prog               #:loc (change-location cng) #:color "blue")
             err)]))

(define (output-timeline timeline)
  (printf "<div class='timeline'>")
  (for ([curr timeline] [next (cdr timeline)])
    (printf "<div class='timeline-phase ~a' data-timespan='~a'"
            (cdr (assoc 'type curr))
            (- (cdr (assoc 'time next)) (cdr (assoc 'time curr))))
    (for ([(type value) (in-dict curr)] #:when (not (member type '(time))))
      (printf " data-~a='~a'" type value))
    (printf "></div>"))
  (printf "</div>\n"))

(define (srcloc->string sl)
  (if sl
      (string-append
       (path->string (srcloc-source sl))
       ":"
       (number->string (srcloc-line sl))
       ":"
       (number->string (srcloc-column sl)))
      "???"))
