#lang racket

(require "datafile.rkt")
(require "../common.rkt")
(require "../points.rkt")
(require "../matcher.rkt")
(require "../alternative.rkt")
(require "../test.rkt")
(require "../infer-regimes.rkt")
(require "../programs.rkt")
(require "../plot.rkt")
(require "../sandbox.rkt")
(require "../compile/tex.rkt")

(provide make-graph make-traceback make-timeout)

;; TODO: Move to a common file
(define (format-time ms)
  (cond
   [(< ms 1000) (format "~a ms" (round ms))]
   [(< ms 60000) (format "~a s" (/ (round (/ ms 100.0)) 10))]
   [(< ms 3600000) (format "~a m" (/ (round (/ ms 6000.0)) 10))]
   [else (format "~a hr" (/ (round (/ ms 360000.0)) 10))]))

(define (display-bits r #:sign [sign #f])
  (cond
   [(not r) ""]
   [(and (r . > . 0) sign) (format "+~a" (/ (round (* r 10)) 10))]
   [else (format "~a" (/ (round (* r 10)) 10))]))

(define (make-axis pts #:axis idx #:out path)
  (call-with-output-file path #:exists 'replace
    (λ (out) (herbie-plot #:port out #:kind 'png (error-axes pts #:axis idx)))))

(define (make-plot err pts #:axis idx #:color theme #:out path)
  (call-with-output-file path #:exists 'replace
    (λ (out)
      (herbie-plot #:port out #:kind 'png
                   (error-points err pts #:axis idx #:color theme)
                   (error-avg err pts #:axis idx #:color theme)))))

(define (print-test t)
  (printf "(lambda ~a\n  #:name ~s\n  ~a~a)\n\n"
          (for/list ([v (test-vars t)]
                     [s (test-sampling-expr t)])
                    (list v s))
          (test-name t)
          (test-input t)
          (if (test-output t)
              (format "\n  #:target\n  ~a" (test-output t))
              "")))

(define (make-graph result profile?)
  (match result
    [(test-result test rdir time bits start-alt end-alt points exacts
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

     (printf "<section id='about'>\n")

     (printf "<div>\\[~a\\]</div>\n"
             (texify-expression (program-body (alt-program start-alt))))

     (printf "<dl id='kv'>\n")
     (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
     (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
     (printf "</dl>\n")

     (printf "<div id='graphs'>\n")
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
     (printf "</div>\n")

     (printf "</section>\n")

     (printf "<section id='details'>\n")

     ; Big bold numbers
     (printf "<div id='large'>\n")
     (printf "<div>Time: <span class='number'>~a</span></div>\n"
             (format-time time))
     (printf "<div>Input Error: <span class='number'>~a</span></div>\n"
             (display-bits (errors-score start-error)))
     (printf "<div>Output Error: <span class='number'>~a</span></div>\n"
             (display-bits (errors-score end-error)))
     ; TODO : Make icons
     (printf "<div>Log: <a href='debug.txt' class='icon'><span style='display: block; transform: rotate(-45deg);'>&#x26b2;</span></a></div>")
     (when profile?
       (printf "<div>Profile: <a href='profile.txt' class='icon'>&#x1F552;</a></div>"))
     (printf "</div>\n")

     (printf "<div id='output'>\\(~a\\)</div>\n"
             (texify-expression (program-body (alt-program end-alt))))
     
     (output-timeline timeline)

     (printf "<ol id='process-info'>\n")
     (parameterize ([*pcontext* (mk-pcontext newpoints newexacts)]
                    [*start-prog* (alt-program start-alt)])
       (output-history end-alt))
     (printf "</ol>\n")

     (printf "</section>\n")

     (printf "<div style='clear:both;'>\n")
     (printf "<p>Original test:</p>\n")
     (printf "<pre><code>\n")
     (print-test test)
     (printf "</code></pre>\n")
     (printf "</div>\n")

     (printf "</body>\n")
     (printf "</html>\n")]))

(define (make-traceback result profile?)
  (match result
    [(test-failure test bits exn time rdir timeline)
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

(define (make-timeout result profile?)
  (match result
    [(test-timeout test bits time rdir timeline)
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
  (define err (display-bits (errors-score (alt-errors altn))))
  (match altn
    [(alt-event prog 'start _)
     (printf "<li>Started with <div>\\[~a\\]</div> <div class='error'>~a</div></li>\n"
             (texify-expression (program-body prog)) err)]

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
             pt (texify-expression (program-body (alt-program prev)) #:loc loc #:color "red")
             (texify-expression (program-body prog) #:loc loc #:color "blue") err)]

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
             (texify-expression (program-body (alt-program prev)) #:loc (change-location cng) #:color "red")
             (texify-expression (program-body prog) #:loc (change-location cng) #:color "blue")
             err)]))

(define (output-timeline timeline)
  (printf "<div class='timeline'>")
  (for ([curr timeline] [next (cdr timeline)])
    (printf "<div class='timeline-phase ~a' data-timespan='~a'"
            (cdr (assoc 'type curr))
            (- (cdr (assoc 'time next)) (cdr (assoc 'time curr))))
    (for ([(type value) (in-pairs curr)] #:when (not (member type '(time))))
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
