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

(provide make-graph make-traceback make-timeout)

(define (make-axis pts #:axis idx #:out path #:regimes regimes)
  (call-with-output-file path #:exists 'replace
    (λ (out)
      (herbie-plot #:port out #:kind 'png
                   (error-axes pts #:axis idx)
                   (map error-mark regimes)))))

(define (make-plot err pts #:axis idx #:color theme #:out path)
  (call-with-output-file path #:exists 'replace
    (λ (out)
      (herbie-plot #:port out #:kind 'png
                   (error-points err pts #:axis idx #:color theme)
                   (error-avg err pts #:axis idx #:color theme)))))

(define (regime-var alt)
  (let loop ([alt alt])
    (match alt
      [(alt-event _ `(regimes ,splitpoints) prevs)
       (sp-bexpr (car splitpoints))]
      [(alt-event _ _ (list)) #f]
      [(alt-event _ _ (list prev _ ...)) (loop prev)]
      [(alt-delta _ _ prev) (loop prev)])))

(define (regime-splitpoints alt)
  (let loop ([alt alt])
    (match alt
      [(alt-event _ `(regimes ,splitpoints) prevs)
       (map sp-point (take splitpoints (sub1 (length splitpoints))))]
      [(alt-event _ _ (list)) #f]
      [(alt-event _ _ (list prev _ ...)) (loop prev)]
      [(alt-delta _ _ prev) (loop prev)])))

(define (render-process-info time timeline profile? test #:bug? [bug? #t])
  (printf "<section id='process-info'>\n")
  (printf "<h1>Runtime</h1>\n")
  (printf "<p class='header'>")
  (printf "Total time: <span class='number'>~a</span>\n" (format-time time))
  (printf "<a class='attachment' href='debug.txt'>Debug log</a>")
  (when profile?
    (printf "<a class='attachment' href='profile.txt'>Profile</a>"))
  (printf "</p>")
  (output-timeline timeline)
  (when bug?
    (printf "<p>Please <a href='https://github.com/uwplse/herbie/issues'>report a bug</a> with the following info:</p>\n"))
  (printf "<pre class='shell'><code>")
  (printf "herbie --seed '~a'" (get-seed))
  (for ([rec (changed-flags)])
    (match rec
      [(list 'enabled class flag) (printf " +o ~a:~a" class flag)]
      [(list 'disabled class flag) (printf " -o ~a:~a" class flag)]))
  (printf "\n")

  (printf "(FPCore ~a\n" (test-vars test))
  (printf "  :name ~s\n" (test-name test))
  (unless (equal? (test-precondition test) 'TRUE)
    (printf "  :pre ~a\n" (test-precondition test)))
  (unless (andmap (curry equal? 'default) (test-sampling-expr test))
    (printf "  :herbie-samplers ~a\n"
            (for/list ([var (test-vars test)] [samp (test-sampling-expr test)]
                       #:unless (equal? samp 'default))
              (list var samp))))
  (unless (equal? (test-expected test) #t)
    (printf "  :herbie-expected ~a\n" (test-expected test)))
  (when (test-output test) (printf "\n  :target\n  ~a\n\n" (test-output test)))
  (printf "  ~a)" (test-input test))
  (printf "</code></pre>\n")
  (printf "</section>\n"))

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
     (printf "<div>Average Error: <span class='number' title='Maximum error: ~a → ~a'>~a → ~a</span></div>\n"
             (format-bits (apply max (map ulps->bits start-error)) #:unit #f)
             (format-bits (apply max (map ulps->bits end-error)) #:unit #f)
             (format-bits (errors-score start-error) #:unit #f)
             (format-bits (errors-score end-error) #:unit #f))
     (printf "<div>Time: <span class='number'>~a</span></div>\n" (format-time time))
     (printf "<div>Precision: <span class='number'>~a</span></div>\n" (format-bits (*bit-width*) #:unit #f))
     (printf "<div>Ground Truth: <span class='number'>~a</span></div>\n" (format-bits bits #:unit #f))
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

         (define title "The X axis may use a short exponential scale")
         (make-axis newpoints #:axis idx #:out (build-path rdir (format "plot-~a.png" idx)) #:regimes (if split-var? (regime-splitpoints end-alt) '()))
         (make-plot start-error newpoints #:axis idx #:color *red-theme*
                    #:out (build-path rdir (format "plot-~ar.png" idx)))
         (when target-error
           (make-plot target-error newpoints #:axis idx #:color *green-theme*
                      #:out (build-path rdir (format "plot-~ag.png" idx))))
         (make-plot end-error newpoints #:axis idx #:color *blue-theme*
                    #:out (build-path rdir (format "plot-~ab.png" idx)))
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
       (output-history end-alt))
     (printf "</ol>\n")
     (printf "</section>\n")


     (render-process-info time timeline profile? test)


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

     ; Big bold numbers
     (printf "<h1>Error in ~a</h1>\n" (format-time time))

     (match exn
       [(exn:fail:user:herbie message _ url location)
        (printf "<section id='user-error'>\n")
        (printf "<h2>~a <a href='https://herbie.uwplse.org/~a/~a'>(more)</a></h2>\n" message *herbie-version* url)
        (printf "</section>")]
       [_
        (render-process-info time timeline profile? test #:bug? #t)

        (printf "<section id='backtrace'>\n")
        (printf "<h1>Backtrace</h1>\n")
        (printf "<table>\n")
        (printf "<thead>\n")
        (printf "<th colspan='2'>~a</th><th>L</th><th>C</th>\n" (html-escape-unsafe (exn-message exn)))
        (printf "</thead>\n")
        (for ([tb (continuation-mark-set->context (exn-continuation-marks exn))])
          (match (cdr tb)
            [(srcloc file line col _ _)
             (printf "<tr><td class='procedure'>~a</td><td>~a</td><td>~a</td><td>~a</td></tr>\n"
                     (procedure-name->string (car tb)) file line col)]
            [#f
             (printf "<tr><td class='procedure'>~a</td><td colspan='3'>unknown</td></tr>"
                     (procedure-name->string (car tb)))]))
        (printf "</table>\n")])
     (printf "<section>")

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
     
     (printf "<h1>Timeout in ~a</h1>\n" (format-time time))

     (render-process-info time timeline profile? test)

     (printf "</body>\n")
     (printf "</html>\n")]))

(struct interval (alt-idx start-point end-point expr))

(define (output-history altn)
  (define err (format-bits (errors-score (alt-errors altn))))
  (match altn
    [(alt-event prog 'start _)
     (printf "<li><p>Initial program <span class='error'>~a</span></p><div>\\[~a\\]</div></li>\n"
             err (texify-prog prog))]

    [(alt-event prog `(start ,strategy) `(,prev))
     (output-history prev)
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
             (output-history entry))
           (printf "</ol>\n")))
       (printf "</li>\n")
       (printf "<li class='event'>Recombined ~a regimes into one program.</li>\n"
               (length prevs)))]

    [(alt-event prog `(taylor ,pt ,loc) `(,prev))
     (output-history prev)
     (printf "<li><p>Taylor expanded around ~a <span class='error'>~a</span></p> <div>\\[\\leadsto ~a\\]</div></li>"
             pt err (texify-prog prog #:loc loc #:color "blue"))]

    [(alt-event prog 'periodicity `(,base ,subs ...))
     (output-history base)
     (for ([sub subs])
       (printf "<li class='event'>Optimizing periodic subexpression</li>\n")
       (output-history sub))
     (printf "<li class='event'>Combined periodic subexpressions</li>\n")]

    [(alt-event prog 'removed-pows `(,alt))
     (output-history alt)
     (printf "<li class='event'>Removed slow pow expressions</li>\n")]

    [(alt-event prog 'final-simplify `(,alt))
     (output-history alt)
     (printf "<li class='event'>Applied final simplification</li>\n")]

    [(alt-delta prog cng prev)
     (output-history prev)
     (printf "<li><p>Applied <span class='rule'>~a</span> <span class='error'>~a</span></p>"
             (rule-name (change-rule cng)) err)
     (printf "<div>\\[\\leadsto ~a\\]</div></li>\n"
             (texify-prog prog #:loc (change-location cng) #:color "blue"))]))

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


(define (procedure-name->string name)
  (if name
      (html-escape-unsafe (~a name))
      "(unnamed)"))
