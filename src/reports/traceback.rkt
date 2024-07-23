#lang racket

(require (only-in xml write-xexpr xexpr?))
(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../syntax/read.rkt"
         "../api/sandbox.rkt"
         "common.rkt")
(provide make-traceback)

(define (make-traceback result out profile?)
  (eprintf "make-traceback\n")
  ;; Called with timeout or failure results
  (match-define (job-result command test status time timeline warnings backend) result)
  (define exn (if (eq? status 'failure) backend 'timeout))

  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html (head (meta ((charset "utf-8")))
                (title "Exception for " ,(~a (test-name test)))
                (link ((rel "stylesheet") (type "text/css") (href "../report.css")))
                ,@js-tex-include
                (script ([src "../report.js"])))
          (body ,(render-menu (~a (test-name test))
                              (list '("Report" . "../index.html") '("Metrics" . "timeline.html")))
                ,(render-warnings warnings)
                ,(let-values ([(dropdown body) (render-program (test-spec test)
                                                               (test-context test)
                                                               #:pre (test-pre test)
                                                               #:ident (test-identifier test))])
                   `(section (details ([id "specification"] (class "programs"))
                                      (summary (h2 "Specification")
                                               ,dropdown
                                               (a ((class "help-button float")
                                                   [href ,(doc-url "report.html#spec")]
                                                   [target "_blank"])
                                                  "?"))
                                      ,body)))
                ,(match exn
                   [(? exn:fail:user:herbie?)
                    `(section
                      ([id "user-error"] (class "error"))
                      (h2 ,(~a (exn-message exn)) " " (a ([href ,(herbie-error-url exn)]) "(more)"))
                      ,(if (exn:fail:user:herbie:syntax? exn) (render-syntax-errors exn) ""))]
                   ['timeout
                    `(section ([id "user-error"] (class "error"))
                              (h2 "Timeout after " ,(format-time time))
                              (p "Use the " (code "--timeout") " flag to change the timeout."))]
                   [_ ""])
                ,(match exn
                   [(? exn:fail:user:herbie?) ""]
                   [(? exn?)
                    `(,@(render-reproduction test #:bug? #t)
                      (section ([id "backtrace"]) (h2 "Backtrace") ,(render-traceback exn)))]
                   [_ ""])))
   out))

(define (render-syntax-errors exn)
  `(table (thead (th ([colspan "2"]) ,(exn-message exn)) (th "L") (th "C"))
          (tbody ,@(for/list ([(stx msg) (in-dict (exn:fail:user:herbie:syntax-locations exn))])
                     `(tr (td ((class "procedure")) ,(~a msg))
                          (td ,(~a (syntax-source stx)))
                          (td ,(or (~a (syntax-line stx) "")))
                          (td ,(or (~a (syntax-column stx)) (~a (syntax-position stx)))))))))

(define (render-traceback exn)
  `(table (thead (th ([colspan "2"]) ,(exn-message exn)) (th "L") (th "C"))
          (tbody ,@(for/list ([tb (continuation-mark-set->context (exn-continuation-marks exn))])
                     (match (cdr tb)
                       [(srcloc file line col _ _)
                        `(tr (td ((class "procedure")) ,(~a (or (car tb) "(unnamed)")))
                             (td ,(~a file))
                             (td ,(~a line))
                             (td ,(~a col)))]
                       [#f
                        `(tr (td ((class "procedure")) ,(~a (or (car tb) "(unnamed)")))
                             (td ([colspan "3"]) "unknown"))])))))
