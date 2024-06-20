; #lang racket

; (require json)
; (require racket/exn)
; (require openssl/sha1 (rename-in xml [location? xml-location?]))
; (require web-server/servlet web-server/servlet-env web-server/dispatch
;          web-server/dispatchers/dispatch web-server/dispatch/extend
;          web-server/http/bindings web-server/configuration/responders
;          web-server/managers/none)

; (require "../common.rkt" "../config.rkt" "../syntax/read.rkt" "../errors.rkt")
; (require "../syntax/syntax-check.rkt" "../syntax/type-check.rkt" "../syntax/types.rkt"
;          "../syntax/sugar.rkt" "../alternative.rkt" "../points.rkt"
;          "../programs.rkt" "../sandbox.rkt" "../float.rkt")
; (require "../datafile.rkt" "pages.rkt" "make-report.rkt"
;          "common.rkt" "core2mathjs.rkt" "history.rkt" "plot.rkt")
; (require (submod "../timeline.rkt" debug))

; (provide create-job start-job wait-for-job is-job-finished job-count start-job-server is-server-up compute-job-id get-finnished-jobs *demo-output* *demo?* (struct-out run-herbie-command))