#lang info
(define collection "herbie")
(define compile-omit-paths '("test" "reports/bash-pred-test.rkt" "herbie-web" "util.rkt"))
(define raco-commands '(("herbie" (submod herbie/interface/inout main) "improve floating point expression accuracy" 75)))
(define deps '("base"
               "math-lib"
               "plot-lib"
               "profile-lib"
               "rackunit-lib"
               "srfi-lite-lib"
               "web-server-lib"))
