#lang info

(define collection "herbie")

(define compile-omit-paths
  '("test"
    "reports/bash-pred-test.rkt"
    "herbie-web"
    "util.rkt"))

(define test-omit-paths
  '("compile/results-to-csv.rkt"
    "herbie-web"
    "reports/bash-pred-test.rkt"
    "reports/rerun.rkt"
    "reports/run.rkt"
    "reports/travis.rkt"
    "test/regimes-test.rkt"
    "test/rules-test.rkt"
    "util.rkt"))

(define raco-commands
  '(("herbie" (submod herbie/herbie main)
              "improve floating point expression accuracy" 75)))

(define deps
  '(("base" #:version "6.3")
    "math-lib"
    "plot-lib"
    "profile-lib"
    "rackunit-lib"
    "srfi-lite-lib"
    "web-server-lib"))

(define build-deps
  '("rackunit-lib"))
