#lang info

(define collection "herbie")

(define compile-omit-paths
  '("test" "web" "reports" "old"))

(define test-omit-paths
  '("test" "web" "reports" "old"))

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
