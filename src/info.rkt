#lang info

(define collection "herbie")
(define version "1.4")

;; Packaging information

(define pkg-desc "A tool for automatically improving the accuracy of floating point expressions")
(define pkg-authors
  '("Pavel Panchekha"
    "Alex Sanchez-Stern"
    "David Thien"
    "Jason Qiu"
    "James Wilcox"
    "Zachary Tatlock"
    "Jack Firth"))

;; The `herbie` command-line tool

(define racket-launcher-names '("herbie"))
(define racket-launcher-libraries '("herbie.rkt"))

;; Dependencies

(define deps
  '(("base" #:version "7.0")
    "math-lib"
    "plot-lib"
    "profile-lib"
    "rackunit-lib"
    "web-server-lib"
    ("egg-herbie-windows" #:platform "win32\\x86_64" #:version "1.4")
    ("egg-herbie-osx" #:platform "x86_64-macosx" #:version "1.4")
    ("egg-herbie-linux" #:platform "x86_64-linux" #:version "1.4")))

(define build-deps
  '("rackunit-lib"))

(define test-omit-paths
  (if (getenv "PLT_PKG_BUILD_SERVICE")
      '("syntax/test-rules.rkt" ; These take too long, package server gives us 60s
        "points.rkt") ; These require the benchmarks
      '()))
