#lang info

(define collection "herbie")
(define version "1.6")

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
  '(("base" #:version "8.0")
    "math-lib"
    "profile-lib"
    "rackunit-lib"
    "web-server-lib"
    ("rival" #:version "1.7")
    ("fpbench" #:version "2.0.3")))

(define build-deps
  '("rackunit-lib"))

(define test-omit-paths
  (if (getenv "PLT_PKG_BUILD_SERVICE")
      '("syntax/test-rules.rkt" ; These take too long, package server gives us 60s
        "sampling.rkt") ; These require the benchmarks
      '()))
