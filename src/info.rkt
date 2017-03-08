#lang info

(define collection "herbie")
(define version "1.1")

(define pkg-desc "A tool for automatically improving the accuracy of floating point expressions")
(define pkg-authors
  '("Pavel Panchekha"
    "Alex Sanchez-Stern"
    "Jason Qiu"
    "James Wilcox"
    "Zachary Tatlock"
    "Jack Firth"))

(define compile-omit-paths '("test" "old"))
(define test-omit-paths '("test" "old"))

(define racket-launcher-names '("herbie"))

(define racket-launcher-libraries '("herbie.rkt"))

(define deps
  '(("base" #:version "6.3")
    "math-lib"
    "plot-lib"
    "profile-lib"
    "rackunit-lib"
    "web-server-lib"))

(define build-deps
  '("rackunit-lib"))
