#lang info

(define collection "egglog-herbie")
(define version "2.0")

(define pkg-desc "Racket bindings for simplifying math expressions using egglog")
(define pkg-authors `("Brett Saiki"))

(define build-deps '("rackunit-lib"))

(define deps '(("base" #:version "8.0")))

(define license 'MIT)
