#lang info

(define collection "egg-herbie")
(define version "1.6")

(define pkg-desc "Racket bindings for simplifying math expressions using egg")
(define pkg-authors
  `("Oliver Flatt"))

(define build-deps
  '("rackunit-lib"))

(define deps '(("base" #:version "8.0")))
